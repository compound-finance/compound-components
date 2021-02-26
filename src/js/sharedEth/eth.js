import Eth from 'web3-eth';
import EthUtils from 'web3-utils';
import BN from 'bn.js';

import { at, fetchContract, getContractEvent } from './contract.js';
import {
  reverseObject,
  debug,
  isUserCancel,
  providerType,
  PROVIDER_TYPE_COINBASE_WALLET,
  PROVIDER_TYPE_IM_TOKEN,
} from './utils.js';
import { Ledger } from './ledger/ledger.js';

const actionSubmittedTypeMap = {
  mint: 0,
  redeem: 1,
  redeemUnderlying: 1,
  borrow: 2,
  repayBorrow: 3,
  repayBehalf: 3,
};

const DEFAULT_GAS_PRICE = 8000000000; // 8 Gwei

const TEMP_UPPER_GAS_LIMIT = 550000;
const ESTIMATED_GAS_MULTIPLIER = 1.25; // Multiplied with estimateGas and used as transaction gasLimits.

const COINBASE_WALLET_GAS_SUFFIX = 7350;
const IM_TOKEN_GAS_SUFFIX = 1896;
const DEFAULT_GAS_SUFFIX = 2020;

const COINBASE_WALLET_UPPER_GAS_LIMIT = TEMP_UPPER_GAS_LIMIT + COINBASE_WALLET_GAS_SUFFIX;
const IM_TOKEN_UPPER_GAS_LIMIT = TEMP_UPPER_GAS_LIMIT + IM_TOKEN_GAS_SUFFIX;
const DEFAULT_UPPER_GAS_LIMIT = TEMP_UPPER_GAS_LIMIT + DEFAULT_GAS_SUFFIX;

const defaultCallParams = {};
const defaultSendParams = {};

const callProviderPromises = [];

function getBlockNumberForCall(blockNumber) {
  if (blockNumber === 0 || !blockNumber) {
    return 'latest';
  } else {
    return blockNumber;
  }
}

function makeEth(dataProviders, networkMap, networkAbiMap, configNameToAddressMappings, defaultNetwork) {
  const targetAbiMap = networkAbiMap[defaultNetwork] || networkAbiMap['mainnet'];

  let networkAddressToNameMap = Object.entries(configNameToAddressMappings).reduce((acc, [key, value]) => {
    acc[key] = reverseObject(value);
    return acc;
  }, {});
  const targetAddressToNameMap = networkAddressToNameMap[defaultNetwork] || networkAddressToNameMap['mainnet'];

  return {
    trxEth: undefined,
    dataEth: undefined,
    dataProviders: dataProviders,
    readOnly: true,
    networkIdMap: reverseObject(networkMap),
    networkAbiMap: networkAbiMap,
    networkAddressToNameMap: networkAddressToNameMap,
    defaultNetworkId: networkMap[defaultNetwork],
    currentAbiMap: targetAbiMap,
    currentAddressToNameMap: targetAddressToNameMap,
    appProviderType: undefined,
  };
}

export async function giveNewTrx(
  app,
  eth,
  contractAddress,
  assetAddress,
  customerAddress,
  result,
  funcName,
  args,
  currentTrxCount
) {
  const networkId = parseInt(await getNetworkId(eth));
  const computedNonce = currentTrxCount < 0 ? null : currentTrxCount + 1;

  await app.ports.giveNewTrxPort.send({
    trxHash: result,
    network: networkId,
    timestamp: +new Date(), // current epoch
    contract: contractAddress,
    asset: assetAddress,
    customer: customerAddress,
    function: funcName,
    args: args.map((a) => a.toString()),
    expectedNonce: computedNonce,
  });
}

async function wrapCallErr(app, eth, calls, errorHandler, blockNumber = 'latest') {
  try {
    return await wrapCall(app, eth, calls, blockNumber);
  } catch (e) {
    await errorHandler(e);
  }
}

async function callWeb3(web3, contractJson, address, method, args) {
  let funcABI = contractJson.abi.find(({ name, inputs }) => name == method && inputs.length === args.length);
  if (!funcABI) {
    throw new Error(`Cannot find ABI function ${name} in ${contractJson.name}`);
  }

  let callResult = await web3.call({
    to: address,
    data: web3.abi.encodeFunctionCall(funcABI, args),
  });

  try {
    return web3.abi.decodeParameters(funcABI.outputs, callResult)[0];
  } catch (e) {
    console.error(['Error decoding call result', callResult]);
    throw e;
  }
}

function wrapCall(app, eth, calls, blockNumber = 'latest') {
  // Setup each call as a promise
  return withWeb3Eth(eth).then((web3Eth) => {
    return Promise.all(
      calls.map(async ([contractJson, address, method, args]) => {
        let host = web3Eth.currentProvider.host;
        let provider = web3Eth.currentProvider;

        try {
          return await callWeb3(web3Eth, contractJson, address, method, args);
        } catch (e) {
          console.error(`Error calling web3`, { address, method, args, host });
          console.error(e);
          throw e;
        }
      })
    );
  });
}

function getLogs(app, eth, address, topics, fromBlock, toBlock = 'latest') {
  return withWeb3Eth(eth).then((web3Eth) => {
    return web3Eth.getPastLogs({
      fromBlock,
      toBlock,
      address,
      topics,
    });
  });
}

function wrapSend(
  app,
  eth,
  contractJson,
  contractAddress,
  funcName,
  args,
  assetAddress,
  customerAddress,
  currentGasPrice,
  opts = {}
) {
  return withTrxWeb3(
    eth,
    (trxEth, trxPromise) => {
      return withGasLimitAndTrxCount(
        app,
        eth,
        contractJson,
        contractAddress,
        funcName,
        args,
        customerAddress,
        opts
      ).then(([estimatedGasLimit, currentTrxCount]) => {
        return new Promise((resolve, reject) => {
          const contract = fetchContract(eth.trxEth, contractJson, contractAddress);

          const defaultParamsClone = Object.assign({}, defaultSendParams);
          let sendParams = Object.assign(defaultParamsClone, {
            from: customerAddress,
            gasLimit: estimatedGasLimit,
          });

          if (currentGasPrice !== undefined) {
            sendParams.gasPrice = currentGasPrice;
          } else {
            sendParams.gasPrice = DEFAULT_GAS_PRICE;
          }

          if (opts.value !== undefined) {
            sendParams.value = opts.value;
          }

          const sendArgs = args.map((arg) => {
            if (arg instanceof BN) {
              return arg.toString();
            } else {
              return arg;
            }
          });
          const promiEvent = contract.methods[funcName](...sendArgs).send(sendParams);

          promiEvent.on('error', (error) => {
            if (isUserCancel(error)) {
              // Swallow these errors, since they aren't really errors;
              resolve(null);
            } else {
              reject(error);
            }
          });

          promiEvent.on('transactionHash', (trxHash) => {
            if (trxHash) {
              const displayName = opts.displayName || funcName;
              const displayArgs = opts.displayArgs || args;

              giveNewTrx(
                app,
                eth,
                contractAddress,
                assetAddress,
                customerAddress,
                trxHash,
                displayName,
                displayArgs,
                currentTrxCount
              );

              resolve(trxHash);
            }
          });
        });
      });
    },
    () => {
      throw 'Cannot send transaction without transactional Web3';
    }
  );
}

function withGasLimitAndTrxCount(app, eth, contractJson, contractAddress, funcName, args, customerAddress, opts = {}) {
  return withWeb3Eth(eth).then((web3Eth) => {
    const contract = fetchContract(web3Eth, contractJson, contractAddress);

    const defaultParamsClone = Object.assign({}, defaultSendParams);
    let sendParams = Object.assign(defaultParamsClone, {
      from: customerAddress,
    });

    if (opts.value !== undefined) {
      sendParams.value = opts.value;
    }

    const sendArgs = args.map((arg) => {
      if (arg instanceof BN) {
        return arg.toString();
      } else {
        return arg;
      }
    });

    return Promise.all([
      contract.methods[funcName](...sendArgs)
        .estimateGas(sendParams)
        .then((estimatedGas) => {
          const roundedGasWithBuffer = Math.round(estimatedGas * ESTIMATED_GAS_MULTIPLIER);

          // Some integrations require the gas to end with specific values.
          const maskedGasLimit = roundedGasWithBuffer - (roundedGasWithBuffer % 10000);
          let actualGasLimit;
          switch (eth.appProviderType) {
            case PROVIDER_TYPE_COINBASE_WALLET:
              actualGasLimit = maskedGasLimit + COINBASE_WALLET_GAS_SUFFIX;
              break;
            case PROVIDER_TYPE_IM_TOKEN:
              actualGasLimit = maskedGasLimit + IM_TOKEN_GAS_SUFFIX;
              break;
            default:
              actualGasLimit = maskedGasLimit + DEFAULT_GAS_SUFFIX;
              break;
          }

          return Promise.resolve(actualGasLimit);
        })
        .catch((error) => {
          let upperGasLimit;
          switch (eth.appProviderType) {
            case PROVIDER_TYPE_COINBASE_WALLET:
              upperGasLimit = COINBASE_WALLET_UPPER_GAS_LIMIT;
              break;
            case PROVIDER_TYPE_IM_TOKEN:
              upperGasLimit = IM_TOKEN_UPPER_GAS_LIMIT;
              break;
            default:
              upperGasLimit = DEFAULT_UPPER_GAS_LIMIT;
              break;
          }

          return Promise.resolve(upperGasLimit);
        }),
      web3Eth
        .getTransactionCount(customerAddress)
        .then((transactionCount) => {
          return Promise.resolve(transactionCount);
        })
        .catch((error) => {
          return Promise.resolve(-1);
        }),
    ]);
  });
}

function withGasLimitFromPayload(web3Eth, trxPayload) {
  return web3Eth
    .estimateGas(trxPayload)
    .then((estimatedGas) => {
      const roundedGasWithBuffer = Math.round(estimatedGas * ESTIMATED_GAS_MULTIPLIER);

      // Some integrations require the gas to end with specific values.
      const maybeMasked = roundedGasWithBuffer - (roundedGasWithBuffer % 10000);
      const maskedGasLimit = maybeMasked < estimatedGas ? maybeMasked + 10000 : maybeMasked;
      let actualGasLimit;
      switch (web3Eth.appProviderType) {
        case PROVIDER_TYPE_COINBASE_WALLET:
          actualGasLimit = maskedGasLimit + COINBASE_WALLET_GAS_SUFFIX;
          break;
        case PROVIDER_TYPE_IM_TOKEN:
          actualGasLimit = maskedGasLimit + IM_TOKEN_GAS_SUFFIX;
          break;
        default:
          actualGasLimit = maskedGasLimit + DEFAULT_GAS_SUFFIX;
          break;
      }
      return Promise.resolve(actualGasLimit);
    })
    .catch((error) => {
      let upperGasLimit;
      switch (web3Eth.appProviderType) {
        case PROVIDER_TYPE_COINBASE_WALLET:
          upperGasLimit = COINBASE_WALLET_UPPER_GAS_LIMIT;
          break;
        case PROVIDER_TYPE_IM_TOKEN:
          upperGasLimit = IM_TOKEN_UPPER_GAS_LIMIT;
          break;
        default:
          upperGasLimit = DEFAULT_UPPER_GAS_LIMIT;
          break;
      }

      return Promise.resolve(upperGasLimit);
    });
}

function withWeb3Eth(eth) {
  if (eth.dataEth) {
    return Promise.resolve(eth.dataEth);
  } else {
    return new Promise((resolve, reject) => {
      callProviderPromises.push(resolve);
    });
  }
}

function withTrxWeb3(eth, fnTrxWeb3, fnEls) {
  if (eth.trxEth) {
    let res = fnTrxWeb3(eth.trxEth, eth.trxEth.trxPromise);
    eth.trxEth.trxPromise = res;
    return res;
  } else {
    return fnEls();
  }
}

async function getLedgerAddressAndBalance(eth, derivationPath, useRopsen = false) {
  const desiredProvider = useRopsen ? 'ropsten' : 'mainnet';

  let mainnetEth = new Eth(eth.dataProviders[desiredProvider]);
  mainnetEth.type = 'call';

  let networkMap = reverseObject(eth.networkIdMap);
  let ledger = await Ledger.connect(mainnetEth.currentProvider, {
    networkId: networkMap[desiredProvider],
    path: derivationPath,
  });
  let accountAddress = null;
  let ethBalanceWei = null;
  if (ledger != null) {
    accountAddress = await ledger.getAddress().catch((error) => console.log('caught ledger error: %o', error));
    if (accountAddress != null) {
      ethBalanceWei = await mainnetEth.getBalance(accountAddress);
    }
  }
  return {
    accountAddress,
    ethBalanceWei,
  };
}

async function setLedgerProvider(eth, networkId, ledgerDerivationPath) {
  setNetworkId(eth, networkId);
  const web3Eth = await withWeb3Eth(eth);
  let ledger = await Ledger.connect(web3Eth.currentProvider, {
    networkId: networkId,
    path: ledgerDerivationPath,
  });
  let trxProvider = ledger.getProvider();

  setNewTrxProvider(eth, trxProvider);
}

function setNewTrxProvider(eth, newTrxProvider, showAccount, showNetwork) {
  if (newTrxProvider != null) {
    if (newTrxProvider.setMaxListeners) {
      newTrxProvider.setMaxListeners(30);
    }

    eth.trxEth = new Eth(newTrxProvider);
    eth.trxEth.type = 'send';
    eth.trxEth.trxPromise = Promise.resolve(null);
  } else {
    eth.trxEth = null;
  }

  // Set a show acocunt if given
  if (showAccount) {
    eth.showAccount = showAccount;
    eth.defaultNetworkId = showNetwork;
  }

  eth.appProviderType = providerType(newTrxProvider);
}

function setNetworkId(eth, networkId) {
  const targetNetworkName = eth.networkIdMap[networkId];
  if (targetNetworkName && eth.dataProviders[targetNetworkName]) {
    // TODO: This?
    eth.dataEth = new Eth(eth.dataProviders[targetNetworkName]);
    eth.dataEth.type = 'call';

    for (let promise of callProviderPromises) {
      // Resolve each promise that was enqueued
      promise(eth.dataEth);
    }

    eth.currentAbiMap = eth.networkAbiMap[targetNetworkName];
    eth.currentAddressToNameMap = eth.networkAddressToNameMap[targetNetworkName];
  } else {
    // TODO: This?
    eth.dataEth = undefined;
    eth.currentAbiMap = eth.networkAbiMap['mainnet'];
    eth.currentAddressToNameMap = eth.networkAddressToNameMap['mainnet'];
  }
}

async function getNetworkId(eth) {
  return withTrxWeb3(
    eth,
    (trxEth) => trxEth.net.getId(),
    () => eth.defaultNetworkId
  );
}

async function getBalance(eth, address) {
  const web3Eth = await withWeb3Eth(eth);

  return web3Eth.getBalance(address);
}

async function getBlockNumber(eth) {
  const web3Eth = await withWeb3Eth(eth);

  return web3Eth.getBlockNumber();
}

async function getAccounts(eth) {
  return withTrxWeb3(
    eth,
    (trxEth) => trxEth.getAccounts(),
    () => (eth.showAccount ? [eth.showAccount] : [])
  );
}

async function getTransaction(eth, trxHash) {
  const web3Eth = await withWeb3Eth(eth);

  return web3Eth.getTransaction(trxHash);
}

async function getTransactionCount(eth, address) {
  const web3Eth = await withWeb3Eth(eth);

  return web3Eth.getTransactionCount(address);
}

async function getTransactionReceipt(eth, trxHash) {
  const web3Eth = await withWeb3Eth(eth);

  return web3Eth.getTransactionReceipt(trxHash);
}

async function sign(eth, message, address) {
  return withTrxWeb3(
    eth,
    (trxEth) => trxEth.personal.sign(EthUtils.fromUtf8(message), address),
    () => {
      throw 'Cannot sign message without transactional Web3';
    }
  );
}

function getEvent(eth, contractJson, eventName) {
  // Note: We don't need this to be initialized with a provider
  return getContractEvent(new Eth(), contractJson, eventName);
}

export {
  getAccounts,
  getBalance,
  getBlockNumber,
  getEvent,
  getLogs,
  getLedgerAddressAndBalance,
  getNetworkId,
  getTransaction,
  getTransactionCount,
  getTransactionReceipt,
  makeEth,
  setNetworkId,
  setNewTrxProvider,
  setLedgerProvider,
  sign,
  withWeb3Eth,
  withTrxWeb3,
  withGasLimitFromPayload,
  wrapCall,
  wrapCallErr,
  wrapSend,
};
