import Eth from 'web3-eth';
import { getAccounts, getNetworkId, setLedgerProvider, setNewTrxProvider } from './eth';
import WalletLink from 'walletlink';
import { EthereumProvider } from '@walletconnect/ethereum-provider';

async function connectLedger(eth, ledgerDerivationPath, disallowAuthDialog = false, desiredNetworkId = 1) {
  // Never auto-connect to ledger, since it's complicated
  if (disallowAuthDialog) {
    return {
      networkId: null,
      account: null,
      ethereum: null,
    };
  }

  await setLedgerProvider(eth, desiredNetworkId, ledgerDerivationPath);
  let [account, _] = await getAccounts(eth);

  return {
    networkId: desiredNetworkId,
    account: account,
    ethereum: null,
  };
}

async function connectWalletLink(eth, disallowAuthDialog = false) {
  const JSONRPC_URL = eth.dataProviders['mainnet'].host;
  const CHAIN_ID = 1;

  const walletLink = new WalletLink({
    appName: 'Compound',
    appLogoUrl: 'https://app.compound.finance/images/compound-192.png',
  });

  const trxProvider = walletLink.makeWeb3Provider(JSONRPC_URL, CHAIN_ID);

  if (disallowAuthDialog && (await requiresAuthDialog(trxProvider))) {
    return {
      networkId: null,
      account: null,
      ethereum: null,
    };
  }

  setNewTrxProvider(eth, trxProvider);

  let networkIdStr = await getNetworkId(eth);
  let networkId = parseInt(networkIdStr);
  if (networkId === NaN) {
    networkId = null;
  }

  //TODO: This is going to change in the future with EIP-1193
  // This method actually triggers the UI flow from as spec'd in EIP-1102
  await trxProvider.send('eth_requestAccounts').then((accounts) => {
    //Currently don't need accounts here as we synchronous get next.
  });

  let [account, _] = await getAccounts(eth);

  return {
    networkId,
    account,
    ethereum: trxProvider,
  };
}

async function requiresAuthDialog(ethereum) {
  try {
    let [account, _] = await new Eth(ethereum).getAccounts();
    return !account;
  } catch (e) {
    console.log(e);
    return true;
  }
}

async function connectWeb3(eth, ethereum, disallowAuthDialog = false, isAutoConnect = false) {
  if (ethereum && !ethereum.isTally) {
    return await connectWeb3Helper(eth, ethereum, disallowAuthDialog, isAutoConnect);
  } else {
    return {
      networkId: null,
      account: null,
      ethereum: null,
    };
  }
}

async function connectTally(eth, ethereum, disallowAuthDialog = false, isAutoConnect = false) {
  if (ethereum && ethereum.isTally) {
    return await connectWeb3Helper(eth, ethereum, disallowAuthDialog, isAutoConnect);
  } else {
    return {
      networkId: null,
      account: null,
      ethereum: null,
    };
  }
}

async function connectWeb3Helper(eth, ethereum, disallowAuthDialog = false, isAutoConnect = false) {
  let trxProvider = ethereum;

  if (disallowAuthDialog && (await requiresAuthDialog(ethereum))) {
    return {
      networkId: null,
      account: null,
      ethereum: null,
    };
  }

  //TODO: This is going to change in the future with EIP-1193
  if (!isAutoConnect) {
    ethereum.request({ method: 'eth_requestAccounts' });
  }

  setNewTrxProvider(eth, trxProvider);

  let [account, _] = await getAccounts(eth);

  let networkIdStr = await getNetworkId(eth);
  let networkId = parseInt(networkIdStr);
  if (networkId === NaN) {
    networkId = null;
  }

  return { networkId, account, ethereum };
}

async function connectShowAccount(eth, showAccount) {
  setNewTrxProvider(eth, null, showAccount, 1);

  let [account, _] = await getAccounts(eth);
  let networkIdStr = await getNetworkId(eth);
  let networkId = parseInt(networkIdStr);
  if (networkId === NaN) {
    networkId = null;
  }

  return {
    networkId: networkId,
    account: account,
    ethereum: null,
  };
}

async function connectWalletConnect(eth, disallowAuthDialog = false, desiredNetworkId = 1, walletConnectProjectId) {
  const ethProviderName = desiredNetworkId == 3 ? 'ropsten' : 'mainnet';
  const JSONRPC_URL = eth.dataProviders[ethProviderName].host;
  const CHAIN_ID = desiredNetworkId;

  const trxProvider = await EthereumProvider.init({
    projectId: walletConnectProjectId,
    chains: [1],
    showQrCode: true,
  });

  try {
    // Open the walletconnect modal
    await trxProvider.enable();
  } catch (e) {
    // If the error is not just from the user closing the modal, we log it for debugging in the future
    if (e.message !== 'User closed modal') {
      console.log(e);
    }
  }

  if (disallowAuthDialog && (await requiresAuthDialog(trxProvider))) {
    return {
      networkId: null,
      account: null,
      ethereum: null,
    };
  }

  setNewTrxProvider(eth, trxProvider);

  let networkIdStr = await getNetworkId(eth);
  let networkId = parseInt(networkIdStr);
  if (networkId === NaN) {
    networkId = null;
  }

  // This method actually triggers the UI flow from as spec'd in EIP-1102
  await trxProvider.request({ method: 'eth_accounts' }).then((accounts) => {
    //Currently don't need accounts here as we synchronous get next.
  });

  let [account, _] = await getAccounts(eth);

  return {
    networkId,
    account,
    ethereum: trxProvider,
  };
}

async function disconnect(eth) {
  setNewTrxProvider(eth, null);

  return [null, null, null];
}

export {
  connectLedger,
  connectWalletLink,
  connectWeb3,
  connectTally,
  connectShowAccount,
  connectWalletConnect,
  disconnect,
};
