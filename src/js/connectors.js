import Eth from '../../node_modules/web3-eth';
import {
  getAccounts,
  getNetworkId,
  setLedgerProvider,
  setNewTrxProvider
} from './eth';
import WalletLink from 'walletlink';

async function connectLedger(eth, ledgerDerivationPath, disallowAuthDialog=false) {
  // Never auto-connect to ledger, since it's complicated
  if (disallowAuthDialog) {
    return {
      networkId: null,
      account: null,
      ethereum: null
    };
  }

  const networkId = 1;
  await setLedgerProvider(eth, networkId, ledgerDerivationPath);
  let [account, _] = await getAccounts(eth);

  return {
    networkId,
    account,
    ethereum: null
  };
}

async function connectWalletLink(eth, disallowAuthDialog=false) {
  const JSONRPC_URL = eth.dataProviders['mainnet'].host;
  const CHAIN_ID = 1;

  const walletLink = new WalletLink({
    appName: "Compound",
    appLogoUrl: "https://app.compound.finance/images/compound-192.png"
  });

  const trxProvider = walletLink.makeWeb3Provider(JSONRPC_URL, CHAIN_ID);

  if (disallowAuthDialog && await requiresAuthDialog(trxProvider)) {
    return {
      networkId: null,
      account: null,
      ethereum: null
    };
  }

  setNewTrxProvider(eth, trxProvider);

  let networkIdStr = await getNetworkId(eth);
  let networkId = parseInt(networkIdStr);
  if (networkId === NaN) {
    networkId = null;
  }

  // This method actually triggers the UI flow from as spec'd in EIP-1102
  await trxProvider.send("eth_requestAccounts").then((accounts) => {
    //Currently don't need accounts here as we synchronous get next.
  });

  let [account, _] = await getAccounts(eth);

  return {
    networkId,
    account,
    ethereum: trxProvider
  };
}

async function requiresAuthDialog(ethereum) {
  let [account, _] = await new Eth(ethereum).getAccounts();

  return !account;
}

async function connectWeb3(eth, ethereum, disallowAuthDialog=false, isAutoConnect=false) {
  if (ethereum) {
    let trxProvider = ethereum;

    if (disallowAuthDialog && await requiresAuthDialog(ethereum)) {
      return {
        networkId: null,
        account: null,
        ethereum: null
      };
    }

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

    return {networkId, account, ethereum};
  } else {
    return {
      networkId: null,
      account: null,
      ethereum: null
    };
  }
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

async function disconnect(eth) {
  setNewTrxProvider(eth, null);

  return [null, null, null];
}

export {
  connectLedger,
  connectWalletLink,
  connectWeb3,
  connectShowAccount,
  disconnect,
};
