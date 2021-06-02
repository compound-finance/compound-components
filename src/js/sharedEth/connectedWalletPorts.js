import { shouldAutoConnect } from './utils';
import { getAccounts, getLedgerAddressAndBalance, getNetworkId, makeEth, setNetworkId } from './eth';
import {
  connectLedger,
  connectWalletLink,
  connectWeb3,
  connectShowAccount,
  connectWalletConnect,
  disconnect,
} from './connectors';
import { toScaledDecimal } from '../sharedJs/math';
import storage from '../sharedJs/storage';

const ETH_DECIMALS = 18;

const PROVIDER_TYPE_NONE = 0;
const PROVIDER_TYPE_LEDGER = 1;
const PROVIDER_TYPE_WALLET_LINK = 2;
const PROVIDER_TYPE_WEB3 = 3;
const PROVIDER_TYPE_SHOW_ACCOUNT = 3;
const PROVIDER_TYPE_WALLET_CONNECT = 4;

function subscribeToAccountChanges(app, ethereum) {
  if (ethereum && typeof ethereum.on !== 'undefined' && !ethereum.accountsChangedSet) {
    ethereum.on('accountsChanged', function ([account, _]) {
      const networkId = ethereum.chainId;
      app.ports.giveNetworkPort.send({ network: parseInt(networkId) });
      app.ports.giveAccountWeb3Port.send({ account: account });
    });

    ethereum.accountsChangedSet = true;
  }
}

function subscribeToNetworkChanges(app, eth, ethereum) {
  if (ethereum && typeof ethereum.on !== 'undefined' && !ethereum.networkChangedSet) {
    // If we're in MetaMask, tell it not to refresh on network change
    ethereum.autoRefreshOnNetworkChange = false;

    ethereum.on('chainChanged', function (networkId) {
      const networkIdInt = parseInt(networkId);
      setNetworkId(eth, networkIdInt);
      app.ports.giveNetworkPort.send({ network: networkIdInt });

      getAccounts(eth).then(([account]) => {
        app.ports.giveAccountWeb3Port.send({ account: account });
      });
    });

    ethereum.networkChangedSet = true;
  }
}

async function connectToTrxProvider(
  app,
  eth,
  globEthereum,
  newProviderType,
  ledgerDerivationPath,
  disallowAuthDialog = false,
  showAccount = undefined,
  desiredNetworkId = 1
) {
  // We support 4 real provider types
  // 1. Ledger
  // 2. WalletLink
  // 3. Provided Web3
  // 4. WalletConnect

  let networkId, account, ethereum;
  let establishWithoutAccount = false;

  switch (newProviderType) {
    case PROVIDER_TYPE_LEDGER:
      ({ networkId, account, ethereum } = await connectLedger(
        eth,
        ledgerDerivationPath,
        disallowAuthDialog,
        desiredNetworkId
      ));
      break;
    case PROVIDER_TYPE_WALLET_LINK:
      ({ networkId, account, ethereum } = await connectWalletLink(eth, disallowAuthDialog));
      break;
    case PROVIDER_TYPE_WEB3:
      ({ networkId, account, ethereum } = await connectWeb3(eth, globEthereum, disallowAuthDialog));
      break;
    case PROVIDER_TYPE_SHOW_ACCOUNT:
      ({ networkId, account, ethereum } = await connectShowAccount(eth, showAccount));
      break;
    case PROVIDER_TYPE_WALLET_CONNECT:
      ({ networkId, account, ethereum } = await connectWalletConnect(eth, showAccount, desiredNetworkId));
      break;
    default:
      ({ networkId, account, ethereum } = await disconnect(eth));
      establishWithoutAccount = true;
      break;
  }

  if (establishWithoutAccount || !!account) {
    // If we didn't prompt an auth dialog, don't store this as a user choice
    if (!disallowAuthDialog) {
      storage('chosenProvider').set(newProviderType);
    }

    establishConnection(app, eth, networkId, account, ethereum, newProviderType);

    return true;
  } else {
    return false;
  }
}

function subscribeToTrxProviderChanges(app, eth, globEthereum) {
  // port changeTrxProviderType : { newProviderType: Int, ledgerDerivationPath: String, ledgerWalletConnectRopsten : Bool } -> Cmd msg
  app.ports.changeTrxProviderType.subscribe(
    async ({ newProviderType, ledgerDerivationPath, ledgerWalletConnectRopsten }) => {
      subscribeToAccountChanges(app, globEthereum);
      subscribeToNetworkChanges(app, eth, globEthereum);

      const desiredNetworkId = ledgerWalletConnectRopsten ? 3 : 1;
      await connectToTrxProvider(
        app,
        eth,
        globEthereum,
        newProviderType,
        ledgerDerivationPath,
        false,
        false,
        desiredNetworkId
      );
    }
  );
}

function subscribeToTryConnect(app, eth, globEthereum, defaultNetworkId) {
  let urlParams = new URLSearchParams(document.location.search.substring(1));
  if (urlParams.get('account')) {
    return showAccount(app, eth, urlParams.get('account'));
  }
  app.ports.tryConnect.subscribe(async (showProvider) => {
    if (shouldAutoConnect(globEthereum)) {
      // We have something we think is a Web3-only browser, e.g. imToken,
      // so let's just force a connection.
      let { networkId, account, ethereum } = await connectWeb3(eth, globEthereum, false, true);
      await establishConnection(app, eth, networkId, account, ethereum, PROVIDER_TYPE_WEB3);
    } else {
      // If we are not in a Web3-only browser, then we'll see if there's
      // a safe network we can connect to (e.g. Coinbase Mobile or MetaMask).

      // If it's already been authorized, we'll connect, otherwise, we'll
      // sit back and pop a modal.

      // We'll try to set to the user's last chosen provider, otherwise
      // defaulting to Web3.
      let providerType = Number(storage('chosenProvider').get(PROVIDER_TYPE_WEB3));
      let connected = await connectToTrxProvider(app, eth, globEthereum, providerType, '', true);

      if (!connected) {
        // Otherwise, let's connect to mainnet to show numbers
        establishConnection(app, eth, 1, null, null, PROVIDER_TYPE_NONE);

        if (!storage('skipConnectModal').get(false) && showProvider) {
          // Popup dialog
          app.ports.chooseProvider.send(true);
        }
      }
    }
  });

  // port retrieveLedgerAccounts : { derivationPaths : List String, ledgerConnectRopsten : Bool } -> Cmd msg
  app.ports.retrieveLedgerAccounts.subscribe(async ({ derivationPaths, ledgerConnectRopsten }) => {
    for (let i = 0; i < derivationPaths.length; i++) {
      const path = derivationPaths[i];
      let { accountAddress, ethBalanceWei } = await getLedgerAddressAndBalance(eth, path, ledgerConnectRopsten);

      let ethBalance = null;
      if (ethBalanceWei != null) {
        ethBalance = toScaledDecimal(ethBalanceWei, ETH_DECIMALS);
      }

      app.ports.giveLedgerAccount.send({
        derivationPath: path,
        account: accountAddress,
        ethBalance: ethBalance,
      });

      if (i == derivationPaths.length - 1) {
        app.ports.giveRetrievedAllLedgerAccounts.send(true);
      } else if (accountAddress == null && ethBalance == null) {
        //Ledger needs user interaction. Finish early
        app.ports.giveRetrievedAllLedgerAccounts.send(false);
        break;
      }
    }
  });
}

async function establishConnection(app, eth, networkId, account, ethereum, providerType) {
  app.ports.giveNetworkPort.send({ network: networkId });
  app.ports.giveAccountWeb3Port.send({ account });
  app.ports.giveTrxProviderTypePort.send({ providerType });
  await setNetworkId(eth, networkId);

  subscribeToAccountChanges(app, ethereum);
  subscribeToNetworkChanges(app, eth, ethereum);
}

async function showAccount(app, eth, showAccount) {
  let { networkId, account, ethereum } = await connectShowAccount(eth, showAccount);
  await establishConnection(app, eth, networkId, account, ethereum, PROVIDER_TYPE_SHOW_ACCOUNT);
}

function subscribe(app, eth, globEthereum, networkMap, defaultNetwork) {
  subscribeToTrxProviderChanges(app, eth, globEthereum);
  subscribeToTryConnect(app, eth, globEthereum, networkMap[defaultNetwork]);
}

export default {
  subscribe,
  showAccount,
};
