import Eth from '../../../node_modules/web3-eth';
import { Elm } from '../elm/Main.elm';
import { providerType, langFromURL } from '../../../src/js/sharedEth/utils';
import connectedWalletPorts from '../../../src/js/sharedEth/connectedWalletPorts';
import { makeEth, setNewTrxProvider } from '../../../src/js/sharedEth/eth';
import sharedJsPorts from '../../../src/js/sharedJs/sharedJsPorts';

window.addEventListener('load', function () {
  let globEthereum = null;
  if (window['ethereum']) {
    globEthereum = window['ethereum'];
  }

  const url = new URL(window.location);

  let buildHref = (href) => href;
  let stripHref = (href) => href;
  let mountPath = window.BUILD_MOUNT_PATH || process.env.MOUNT_PATH || '';
  if (mountPath) {
    let mountRegex = new RegExp(`^/${mountPath}`);

    stripHref = (href) => {
      let url = new URL(href);
      url.pathname = url.pathname.replace(mountRegex, '');
      return url.href;
    };

    buildHref = (href) => {
      let url = new URL(href);
      url.pathname = mountPath + url.pathname;
      return url.href;
    };
  }

  const app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: {
      language: langFromURL(url, window.navigator.language),
      path: stripHref(window.location.href),
      providerType: providerType(globEthereum),
      userAgent: navigator.userAgent,
    },
  });

  function findParent(tagname, el) {
    if (!el) {
      return null;
    }

    if ((el.nodeName || el.tagName).toLowerCase() === tagname.toLowerCase()) {
      return el;
    }

    return findParent(tagname, el.parentNode);
  }

  document.addEventListener('click', (e) => {
    e = e || event;
    var from = findParent('a', e.target || e.srcElement);

    if (from && from.href && !from.rel) {
      history.pushState({}, '', buildHref(from.href));
      e.preventDefault();
      app.ports.onUrlChange.send(from.href);
    }
  });

  // Inform app of browser navigation (the BACK and FORWARD buttons)
  window.addEventListener('popstate', () => {
    app.ports.onUrlChange.send(location.href);
  });

  const DATA_PROVIDERS = {
    development: 'http://localhost:8545',
    goerli: 'https://goerli-eth.compound.finance',
    rinkeby: 'https://rinkeby-eth.compound.finance',
    kovan: 'https://kovan-eth.compound.finance',
    ropsten: 'https://ropsten-eth.compound.finance',
    mainnet: 'https://mainnet-eth.compound.finance',
  };

  const NETWORK_MAP = {
    mainnet: 1,
    ropsten: 3,
    rinkeby: 4,
    goerli: 5,
    kovan: 42,
    development: 999,
  };

  const dataProviders = Object.entries(DATA_PROVIDERS).reduce((acc, [network, url]) => {
    return Object.assign(acc, { [network]: new Eth.providers.HttpProvider(url) });
  }, {});

  const eth = makeEth(dataProviders, NETWORK_MAP, {}, {}, 'kovan');
  connectedWalletPorts.subscribe(app, eth, globEthereum, NETWORK_MAP, 'kovan');
  setNewTrxProvider(eth, globEthereum);
});
