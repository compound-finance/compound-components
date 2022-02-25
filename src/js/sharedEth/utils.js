const PROVIDER_TYPE_NONE = 'none';
const PROVIDER_TYPE_COINBASE_WALLET = 'coinbase_mobile';
const PROVIDER_TYPE_IM_TOKEN = 'im_token';
const PROVIDER_TYPE_TALLY = 'tally';
const PROVIDER_TYPE_META_MASK = 'meta_mask';
const PROVIDER_TYPE_META_MASK_MOBILE = 'meta_mask_mobile';
const PROVIDER_TYPE_OTHER = 'other';

const NON_AUTOCONNECT_PROVIDERS = [PROVIDER_TYPE_NONE, PROVIDER_TYPE_META_MASK];

function reverseObject(obj) {
  return Object.keys(obj).reduce((acc, key) => {
    const val = obj[key];

    return {
      ...acc,
      [val]: key,
    };
  }, {});
}

function debug(...args) {
  if (process.env.NODE_ENV !== 'production') {
    console.log(...args);
  }
}

// This error message is returned when a user hits "cancel" on MetaMask
function isUserCancel(error) {
  const errorMessage = error.message.toString();
  return (
    errorMessage.startsWith('Returned error: Error: MetaMask Tx Signature: User denied') ||
    errorMessage.startsWith('Returned error: Error: MetaMask Message Signature: User denied')
  );
}

function log(...msg) {
  let el = document.createElement('span');
  let text = document.createTextNode(JSON.stringify(msg));
  el.appendChild(text);
  document.body.appendChild(el);
}

function providerType(provider) {
  if (provider === undefined || provider == null) {
    return PROVIDER_TYPE_NONE;
  } else if (provider.isToshi) {
    return PROVIDER_TYPE_COINBASE_WALLET;
  } else if (provider.isImToken) {
    return PROVIDER_TYPE_IM_TOKEN;
  }else if (provider.isTally) {
    return PROVIDER_TYPE_TALLY;
  } else if (provider.isMetaMask) {
    if (provider.constructor.name === 'InpageBridge') {
      return PROVIDER_TYPE_META_MASK_MOBILE;
    } else {
      return PROVIDER_TYPE_META_MASK;
    }
  } else {
    return null;
  }
}

function networkFromId(id) {
  switch (id) {
    case 0:
      return 'olympic';

    case 1:
      return 'mainnet';

    case 2:
      return 'morden';

    case 3:
      return 'ropsten';

    case 4:
      return 'rinkeby';

    case 5:
      return 'goerli';

    case 8:
      return 'ubiq';

    case 42:
      return 'kovan';

    case 77:
      return 'sokol';

    case 99:
      return 'core';

    case 999:
      return 'development';

    default:
      'development';
  }
}

function shouldAutoConnect(provider) {
  return !NON_AUTOCONNECT_PROVIDERS.includes(providerType(provider));
}

function supportFromEntries(keyValueList) {
  if (Object.hasOwnProperty('fromEntries')) {
    return Object.fromEntries(keyValueList);
  } else {
    return keyValueList.reduce((o, [key, value]) => {
      o[key] = value;
      return o;
    }, {});
  }
}

//TODO: Put into sharedJS i18n Utils...
function langFromURL(url, navigatorLanguage) {
  // Most locales are of the form: "en_US", "es_MX", "fr_FR", etc...
  // However some have language subdialect specifiers like: "zh-Hant_US", "zh-Hant-HK", etc...
  const paramLocale = url.searchParams.get('locale');
  const localeMatch = /^[a-z]{2}.*[_][A-Z]{2}/.exec(paramLocale);
  const secondaryLocaleMatch = /^[a-z]{2}.*[-][A-Z]{2}/.exec(paramLocale); // Some people (imToken) use the other form of locale :(

  let localeLangOnly = null;
  if (localeMatch) {
    localeLangOnly = localeMatch[0].substring(0, 2);
  } else if (secondaryLocaleMatch) {
    localeLangOnly = secondaryLocaleMatch[0].substring(0, 2);
  }

  const domainMatch = /^(es|en|zh)[-.]/.exec(url.host);
  const domainLanguage = domainMatch ? domainMatch[1] : null;
  return localeLangOnly || domainLanguage || navigatorLanguage || 'en';
}

export {
  reverseObject,
  debug,
  isUserCancel,
  log,
  networkFromId,
  providerType,
  PROVIDER_TYPE_NONE,
  PROVIDER_TYPE_COINBASE_WALLET,
  PROVIDER_TYPE_IM_TOKEN,
  PROVIDER_TYPE_TALLY,
  PROVIDER_TYPE_META_MASK,
  PROVIDER_TYPE_META_MASK_MOBILE,
  shouldAutoConnect,
  supportFromEntries,
  langFromURL,
};
