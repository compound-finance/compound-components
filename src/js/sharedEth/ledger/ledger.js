import { ethers } from 'ethers';

// import Eth from '../../../node_modules/web3-eth';
import LedgerEth from '@ledgerhq/hw-app-eth';
import Transport from '@ledgerhq/hw-transport-webusb';
import SignerProvider from './signer_provider';
import { encodeSignedTx, encodeTxForSignature } from './trx';

// We use this to serialize all calls to the Ledger; we should probably do this
// on a per-transport basis, but we only support one transport (per library)
let _pending = Promise.resolve(null);

let _oldLedgerPaths = ["44'/60'/0'/0", "44'/60'/0'/1", "44'/60'/0'/2"];
let _ledgerLivePaths = ["44'/60'/0'/0/0", "44'/60'/1'/0/0", "44'/60'/2'/0/0"];

class Ledger {
  constructor(transport, provider, options = {}) {
    options = {
      path: "44'/60'/0'/0/0",
      networkId: 1,
      ...options,
    };

    this.provider = provider;
    this.path = options.path;
    this._transport = transport;
    this._eth = new LedgerEth(transport);
    this.networkId = options.networkId;

    this._ready = this._eth.getAppConfiguration().then((result) => {
      this._config = JSON.stringify(result);
    });

    _pending = this._ready;

    this._config = JSON.stringify(null);
  }

  get config() {
    return JSON.parse(this._config);
  }

  getConfig() {
    return this._ready.then(() => {
      return this.config;
    });
  }

  getAddress() {
    let addressPromise = _pending.then(() => {
      return this._eth.getAddress(this.path).then((result) => {
        return result.address;
      });
    });
    _pending = addressPromise;
    return addressPromise;
  }

  async sign(transaction) {
    transaction.chainId = transaction.chainId || this.networkId;
    let { trxType, payloadToSign, rlpPayload } = encodeTxForSignature(transaction);

    await _pending; // be nice if there's other transactions pending

    // set ourselves as the global pending tx
    _pending = this._eth.signTransaction(this.path, payloadToSign.substring(2)).catch((error) => {
      //User rejected here. Don't have specific rejection UX yet.
    });
    let signature = await _pending; // await completion of our signature trx

    return encodeSignedTx(transaction, signature);
  }

  sendTransaction(transaction) {
    return this.sign(transaction).then((signedTx) => {
      return this.provider.sendTransaction(signedTx);
    });
  }

  signMessage(message) {
    if (typeof message === 'string' && message.slice(0, 2) !== '0x') {
      message = ethers.utils.toUtf8Bytes(message);
    }

    let messageHex = ethers.utils.hexlify(message).substring(2);

    let signPromise = _pending.then(() => {
      return this._eth.signPersonalMessage(this.path, messageHex).then((signature) => {
        signature.r = '0x' + signature.r;
        signature.s = '0x' + signature.s;
        return ethers.utils.joinSignature(signature);
      });
    });

    _pending = signPromise;

    return signPromise;
  }

  getProvider() {
    return new SignerProvider(this.provider.host, {
      signTransaction: (rawTx, cb) => {
        this.sign(rawTx)
          .then((signedTx) => {
            cb(null, signedTx);
          })
          .catch((err) => {
            console.log('ledger sign error:', err);
            cb(err, null);
          });
      },
      accounts: (cb) => {
        this.getAddress()
          .then((address) => {
            cb(null, [address]);
          })
          .catch((error) => {
            console.log('connect error' + error);
            // TODO: Torrey we need a callback to handle connection errors
          });
      },
      signPersonalMessage: (message, cb) => {
        this.signMessage(message)
          .then((signResult) => {
            cb(null, signResult);
          })
          .catch((err) => cb(err, null));
      },
    });
  }

  connectToProvider(provider) {
    return new Ledger(this._transport, provider);
  }

  static async connect(provider, options) {
    let transport = await Transport.create().catch((error) => console.log('connect error' + error));
    if (transport == undefined) {
      return null; //TODO: what to do here?
    }

    return new Ledger(transport, provider, options);
  }
}

export { Ledger };
