import RLP from 'eth-lib/lib/rlp';
import Bytes from 'eth-lib/lib/bytes';
import utils from 'web3-utils';
import helpers from 'web3-core-helpers';

function trimLeadingZero(hex) {
  while (hex && hex.startsWith('0x0')) {
    hex = '0x' + hex.slice(3);
  }

  return hex;
}

function makeEven(hex) {
  if (hex.length % 2 === 1) {
    hex = hex.replace('0x', '0x0');
  }

  return hex;
}

export function encodeTx(tx) {
  if (!tx.gas && !tx.gasLimit) {
    throw new Error('"gas" is missing');
  }

  if (tx.nonce < 0 || tx.gas < 0 || tx.gasPrice < 0 || tx.chainId < 0) {
    throw new Error('Gas, gasPrice, nonce or chainId is lower than 0');
  }

  tx = helpers.formatters.inputCallFormatter(tx);

  var transaction = tx;
  transaction.to = tx.to || '0x';
  transaction.data = tx.data || '0x';
  transaction.value = tx.value || '0x';
  transaction.chainId = utils.numberToHex(tx.chainId);

  return RLP.encode([
    Bytes.fromNat(transaction.nonce),
    Bytes.fromNat(transaction.gasPrice),
    Bytes.fromNat(transaction.gas),
    transaction.to.toLowerCase(),
    Bytes.fromNat(transaction.value),
    transaction.data,
    Bytes.fromNat(transaction.chainId || '0x1'),
    '0x',
    '0x',
  ]);
}

export function encodeSignedTx(tx, signature) {
  const rlpEncoded = encodeTx(tx);

  var rawTx = RLP.decode(rlpEncoded)
    .slice(0, 6)
    .concat([`0x${signature.v}`, `0x${signature.r}`, `0x${signature.s}`]);

  rawTx[6] = makeEven(trimLeadingZero(rawTx[6]));
  rawTx[7] = makeEven(trimLeadingZero(rawTx[7]));
  rawTx[8] = makeEven(trimLeadingZero(rawTx[8]));

  return RLP.encode(rawTx);
}
