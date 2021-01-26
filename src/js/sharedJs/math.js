import BN from 'bn.js';

function toScaledDecimal(bn, decimal) {
  return {
    man: bn.toString(),
    exp: Number(decimal),
  };
}

function realToBigNumberScaled(real, scale) {
  // TODO: This is a hack since BN.js won't let us do proper float division :(
  if (real < 1) {
    return new BN(10).pow(new BN(scale)).div(new BN(1 / real));
  } else {
    return new BN(10).pow(new BN(scale)).mul(new BN(real));
  }
}

function parseWeiStr(weiStr) {
  return new BN(weiStr);
}

function resultToNumber(result) {
  if (result) {
    if (Number.isInteger(result)) {
      return result;
    } else {
      return Number(result);
    }
  } else {
    return null;
  }
}

function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}

export { getRandomInt, parseWeiStr, realToBigNumberScaled, resultToNumber, toScaledDecimal };
