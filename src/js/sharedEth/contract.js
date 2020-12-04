function at(contract, address) {
  const clone = contract.clone();
  clone.options.address = address;

  return clone;
}

function deepGet(map, ...keys) {
  if (keys.length === 0) {
    return map;
  } else {
    const [key, ...rest] = keys;

    if (map.hasOwnProperty(key)) {
      return deepGet(map[key], ...rest);
    } else {
      return undefined;
    }
  }
}

function deepSet(map, value, ...keys) {
  if (keys.length === 1) {
    return (map[keys[0]] = value);
  } else {
    const [key, ...rest] = keys;

    if (map.hasOwnProperty(key)) {
      deepSet(map[key], value, ...rest);
    } else {
      const newMap = {};
      map[key] = newMap;

      deepSet(newMap, value, ...rest);
    }
  }
}

function fetchContract(web3Eth, contractJson, address) {
  if (!web3Eth.contracts) {
    web3Eth.contracts = {};
    web3Eth.parsedContracts = {};
  }

  // First, load and treturn the correct contract if we already have it
  let web3Contract = deepGet(web3Eth.contracts, web3Eth.type, contractJson.contractName, address);

  if (web3Contract !== undefined) {
    return web3Contract;
  }

  // Otherwise, see if we've already parsed it, or parse it ourselves
  let web3ParsedContract = deepGet(web3Eth.parsedContracts, web3Eth.type, contractJson.contractName);

  if (web3ParsedContract === undefined) {
    web3ParsedContract = new web3Eth.Contract(contractJson.abi);

    deepSet(web3Eth.parsedContracts, web3ParsedContract, web3Eth.type, contractJson.contractName);
  }

  // Now, set the address, store and return this contract
  web3Contract = at(web3ParsedContract, address);

  deepSet(web3Eth.contracts, web3Contract, web3Eth.type, contractJson.contractName, address);

  return web3Contract;
}

function getContractEvent(web3Eth, contractJson, eventName) {
  const abi = contractJson.abi.find((abi) => {
    return abi.type === 'event' && abi.name === eventName;
  });

  if (abi === undefined) {
    return null;
  } else {
    const signature = web3Eth.abi.encodeEventSignature(abi);

    return {
      abi: abi,
      signature: signature,
      matches: (log) => log.topics[0] == signature,
      decode: (log) => {
        return web3Eth.abi.decodeLog(abi.inputs, log.data, log.topics.slice(1));
      },
    };
  }
}

export { at, fetchContract, getContractEvent };
