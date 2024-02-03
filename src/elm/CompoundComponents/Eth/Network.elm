module CompoundComponents.Eth.Network exposing (Network(..), getEtherscanDomain, networkFromId, networkFromName, networkId, networkName)

-- From https://ethereum.stackexchange.com/a/17101


type Network
    = Olympic
    | MainNet
    | Morden
    | Ropsten
    | Rinkeby
    | Goerli
    | Ubiq
    | Kovan
    | Sokol
    | Core
    | Development
    | Unknown
    | Polygon
    | Mumbai
    | Arbitrum
    | ArbitrumGoerli
    | Optimism
    | OptimismGoerli
    | Base
    | BaseGoerli
    | Dmctest


networkFromId : Int -> Network
networkFromId networkIdVal =
    case networkIdVal of
        0 ->
            Olympic

        1 ->
            MainNet

        2 ->
            Morden

        3 ->
            Ropsten

        4 ->
            Rinkeby

        5 ->
            Goerli

        8 ->
            Ubiq

        10 ->
            Optimism

        42 ->
            Kovan

        77 ->
            Sokol

        99 ->
            Core

        137 ->
            Polygon

        420 ->
            OptimismGoerli

        999 ->
            Development

        1337 ->
            Development

        8453 ->
            Base

        42161 ->
            Arbitrum

        80001 ->
            Mumbai

        84531 ->
            BaseGoerli

        421613 ->
            ArbitrumGoerli

        1131 ->
            Dmctest

        _ ->
            Unknown


networkFromName : String -> Network
networkFromName name =
    case String.toLower name of
        "mainnet" ->
            MainNet

        "ropsten" ->
            Ropsten

        "goerli" ->
            Goerli

        "kovan" ->
            Kovan

        "rinkeby" ->
            Rinkeby

        "development" ->
            Development

        "dmctest" ->
            Dmctest

        _ ->
            MainNet


networkName : Network -> String
networkName network =
    case network of
        Olympic ->
            "Olympic"

        MainNet ->
            "Mainnet"

        Morden ->
            "Morden"

        Ropsten ->
            "Ropsten"

        Rinkeby ->
            "Rinkeby"

        Goerli ->
            "Goerli"

        Ubiq ->
            "Ubiq"

        Kovan ->
            "Kovan"

        Sokol ->
            "Sokol"

        Core ->
            "Core"

        Polygon ->
            "Polygon"

        Mumbai ->
            "Mumbai"

        Arbitrum ->
            "Arbitrum"

        ArbitrumGoerli ->
            "Arbitrum Goerli"

        Optimism ->
            "Optimism"

        OptimismGoerli ->
            "Optimism Goerli"

        Base ->
            "Base"

        BaseGoerli ->
            "Base Goerli"

        Development ->
            "Development"

        Unknown ->
            "unknown"

        Dmctest ->
            "dmctest"


networkId : Network -> Int
networkId network =
    case network of
        Olympic ->
            0

        MainNet ->
            1

        Morden ->
            2

        Ropsten ->
            3

        Rinkeby ->
            4

        Goerli ->
            5

        Ubiq ->
            8

        Optimism ->
            10

        Kovan ->
            42

        Sokol ->
            77

        Core ->
            99

        Polygon ->
            137

        OptimismGoerli ->
            420

        Development ->
            999

        Base ->
            8453

        Arbitrum ->
            42161

        Mumbai ->
            80001

        BaseGoerli ->
            84531

        ArbitrumGoerli ->
            421613

        Unknown ->
            9999
        
        Dmctest ->
            1131


getEtherscanDomain : Network -> Maybe String
getEtherscanDomain network =
    case network of
        Olympic ->
            Nothing

        MainNet ->
            Just "etherscan.io"

        Morden ->
            Nothing

        Ropsten ->
            Just "ropsten.etherscan.io"

        Rinkeby ->
            Just "rinkeby.etherscan.io"

        Goerli ->
            Just "goerli.etherscan.io"

        Ubiq ->
            Nothing

        Kovan ->
            Just "kovan.etherscan.io"

        Sokol ->
            Nothing

        Core ->
            Nothing

        Polygon ->
            Just "polygonscan.com"

        Mumbai ->
            Just "mumbai.polygonscan.com"

        Arbitrum ->
            Just "arbiscan.io"

        ArbitrumGoerli ->
            Just "goerli.arbiscan.io"

        Optimism ->
            Just "optimistic.etherscan.io"

        OptimismGoerli ->
            Just "goerli-optimism.etherscan.io"

        Base ->
            Just "basescan.org"

        BaseGoerli ->
            Just "goerli.basescan.org"

        Development ->
            Nothing

        Unknown ->
            Nothing

        Dmctest -> 
            Just "https://meta.defiscan.live/"