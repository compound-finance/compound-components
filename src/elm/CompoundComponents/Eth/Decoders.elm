module CompoundComponents.Eth.Decoders exposing
    ( decimal
    , decodeAssetAddress
    , decodeContractAddress
    , decodeCustomerAddress
    , decodeLedgerAccount
    , decodeNetwork
    , decodeTrxHash
    , encodedDecimal
    , floatDecimal
    , forceMaybe
    , forceOk
    , stringDecimal
    , stringDecimalWithDefault
    )

import CompoundComponents.Eth.Ethereum exposing (AssetAddress(..), ContractAddress(..), CustomerAddress(..), TrxHash)
import CompoundComponents.Eth.Ledger exposing (LedgerAccount, intToLedgerAccount)
import CompoundComponents.Eth.Network exposing (Network, networkFromId)
import Decimal exposing (Decimal)
import Json.Decode exposing (Decoder, andThen, fail, field, float, int, map, map2, maybe, oneOf, string, succeed)


forceMaybe : String -> Maybe b -> Decoder b
forceMaybe err maybe =
    case maybe of
        Just val ->
            succeed val

        Nothing ->
            fail err


forceOk : Result String b -> Decoder b
forceOk result =
    case result of
        Ok val ->
            succeed val

        Err err ->
            fail err


floatDecimal : Decoder Decimal
floatDecimal =
    float
        |> map Decimal.fromFloat
        |> andThen (forceMaybe "invalid float")


stringDecimal : Decoder Decimal
stringDecimal =
    string
        |> map Decimal.fromString
        |> andThen (forceMaybe "invalid float")


stringDecimalWithDefault : Decimal -> Decoder Decimal
stringDecimalWithDefault default =
    string
        |> map Decimal.fromString
        |> map (Maybe.withDefault default)


encodedDecimal : Decoder Decimal
encodedDecimal =
    map2 (\a b -> ( a, b ))
        (field "man" stringDecimal)
        (field "exp" int)
        |> map
            (\( man, exp ) ->
                let
                    expDec =
                        Decimal.fromIntWithExponent 1 (-1 * exp)
                in
                Decimal.mul man expDec
            )


decimal : Decoder Decimal
decimal =
    oneOf [ floatDecimal, stringDecimal, encodedDecimal ]


decodeTrxHash : Decoder TrxHash
decodeTrxHash =
    string


decodeAssetAddress : Decoder AssetAddress
decodeAssetAddress =
    string |> andThen (succeed << Asset)


decodeCustomerAddress : Decoder CustomerAddress
decodeCustomerAddress =
    string |> andThen (succeed << Customer)


decodeContractAddress : Decoder ContractAddress
decodeContractAddress =
    string
        |> map String.toLower
        |> andThen (succeed << Contract)


decodeLedgerAccount : Decoder LedgerAccount
decodeLedgerAccount =
    int
        |> map intToLedgerAccount
        |> andThen forceOk


decodeNetwork : Decoder Network
decodeNetwork =
    int
        |> map networkFromId
