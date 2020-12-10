module CompoundComponents.Ether.RPC exposing (..)

import Array
import BigInt exposing (BigInt)
import CompoundComponents.Ether.Address as Address exposing (Address)
import CompoundComponents.Ether.Decoder as Decoder
import CompoundComponents.Ether.FunctionSpec as FunctionSpec
import CompoundComponents.Ether.Helpers exposing (combineResults, flip, skipNothings, tupleAndThenFirst)
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import CompoundComponents.Ether.Spec as Spec exposing (Spec)
import CompoundComponents.Ether.Value as Value exposing (Value)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias EthCallParams =
    { to : Address
    , from : Maybe Address
    , data : Hex
    , value : Maybe BigInt
    , gas : Maybe BigInt
    , gasPrice : Maybe BigInt
    }


type alias EthCall a =
    { to : Address
    , func : String
    , params : List Value
    , decoder : Decoder.Decoder a
    , block : Block
    }


type Block
    = Latest
    | Pending
    | Earliest
    | Block Int


rpcSingleBodyEnc : Int -> String -> Encode.Value -> Encode.Value
rpcSingleBodyEnc id method params =
    Encode.object
        [ ( "jsonrpc", Encode.string "2.0" )
        , ( "id", Encode.int id )
        , ( "method", Encode.string method )
        , ( "params", params )
        ]


rpcBatchBodyEnc : List ( String, Encode.Value ) -> Encode.Value
rpcBatchBodyEnc calls =
    Encode.list
        (\( id, method, params ) ->
            rpcSingleBodyEnc id method params
        )
        (List.indexedMap (\id ( method, params ) -> ( id, method, params )) calls)


{-| Note: the result doesn't need to be a string in usual JSON-RPC, but we're going to declare such
-}
rpcResponseDec : Decode.Decoder ( Int, String )
rpcResponseDec =
    Decode.map3 (\x y z -> ( x, y, z ))
        (Decode.field "id" Decode.int)
        (Decode.maybe <| Decode.field "result" Decode.string)
        (Decode.maybe <|
            Decode.field "error"
                (Decode.map2 Tuple.pair
                    (Decode.field "code" Decode.int)
                    (Decode.field "message" Decode.string)
                )
        )
        |> Decode.andThen
            (\( id, result, error ) ->
                case ( result, error ) of
                    ( _, Just ( code, message ) ) ->
                        Decode.fail ("Error " ++ String.fromInt code ++ ": " ++ message)

                    ( Nothing, Nothing ) ->
                        Decode.fail "Neither result or error given"

                    ( Just res, Nothing ) ->
                        Decode.succeed ( id, res )
            )


rpcPostSingle : String -> String -> Encode.Value -> (( Int, String ) -> Result err a) -> Http.Request (Result err a)
rpcPostSingle endpoint method params mapper =
    Http.post
        endpoint
        (Http.jsonBody (rpcSingleBodyEnc 0 method params))
        (rpcResponseDec
            |> Decode.map mapper
        )


rpcPostBatch : String -> List ( String, Encode.Value ) -> (List ( Int, String ) -> Result err a) -> Http.Request (Result err a)
rpcPostBatch endpoint calls mapper =
    Http.post
        endpoint
        (Http.jsonBody (rpcBatchBodyEnc calls))
        (rpcResponseDec
            |> Decode.list
            |> Decode.map mapper
        )


ethCallParamsEnc : Block -> EthCallParams -> Encode.Value
ethCallParamsEnc block { to, from, data, value, gas, gasPrice } =
    Encode.list identity
        [ Encode.object
            (skipNothings
                [ Maybe.map (\x -> ( "from", encodeAddress x )) from
                , Just ( "to", encodeAddress to )
                , Maybe.map (\x -> ( "gas", encodeBigInt x )) gas
                , Maybe.map (\x -> ( "gasPrice", encodeBigInt x )) gasPrice
                , Maybe.map (\x -> ( "value", encodeBigInt x )) value
                , Just ( "data", encodeHex data )
                ]
            )
        , encodeBlock block
        ]


encodeParamsForEthCall : Address -> String -> List Value -> Block -> Result String Encode.Value
encodeParamsForEthCall to func params block =
    FunctionSpec.encodeCall func params
        |> Result.map
            (\data ->
                ethCallParamsEnc
                    block
                    { to = to
                    , from = Nothing
                    , data = data
                    , value = Nothing
                    , gas = Nothing
                    , gasPrice = Nothing
                    }
            )


ethCallRequest : String -> Address -> String -> List Value -> (Hex -> Result String a) -> Block -> Result String (Http.Request (Result String a))
ethCallRequest endpoint to func params decoder block =
    encodeParamsForEthCall to func params block
        |> Result.map
            (\encoded ->
                rpcPostSingle endpoint
                    "eth_call"
                    encoded
                    (Tuple.second >> Hex.fromEthereum >> Result.andThen decoder)
            )


ethCall : String -> EthCall a -> Result String (Http.Request (Result String a))
ethCall endpoint { to, func, params, decoder, block } =
    ethCallRequest endpoint to func params (Decoder.decodeHex decoder) block


ethBatchCall : String -> List (EthCall a) -> Result String (Http.Request (Result String (List a)))
ethBatchCall endpoint ethCalls =
    ethCalls
        |> List.map
            (\{ to, func, params, decoder, block } ->
                case encodeParamsForEthCall to func params block of
                    Err err ->
                        Err err

                    Ok encoded ->
                        Ok ( encoded, decoder )
            )
        |> combineResults
        |> Result.map
            (\listEncodeds ->
                let
                    ethCallTuples =
                        List.map (\( encoded, decoder ) -> ( "eth_call", encoded )) listEncodeds

                    decodersArr =
                        listEncodeds
                            |> List.map Tuple.second
                            |> Array.fromList

                    listDecoder : List ( Int, String ) -> Result String (List a)
                    listDecoder =
                        List.foldl
                            (\( id, encoded ) ->
                                Result.andThen
                                    (\accList ->
                                        Array.get id decodersArr
                                            |> Result.fromMaybe "missing decoder from id"
                                            |> Result.andThen
                                                (\decoder ->
                                                    let
                                                        hexEncoded =
                                                            Hex.fromEthereum encoded
                                                    in
                                                    hexEncoded
                                                        |> Result.andThen (Decoder.decodeHex decoder)
                                                        |> Result.mapError (handleError hexEncoded)
                                                        |> Result.map (flip (::) accList)
                                                )
                                    )
                            )
                            (Ok [])
                in
                rpcPostBatch endpoint ethCallTuples listDecoder
            )


ethBalance : String -> Address -> Block -> Http.Request (Result String BigInt)
ethBalance endpoint address blockNumber =
    let
        encoded =
            Encode.list identity [ encodeAddress address, encodeBlock blockNumber ]

        zeroPad : Hex -> Hex
        zeroPad (Hex.Hex string) =
            let
                padding =
                    max 0 (64 - String.length string)

                paddedString =
                    String.repeat padding "0"
                        ++ string
            in
            Hex.Hex paddedString
    in
    rpcPostSingle endpoint
        "eth_getBalance"
        encoded
        (Tuple.second >> Hex.fromEthereum >> Result.andThen (\hex -> Decoder.decodeHex Decoder.uint (zeroPad hex)))


encodeHex : Hex -> Encode.Value
encodeHex hex =
    hex
        |> Hex.toEthereum
        |> Encode.string


encodeAddress : Address -> Encode.Value
encodeAddress address =
    encodeHex (Address.toHex address)


encodeBigInt : BigInt -> Encode.Value
encodeBigInt v =
    v
        |> Hex.fromBigInt
        |> encodeHex


encodeInt : Int -> Encode.Value
encodeInt v =
    v
        |> Hex.fromInt
        |> encodeHex


encodeBlock : Block -> Encode.Value
encodeBlock block =
    case block of
        Latest ->
            Encode.string "latest"

        Pending ->
            Encode.string "pending"

        Earliest ->
            Encode.string "earliest"

        Block x ->
            encodeInt x


handleError : Result String Hex -> String -> String
handleError hexResult originalErr =
    case hexResult of
        Ok hex ->
            let
                ( head, tail ) =
                    Hex.slice 4 hex
            in
            -- TODO: Make this nicer
            if Hex.toString head == "0x08c379a0" then
                Decoder.decodeHex Decoder.string tail
                    |> Result.map (\err -> "Revert: " ++ err)
                    |> Result.withDefault originalErr

            else
                originalErr

        Err err ->
            originalErr
