module CompoundComponents.Ether.Coder exposing
    ( decode
    , encode
    , encodeList
    )

import BigInt exposing (BigInt)
import CompoundComponents.Ether.Address as Address exposing (Address)
import CompoundComponents.Ether.Helpers exposing (andThenFirst, combineResults, ensureTupleMaybe, tupleAndThenFirst)
import CompoundComponents.Ether.Hex as Hex exposing (Hex, wordLen)
import CompoundComponents.Ether.Spec as Spec exposing (Spec)
import CompoundComponents.Ether.Value as Value exposing (Value)
import Either exposing (Either(..))


decode : Spec -> Hex -> Result String Value
decode spec input =
    input
        |> doDecode spec Nothing
        |> Result.mapError (\err -> "Decode encountered error \"" ++ err ++ "\" while decoding " ++ Spec.canonical spec ++ " with data " ++ Hex.toString input)
        |> Result.andThen
            (\( val, excess, _ ) ->
                if Hex.isEmpty excess then
                    Ok val

                else
                    Err ("Excess data: " ++ Hex.toString excess)
            )


doDecode : Spec -> Maybe Int -> Hex -> Result String ( Value, Hex, Int )
doDecode spec maybeHeapOffset input =
    if Spec.isDynamic spec then
        Hex.readInt input
            |> Result.andThen
                (\( position, rest ) ->
                    let
                        heapOffset =
                            Maybe.withDefault 0 maybeHeapOffset

                        finalPos =
                            position
                                - 32
                                - heapOffset

                        ( staticRest, heap ) =
                            Hex.slice finalPos rest
                    in
                    decodeRaw spec heap
                        |> Result.map
                            (\( value, newHeap ) ->
                                ( value
                                , Hex.append staticRest newHeap
                                , Hex.length heap - Hex.length newHeap
                                )
                            )
                )

    else
        decodeRaw spec input
            |> Result.map (\( value, rest ) -> ( value, rest, 0 ))


decodeRaw : Spec -> Hex -> Result String ( Value, Hex )
decodeRaw spec input =
    case spec of
        Spec.UInt sz ->
            input
                |> Hex.readBigInt
                |> Result.andThen (tupleAndThenFirst (ensureSize sz))
                |> Result.map (Tuple.mapFirst (Value.UInt sz))

        Spec.Address ->
            input
                |> Hex.readWord
                |> Result.map (Tuple.mapFirst <| (Hex.slice 12 >> Tuple.second))
                |> Result.andThen (tupleAndThenFirst Address.fromHex)
                |> Result.map (Tuple.mapFirst Value.Address)

        Spec.String ->
            input
                |> Hex.readInt
                |> Result.andThen (readBytesHelper Hex.empty)
                |> Result.andThen (tupleAndThenFirst Hex.hexToString)
                |> Result.map (Tuple.mapFirst Value.String)

        Spec.Bytes ->
            input
                |> Hex.readInt
                |> Result.andThen (readBytesHelper Hex.empty)
                |> Result.map (Tuple.mapFirst Value.Bytes)

        Spec.List subspec ->
            input
                |> Hex.readInt
                |> Result.andThen (\( length, rest ) -> readListHelper subspec length rest)
                |> Result.map (\( l, rest, _ ) -> ( Value.List subspec (List.reverse l), rest ))

        Spec.Tuple t ->
            readTupleHelper t [] input 0


readTupleHelper : List Spec -> List Value -> Hex -> Int -> Result String ( Value, Hex )
readTupleHelper t v heap offset =
    case t of
        [] ->
            Ok ( Value.Tuple (List.reverse v), heap )

        spec :: ts ->
            doDecode spec (Just offset) heap
                |> Result.andThen
                    (\( value, nextHeap, heapRead ) ->
                        readTupleHelper ts (value :: v) nextHeap (offset + 32 + heapRead)
                    )


readBytesHelper : Hex -> ( Int, Hex ) -> Result String ( Hex, Hex )
readBytesHelper acc ( length, curr ) =
    if length <= 0 then
        Ok ( acc, curr )

    else
        case Hex.readWord curr of
            Err err ->
                Err err

            Ok ( word, next ) ->
                let
                    ( left, _ ) =
                        Hex.slice (min length wordLen) word
                in
                readBytesHelper
                    (Hex.append acc left)
                    ( length - 32, next )


readListHelper : Spec -> Int -> Hex -> Result String ( List Value, Hex, Int )
readListHelper spec length data =
    List.range 1 length
        |> List.foldl
            (\_ acc ->
                case acc of
                    Err err ->
                        Err err

                    Ok ( accList, currData, offset ) ->
                        doDecode spec (Just offset) currData
                            |> Result.map
                                (\( el, nextData, heapRead ) ->
                                    ( el :: accList, nextData, offset + 32 + heapRead )
                                )
            )
            (Ok ( [], data, 0 ))


encode : Value -> Result String Hex
encode val =
    doEncode (Value.Tuple [ val ])


encodeList : List Value -> Result String Hex
encodeList vals =
    doEncode (Value.Tuple vals)


doEncode : Value -> Result String Hex
doEncode val =
    encodeRaw val


encodeTuple : List Value -> Result String Hex
encodeTuple values =
    encodeHelper (wordLen * List.length values) [] Hex.empty values


encodeHelper : Int -> List (Either Hex Int) -> Hex -> List Value -> Result String Hex
encodeHelper heapHead headers heap values =
    case values of
        [] ->
            let
                header =
                    headers
                        |> List.map (Either.unpack Ok Hex.wordFromInt)
                        |> combineResults
                        |> Result.map (List.foldl Hex.append Hex.empty)
            in
            Hex.appendResults
                header
                (Ok heap)

        value :: rest ->
            encodeRaw value
                |> Result.andThen
                    (\encoded ->
                        if Spec.isDynamic (Value.toSpec value) then
                            encodeHelper (heapHead + Hex.length encoded) (Right heapHead :: headers) (Hex.append heap encoded) rest

                        else
                            encodeHelper heapHead (Left encoded :: headers) heap rest
                    )


encodeRaw : Value -> Result String Hex
encodeRaw val =
    case val of
        Value.UInt sz v ->
            v
                |> ensureSize sz
                |> Result.andThen Hex.wordFromBigInt

        Value.Address a ->
            a
                |> Address.toHex
                |> Hex.word

        Value.String s ->
            Hex.stringToHex s
                |> Value.Bytes
                |> encodeRaw

        Value.Bytes h ->
            Hex.wordFromInt (Hex.length h)
                |> Result.map
                    (\header ->
                        Hex.append
                            header
                            (writePaddedBytes h)
                    )

        Value.List spec list ->
            Hex.appendResults
                (Hex.wordFromInt (List.length list))
                (encodeTuple list)

        Value.Tuple t ->
            encodeTuple t


writePaddedBytes : Hex -> Hex
writePaddedBytes hex =
    let
        paddingLen =
            32 - modBy 32 (Hex.length hex)

        padding =
            List.range 1 paddingLen
                |> List.map (\_ -> 0)
                |> Hex.fromIntList
    in
    Hex.append hex padding


exp10 : Int -> BigInt
exp10 pow =
    BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt pow)


ensureSize : Int -> BigInt -> Result String BigInt
ensureSize bytes value =
    if BigInt.gte value (exp10 bytes) then
        Err ("Error: Integer does not fit in " ++ String.fromInt bytes ++ " bytes")

    else
        Ok value
