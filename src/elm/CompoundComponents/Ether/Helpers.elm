module CompoundComponents.Ether.Helpers exposing (..)

import Array exposing (Array)
import BigInt exposing (BigInt)
import Decimal exposing (Decimal)
import Http
import Set exposing (Set)


negativeOne : BigInt
negativeOne =
    BigInt.fromHexString "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        |> Maybe.withDefault (BigInt.fromInt -1)


flip : (a -> b -> c) -> b -> a -> c
flip f y x =
    f x y


collapseResult : (a -> c) -> (x -> c) -> Result x a -> c
collapseResult mapResult mapError res =
    case res of
        Ok v ->
            mapResult v

        Err err ->
            mapError err


tupleAndThenFirst : (a -> Result err c) -> ( a, b ) -> Result err ( c, b )
tupleAndThenFirst fn ( a, b ) =
    case fn a of
        Err err ->
            Err err

        Ok v ->
            Ok ( v, b )


combineResults : List (Result String a) -> Result String (List a)
combineResults list =
    List.foldl
        (\el acc ->
            case ( el, acc ) of
                ( _, Err err ) ->
                    Err err

                ( Err err, _ ) ->
                    Err err

                ( Ok x, Ok xs ) ->
                    Ok (x :: xs)
        )
        (Ok [])
        list
        |> Result.map List.reverse


combineLists : Result err a -> Result err (List a) -> Result err (List a)
combineLists r acc =
    case ( acc, r ) of
        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err

        ( Ok l, Ok v ) ->
            Ok (v :: l)


andThenFirst : (a -> Result err x) -> Result err ( a, b ) -> Result err ( x, b )
andThenFirst f r =
    case r of
        Err err ->
            Err err

        Ok ( a, b ) ->
            case f a of
                Ok res ->
                    Ok ( res, b )

                Err err ->
                    Err err


splitString : Int -> String -> ( String, String )
splitString pos str =
    ( String.left pos str
    , String.dropLeft pos str
    )


skipNothings : List (Maybe a) -> List a
skipNothings l =
    l
        |> List.concatMap
            (\el ->
                case el of
                    Just x ->
                        [ x ]

                    Nothing ->
                        []
            )


skipErrors : List (Result x a) -> List a
skipErrors l =
    l
        |> List.map Result.toMaybe
        |> skipNothings


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl str ->
            "bad url " ++ str

        Http.Timeout ->
            "http timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus { body } ->
            "bad status: " ++ body

        Http.BadPayload err1 { body } ->
            "bad payload " ++ err1 ++ ": " ++ body


handleErrors : Result Http.Error (Result String data) -> Result String data
handleErrors res =
    case res of
        Err err ->
            Err (httpErrorToString err)

        Ok decodedRes ->
            decodedRes


ensureTupleMaybe : ( Result x a, b ) -> Result x ( a, b )
ensureTupleMaybe t =
    case t of
        ( Ok a, b ) ->
            Ok ( a, b )

        ( Err err, b ) ->
            Err err


flattenMaybe : Maybe (Maybe a) -> Maybe a
flattenMaybe m =
    case m of
        Just (Just a) ->
            Just a

        _ ->
            Nothing


setToggle : comparable -> Set comparable -> Set comparable
setToggle el set =
    if Set.member el set then
        Set.remove el set

    else
        Set.insert el set


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray index f arr =
    case Array.get index arr of
        Nothing ->
            arr

        Just el ->
            Array.set index (f el) arr


humanJoin : String -> String -> List String -> String
humanJoin joiner finalJoiner strings =
    let
        strLen =
            List.length strings
    in
    strings
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( index, el ) acc ->
                if index == 0 then
                    acc ++ el

                else if index == strLen - 1 then
                    acc ++ finalJoiner ++ el

                else
                    acc ++ joiner ++ el
            )
            ""
