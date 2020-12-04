module CompoundComponents.Functions exposing (andThen3, andThen6, cond, decimalMax, decimalMin, decimalMin3, default, defaultMap, demaybeify, dictFilterMap, first, handleError, map2, map3, map4, map5, maybeMap)

import Decimal exposing (Decimal)
import Dict exposing (Dict)


default : Maybe a -> a -> a
default maybeVal defaultVal =
    case maybeVal of
        Just val ->
            val

        Nothing ->
            defaultVal


maybeMap : (a -> b) -> Maybe a -> Maybe b
maybeMap fun maybeVal =
    case maybeVal of
        Just val ->
            Just (fun val)

        Nothing ->
            Nothing


defaultMap : (a -> b) -> b -> Maybe a -> b
defaultMap fun defaultVal maybeVal =
    case maybeVal of
        Just val ->
            fun val

        Nothing ->
            defaultVal


demaybeify : Maybe (Maybe a) -> Maybe a
demaybeify maybeMaybeVal =
    case maybeMaybeVal of
        Just val ->
            val

        Nothing ->
            Nothing


handleError : (err -> msg) -> (success -> msg) -> (Result err success -> msg)
handleError errorMessage wrapper =
    \result ->
        case result of
            Ok success ->
                wrapper success

            Err error ->
                errorMessage error


first : List a -> Maybe a
first list =
    case list of
        val :: rest ->
            Just val

        _ ->
            Nothing


cond : Bool -> a -> a -> a
cond test trueText falseText =
    case test of
        True ->
            trueText

        False ->
            falseText


decimalMin3 : Decimal -> Decimal -> Decimal -> Decimal
decimalMin3 a b c =
    decimalMin a b
        |> decimalMin c


decimalMax : Decimal -> Decimal -> Decimal
decimalMax a b =
    if Decimal.gt a b then
        a

    else
        b


decimalMin : Decimal -> Decimal -> Decimal
decimalMin a b =
    if Decimal.lt a b then
        a

    else
        b


map2 : Maybe t0 -> Maybe t1 -> (t0 -> t1 -> r) -> Maybe r
map2 m0 m1 f =
    case ( m0, m1 ) of
        ( Just v0, Just v1 ) ->
            Just (f v0 v1)

        _ ->
            Nothing


map3 : Maybe t0 -> Maybe t1 -> Maybe t2 -> (t0 -> t1 -> t2 -> r) -> Maybe r
map3 m0 m1 m2 f =
    let
        r =
            map2 m0 m1 f
    in
    andMap r m2


map4 : Maybe t0 -> Maybe t1 -> Maybe t2 -> Maybe t3 -> (t0 -> t1 -> t2 -> t3 -> r) -> Maybe r
map4 m0 m1 m2 m3 f =
    let
        r =
            map3 m0 m1 m2 f
    in
    andMap r m3


map5 : Maybe t0 -> Maybe t1 -> Maybe t2 -> Maybe t3 -> Maybe t4 -> (t0 -> t1 -> t2 -> t3 -> t4 -> r) -> Maybe r
map5 m0 m1 m2 m3 m4 f =
    let
        r =
            map4 m0 m1 m2 m3 f
    in
    andMap r m4


andThen3 : Maybe t0 -> Maybe t1 -> Maybe t2 -> (t0 -> t1 -> t2 -> Maybe r) -> Maybe r
andThen3 m0 m1 m2 f =
    map3 m0 m1 m2 f
        |> demaybeify


andThen6 : Maybe t0 -> Maybe t1 -> Maybe t2 -> Maybe t3 -> Maybe t4 -> Maybe t5 -> (t0 -> t1 -> t2 -> t3 -> t4 -> t5 -> Maybe r) -> Maybe r
andThen6 m0 m1 m2 m3 m4 m5 f =
    case ( m0, ( m1, ( m2, ( m3, ( m4, m5 ) ) ) ) ) of
        ( Just v0, ( Just v1, ( Just v2, ( Just v3, ( Just v4, Just v5 ) ) ) ) ) ->
            f v0 v1 v2 v3 v4 v5

        _ ->
            Nothing


andMap : Maybe (a -> b) -> Maybe a -> Maybe b
andMap f x =
    Maybe.andThen
        (\x_ ->
            Maybe.andThen
                (\f_ -> Just <| f_ x_)
                f
        )
        x


{-| From Dict.Extra
-}
dictFilterMap : (comparable -> a -> Maybe b) -> Dict comparable a -> Dict comparable b
dictFilterMap f dict =
    Dict.foldl
        (\k v acc ->
            case f k v of
                Just newVal ->
                    Dict.insert k newVal acc

                Nothing ->
                    acc
        )
        Dict.empty
        dict
