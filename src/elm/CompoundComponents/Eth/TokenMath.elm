module CompoundComponents.Eth.TokenMath exposing
    ( getTokenAmount
    , getTokenWei
    , getTokenWeiStr
    )

{-| This module provides basic math functions for tokens and is intended to help avoid creating cyclic dependencies
by keeping these simple functions out of the larger Token module.
-}

import Decimal exposing (Decimal)


{-| Convert a protocol-friendly token wei amount to a human-friendly token amount.
It effectively divides amountWei by 10^^tokenDecimals
-}
getTokenAmount : Decimal -> Int -> Decimal
getTokenAmount amountWei tokenDecimals =
    let
        expDec =
            Decimal.fromIntWithExponent 1 (-1 * tokenDecimals)
    in
    Decimal.mul amountWei expDec


{-| Convert a human-friendly token amount to the wei amount.
It multiplies tokenAmount by 10^^tokenDecimals.
-}
getTokenWei : Decimal -> Int -> Decimal
getTokenWei tokenAmount tokenDecimals =
    let
        expDec =
            Decimal.fromIntWithExponent 1 tokenDecimals
    in
    Decimal.mul tokenAmount expDec
        |> Decimal.truncate 0


{-| Convenience wrapper around getTokenWei that additionally converts its result to a String
-}
getTokenWeiStr : Decimal -> Int -> String
getTokenWeiStr tokenAmount tokenDecimals =
    if Decimal.eq tokenAmount Decimal.minusOne then
        "-1"

    else
        Decimal.toString (getTokenWei tokenAmount tokenDecimals)
