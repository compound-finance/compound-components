module CompoundComponents.DisplayCurrency exposing
    ( DisplayCurrency(..)
    , ValueType(..)
    , displayCurrencyToString
    , formatDisplayCurrency
    , formatDisplayCurrencyInNumberSpec
    , formatDisplayCurrencyManyDecimals
    , formatMarketSize
    , getPlaceholderPrefCurrency
    , readDisplayCurrency
    )

import CompoundComponents.Utils.NumberFormatter
    exposing
        ( formatEth
        , formatEthInNumberSpec
        , formatEthToZeroDecimals
        , formatEuro
        , formatEuroExtraDecimals
        , formatEuroInNumberSpec
        , formatEuroInThousandsOrMillions
        , formatGbp
        , formatGbpExtraDecimals
        , formatGbpInNumberSpec
        , formatGbpInThousandsOrMillions
        , formatUsd
        , formatUsdExtraDecimals
        , formatUsdInNumberSpec
        , formatUsdInThousandsOrMillions
        )
import Decimal exposing (Decimal)
import Dict


type DisplayCurrency
    = Ether
    | USD
    | GBP
    | EUR


readDisplayCurrency : String -> Maybe DisplayCurrency
readDisplayCurrency =
    Dict.fromList [ ( "Ether", Ether ), ( "USD", USD ), ( "GBP", GBP ), ( "EUR", EUR ) ]
        |> (\b a -> Dict.get a b)


calculateGbpValue : Decimal -> Maybe Decimal
calculateGbpValue usdValue =
    case Decimal.fromFloat 1.41 of
        Just gbpDivisor ->
            Decimal.fastdiv usdValue gbpDivisor

        _ ->
            Nothing


calculateEuroValue : Decimal -> Maybe Decimal
calculateEuroValue usdValue =
    case Decimal.fromFloat 1.21 of
        Just euroDivisor ->
            Decimal.fastdiv usdValue euroDivisor

        _ ->
            Nothing


type ValueType
    = EthValue Decimal
    | UsdValue Decimal


getUsdAndEthValuesToDisplay : Maybe Decimal -> ValueType -> ( Maybe Decimal, Maybe Decimal )
getUsdAndEthValuesToDisplay maybeEtherPrice valueToDisplay =
    case ( valueToDisplay, maybeEtherPrice ) of
        ( EthValue ethValue_, Just etherPrice ) ->
            ( ethValue_
                |> Decimal.mul etherPrice
                |> Just
            , ethValue_
                |> Just
            )

        ( UsdValue usdValue_, Just etherPrice_ ) ->
            ( usdValue_
                |> Just
            , Decimal.fastdiv usdValue_ etherPrice_
            )

        ( UsdValue usdValue_, _ ) ->
            ( usdValue_
                |> Just
            , Nothing
            )

        _ ->
            ( Nothing, Nothing )


formatDisplayCurrencyManyDecimals : DisplayCurrency -> Maybe Decimal -> ValueType -> Maybe String
formatDisplayCurrencyManyDecimals displayCurrency maybeEtherPrice valueToDisplay =
    let
        ( usdValue, ethValue ) =
            getUsdAndEthValuesToDisplay maybeEtherPrice valueToDisplay
    in
    case displayCurrency of
        Ether ->
            ethValue
                |> Maybe.map formatEth

        USD ->
            usdValue
                |> Maybe.map formatUsdExtraDecimals

        GBP ->
            usdValue
                |> Maybe.andThen calculateGbpValue
                |> Maybe.map formatGbpExtraDecimals

        EUR ->
            usdValue
                |> Maybe.andThen calculateEuroValue
                |> Maybe.map formatEuroExtraDecimals


formatDisplayCurrencyInNumberSpec : DisplayCurrency -> Maybe Decimal -> ValueType -> String
formatDisplayCurrencyInNumberSpec displayCurrency maybeEtherPrice valueToDisplay =
    let
        ( usdValue, ethValue ) =
            getUsdAndEthValuesToDisplay maybeEtherPrice valueToDisplay
    in
    (case displayCurrency of
        Ether ->
            ethValue
                |> Maybe.map formatEthInNumberSpec

        USD ->
            usdValue
                |> Maybe.map formatUsdInNumberSpec

        GBP ->
            usdValue
                |> Maybe.andThen calculateGbpValue
                |> Maybe.map formatGbpInNumberSpec

        EUR ->
            usdValue
                |> Maybe.andThen calculateEuroValue
                |> Maybe.map formatEuroInNumberSpec
    )
        |> Maybe.withDefault "–"


formatDisplayCurrency : Bool -> DisplayCurrency -> Maybe Decimal -> ValueType -> Maybe String
formatDisplayCurrency truncated displayCurrency maybeEtherPrice valueToDisplay =
    let
        ( usdValue, ethValue ) =
            getUsdAndEthValuesToDisplay maybeEtherPrice valueToDisplay
    in
    case displayCurrency of
        Ether ->
            ethValue
                |> Maybe.map formatEth

        USD ->
            usdValue
                |> Maybe.map (formatUsd truncated)

        GBP ->
            usdValue
                |> Maybe.andThen calculateGbpValue
                |> Maybe.map (formatGbp truncated)

        EUR ->
            usdValue
                |> Maybe.andThen calculateEuroValue
                |> Maybe.map (formatEuro truncated)


formatMarketSize : DisplayCurrency -> Maybe Decimal -> ValueType -> Maybe String
formatMarketSize displayCurrency maybeEtherPrice valueToDisplay =
    let
        ( usdValue, ethValue ) =
            getUsdAndEthValuesToDisplay maybeEtherPrice valueToDisplay
    in
    case displayCurrency of
        Ether ->
            ethValue
                |> Maybe.map formatEthToZeroDecimals

        USD ->
            usdValue
                |> Maybe.map formatUsdInThousandsOrMillions

        GBP ->
            usdValue
                |> Maybe.andThen calculateGbpValue
                |> Maybe.map formatGbpInThousandsOrMillions

        EUR ->
            usdValue
                |> Maybe.andThen calculateEuroValue
                |> Maybe.map formatEuroInThousandsOrMillions


getPlaceholderPrefCurrency : DisplayCurrency -> String
getPlaceholderPrefCurrency displayCurrency =
    case displayCurrency of
        Ether ->
            "Ξ –"

        USD ->
            "$ –"

        GBP ->
            "£ –"

        EUR ->
            "€ –"


displayCurrencyToString : DisplayCurrency -> String
displayCurrencyToString displayCurrency =
    case displayCurrency of
        Ether ->
            "Ether"

        USD ->
            "USD"

        GBP ->
            "GBP"

        EUR ->
            "EUR"
