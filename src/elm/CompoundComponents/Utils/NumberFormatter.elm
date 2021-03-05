module CompoundComponents.Utils.NumberFormatter exposing
    ( commonFormatInThousandsOrMillions
    , formatActionInput
    , formatBlockNumber
    , formatCollateralFactor
    , formatCollateralRatio
    , formatCompAndVoteBalance
    , formatEth
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
    , formatOriginationFee
    , formatPercentage
    , formatPercentageToNearestWhole
    , formatPercentageWithDots
    , formatProposalId
    , formatRate
    , formatToDecimalPlaces
    , formatTokenBalance
    , formatTokenBalanceInNumberSpec
    , formatTokenBalanceInNumberSpecWithSymbol
    , formatTokenBalanceWithSymbol
    , formatUsd
    , formatUsdExtraDecimals
    , formatUsdInNumberSpec
    , formatUsdInThousandsOrMillions
    , getSignAndAbsValue
    , interestRateAsPercentage
    )

import CompoundComponents.Functions exposing (default)
import Decimal exposing (Decimal)


tokenDecimalPlaces : Int
tokenDecimalPlaces =
    4


actionInputDecimalPlaces : Int
actionInputDecimalPlaces =
    8


getSignAndAbsValue : Decimal -> ( String, Decimal )
getSignAndAbsValue decimalValue =
    let
        signString =
            if Decimal.lt decimalValue Decimal.zero then
                "- "

            else
                ""
    in
    ( signString, Decimal.abs decimalValue )


formatActionInput : Decimal -> String
formatActionInput decimalValue =
    -- Action input does not accept commas so we must use a modified version of
    -- our number formatter to work with the input box.
    let
        floatString =
            decimalValue |> Decimal.toString

        ( digitString, decimalString ) =
            case String.split "." floatString of
                [] ->
                    ( "", "" )

                [ headValue ] ->
                    ( headValue
                    , ""
                    )

                [ headValue, tailValue ] ->
                    ( headValue
                    , tailValue
                        |> String.left actionInputDecimalPlaces
                    )

                _ ->
                    ( "", "" )
    in
    case String.isEmpty decimalString of
        True ->
            digitString

        False ->
            digitString ++ "." ++ decimalString


formatBlockNumber : Maybe Int -> String
formatBlockNumber maybeBlockNumber =
    case maybeBlockNumber of
        Just blockNumber ->
            String.fromInt blockNumber

        Nothing ->
            "–"


formatCollateralFactor : Decimal -> String
formatCollateralFactor decimalValue =
    formatToDecimalPlaces 0 False (interestRateAsPercentage decimalValue) ++ "%"


formatCollateralRatio : Decimal -> String
formatCollateralRatio decimalValue =
    formatToDecimalPlaces 2 True decimalValue


formatEth : Decimal -> String
formatEth decimalValue =
    let
        ( signString, absoluteDecimalValue ) =
            getSignAndAbsValue decimalValue
    in
    signString ++ "Ξ" ++ formatToDecimalPlaces tokenDecimalPlaces False absoluteDecimalValue


formatEthToZeroDecimals : Decimal -> String
formatEthToZeroDecimals decimalValue =
    let
        ( signString, absoluteDecimalValue ) =
            getSignAndAbsValue decimalValue
    in
    signString ++ "Ξ" ++ formatToDecimalPlaces 0 False absoluteDecimalValue


formatRate : Decimal -> String
formatRate dec =
    let
        rounded =
            dec
                |> Decimal.round -2
    in
    Decimal.toString rounded ++ "%"


formatPercentage : Decimal -> String
formatPercentage decimalValue =
    formatToDecimalPlaces 2 False (interestRateAsPercentage decimalValue) ++ "%"


formatPercentageToNearestWhole : Decimal -> String
formatPercentageToNearestWhole decimalValue =
    formatToDecimalPlaces 0 False (interestRateAsPercentage decimalValue) ++ "%"


formatPercentageWithDots : Maybe Decimal -> String
formatPercentageWithDots maybeDecimalValue =
    CompoundComponents.Functions.defaultMap formatPercentage "..." maybeDecimalValue


formatOriginationFee : Decimal -> String
formatOriginationFee originationFeeBasisPoints =
    let
        originationFeeAsPercentage =
            Decimal.mul
                originationFeeBasisPoints
                (Decimal.fromInt 100)
                |> Decimal.round -3
    in
    formatToDecimalPlaces 3 False originationFeeAsPercentage ++ "%"


formatCompAndVoteBalance : Decimal -> String
formatCompAndVoteBalance decimalValue =
    let
        roundedValue =
            decimalValue
                |> Decimal.round -8

        ( signString, absoluteDecimalValue ) =
            getSignAndAbsValue roundedValue
    in
    signString ++ formatToDecimalPlaces 8 False absoluteDecimalValue


formatTokenBalance : Decimal -> String
formatTokenBalance decimalValue =
    let
        roundedValue =
            decimalValue
                |> Decimal.round -tokenDecimalPlaces

        ( signString, absoluteDecimalValue ) =
            getSignAndAbsValue roundedValue
    in
    signString ++ formatToDecimalPlaces tokenDecimalPlaces True absoluteDecimalValue


formatTokenBalanceWithSymbol : Decimal -> String -> String
formatTokenBalanceWithSymbol decimalValue symbol =
    formatTokenBalance decimalValue ++ " " ++ symbol


formatUsd : Bool -> Decimal -> String
formatUsd shouldTruncate decimalValue =
    let
        roundedValue =
            decimalValue
                |> Decimal.round -2

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue roundedValue

        numberOfDecimalPlaces =
            case shouldTruncate of
                True ->
                    0

                False ->
                    2
    in
    signString ++ "$" ++ formatToDecimalPlaces numberOfDecimalPlaces False absoluteRoundedValue


formatUsdExtraDecimals : Decimal -> String
formatUsdExtraDecimals decimalValue =
    let
        numberOfDecimalPlaces =
            if Decimal.gt decimalValue (Decimal.fromInt 100000) then
                6

            else
                8

        roundedValue =
            decimalValue
                |> Decimal.round -numberOfDecimalPlaces

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue roundedValue
    in
    signString ++ "$" ++ formatToDecimalPlaces numberOfDecimalPlaces False absoluteRoundedValue


formatGbp : Bool -> Decimal -> String
formatGbp shouldTruncate decimalValue =
    let
        roundedValue =
            decimalValue
                |> Decimal.round -2

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue roundedValue

        numberOfDecimalPlaces =
            case shouldTruncate of
                True ->
                    0

                False ->
                    2
    in
    signString ++ "£" ++ formatToDecimalPlaces numberOfDecimalPlaces False absoluteRoundedValue


formatGbpExtraDecimals : Decimal -> String
formatGbpExtraDecimals decimalValue =
    let
        roundedValue =
            decimalValue
                |> Decimal.round -6

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue roundedValue

        numberOfDecimalPlaces =
            6
    in
    signString ++ "£" ++ formatToDecimalPlaces numberOfDecimalPlaces False absoluteRoundedValue


formatEuro : Bool -> Decimal -> String
formatEuro shouldTruncate decimalValue =
    let
        roundedValue =
            decimalValue
                |> Decimal.round -2

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue roundedValue

        numberOfDecimalPlaces =
            case shouldTruncate of
                True ->
                    0

                False ->
                    2
    in
    signString ++ "€" ++ formatToDecimalPlaces numberOfDecimalPlaces False absoluteRoundedValue


formatEuroExtraDecimals : Decimal -> String
formatEuroExtraDecimals decimalValue =
    let
        roundedValue =
            decimalValue
                |> Decimal.round -6

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue roundedValue

        numberOfDecimalPlaces =
            6
    in
    signString ++ "€" ++ formatToDecimalPlaces numberOfDecimalPlaces False absoluteRoundedValue


commonFormatInThousandsOrMillions : String -> Decimal -> String
commonFormatInThousandsOrMillions currencySymbol decimalValue =
    let
        millionsValue =
            Decimal.fastdiv decimalValue (Decimal.fromInt 1000000)
                |> Maybe.withDefault Decimal.zero

        thousandsValue =
            Decimal.fastdiv decimalValue (Decimal.fromInt 1000)
                |> Maybe.withDefault Decimal.zero

        ( scaledValueToUse, roundedDecimals, valueSuffix ) =
            if Decimal.gt millionsValue Decimal.one then
                ( millionsValue, 2, "M" )

            else
                ( thousandsValue, 0, "k" )

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue scaledValueToUse
    in
    signString ++ currencySymbol ++ formatToDecimalPlaces roundedDecimals False absoluteRoundedValue ++ valueSuffix


formatUsdInThousandsOrMillions : Decimal -> String
formatUsdInThousandsOrMillions decimalValue =
    commonFormatInThousandsOrMillions "$" decimalValue


formatGbpInThousandsOrMillions : Decimal -> String
formatGbpInThousandsOrMillions decimalValue =
    commonFormatInThousandsOrMillions "£" decimalValue


formatEuroInThousandsOrMillions : Decimal -> String
formatEuroInThousandsOrMillions decimalValue =
    commonFormatInThousandsOrMillions "€" decimalValue



-- Formatting from spec here: https://docs.google.com/document/d/1VghwT8wTh4wtEfc2K3aBcIOMtsGPc6-eHEA4JBBnKl8/edit#heading=h.ahl50pnvza5t


getNumberSpecValues : Decimal -> ( Decimal, Int, String )
getNumberSpecValues decimalValue =
    let
        thousandsValue =
            Decimal.fastdiv decimalValue (Decimal.fromInt 1000)
                |> Maybe.withDefault Decimal.zero
    in
    if Decimal.lt decimalValue (Decimal.fromInt 100000) then
        ( decimalValue, 2, "" )

    else if Decimal.lt decimalValue (Decimal.fromInt 1000000) then
        ( thousandsValue, 2, "k" )

    else
        let
            millionsValue =
                Decimal.fastdiv decimalValue (Decimal.fromInt 1000000)
                    |> Maybe.withDefault Decimal.zero
        in
        ( millionsValue, 4, "M" )


commonFormatInNumberSpec : String -> Decimal -> String
commonFormatInNumberSpec currencySymbol decimalValue =
    let
        ( scaledValueToUse, roundedDecimals, valueSuffix ) =
            getNumberSpecValues decimalValue

        ( signString, absoluteRoundedValue ) =
            getSignAndAbsValue scaledValueToUse
    in
    signString ++ currencySymbol ++ formatToDecimalPlaces roundedDecimals False absoluteRoundedValue ++ valueSuffix


formatUsdInNumberSpec : Decimal -> String
formatUsdInNumberSpec decimalValue =
    commonFormatInNumberSpec "$" decimalValue


formatEthInNumberSpec : Decimal -> String
formatEthInNumberSpec decimalValue =
    let
        roundedDecimals =
            if Decimal.lt decimalValue Decimal.one then
                6

            else if Decimal.lt decimalValue (Decimal.fromInt 100) then
                4

            else if Decimal.lt decimalValue (Decimal.fromInt 10000) then
                2

            else
                0

        ( signString, absoluteDecimalValue ) =
            getSignAndAbsValue decimalValue
    in
    signString ++ "Ξ" ++ formatToDecimalPlaces roundedDecimals True absoluteDecimalValue


formatGbpInNumberSpec : Decimal -> String
formatGbpInNumberSpec decimalValue =
    commonFormatInNumberSpec "£" decimalValue


formatEuroInNumberSpec : Decimal -> String
formatEuroInNumberSpec decimalValue =
    commonFormatInNumberSpec "€" decimalValue


formatTokenBalanceInNumberSpec : Decimal -> String
formatTokenBalanceInNumberSpec decimalValue =
    let
        roundedDecimals =
            if Decimal.lt decimalValue (Decimal.fromInt 10) then
                4

            else if Decimal.lt decimalValue (Decimal.fromInt 1000) then
                2

            else
                0

        roundedValue =
            decimalValue
                |> Decimal.truncate -roundedDecimals

        ( signString, absoluteDecimalValue ) =
            getSignAndAbsValue roundedValue
    in
    signString ++ formatToDecimalPlaces roundedDecimals True absoluteDecimalValue


formatTokenBalanceInNumberSpecWithSymbol : Decimal -> String -> String
formatTokenBalanceInNumberSpecWithSymbol decimalValue symbol =
    formatTokenBalanceInNumberSpec decimalValue ++ " " ++ symbol


interestRateAsPercentage : Decimal -> Decimal
interestRateAsPercentage interestRate =
    Decimal.mul
        interestRate
        (Decimal.fromInt 100)
        |> Decimal.round -2



-- Proposal Ids are current show with at least 3 places of precision
-- (ie: 001, 002, etc...)


formatProposalId : Int -> String
formatProposalId proposalId =
    let
        intString =
            String.fromInt proposalId

        intStringLength =
            String.length intString
    in
    if intStringLength < 3 then
        String.repeat (3 - intStringLength) "0"
            ++ intString

    else
        intString



---- Internal Methods ----
{--
    The current elm number formatter decimals fields always shows
    the number of decimals specifed. For example, format {decimals = 4} 2.0
    gives the string "2.0000" but we really just want a max decimal field
    so this function implements that.
    TODO: This function currenctly does NOT handle negative numbers correctly.
--}


formatToDecimalPlaces : Int -> Bool -> Decimal -> String
formatToDecimalPlaces decimalPlaces dropZeros decimalValue =
    let
        floatString =
            decimalValue |> Decimal.toString

        dropper =
            if dropZeros then
                dropRight '0'

            else
                \i -> i

        ( digitString, decimalString ) =
            case String.split "." floatString of
                [] ->
                    ( "", "" )

                [ headValue ] ->
                    ( String.join "," (splitThousands headValue)
                    , ""
                    )

                [ headValue, tailValue ] ->
                    ( String.join "," (splitThousands headValue)
                    , tailValue
                        |> dropper
                        |> String.left decimalPlaces
                    )

                _ ->
                    ( "", "" )
    in
    case String.isEmpty decimalString of
        True ->
            digitString

        False ->
            digitString ++ "." ++ decimalString


splitThousands : String -> List String
splitThousands integers =
    let
        reversedSplitThousands : String -> List String
        reversedSplitThousands value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> reversedSplitThousands
                    |> (::) (String.right 3 value)

            else
                [ value ]
    in
    integers
        |> reversedSplitThousands
        |> List.reverse


dropRight : Char -> String -> String
dropRight dropChar string =
    let
        reducer : Char -> Maybe String -> Maybe String
        reducer nextChar res =
            case res of
                Nothing ->
                    if nextChar == dropChar then
                        Nothing

                    else
                        Just (String.fromChar nextChar)

                Just str ->
                    Just (String.cons nextChar str)
    in
    default (String.foldr reducer Nothing string) ""
