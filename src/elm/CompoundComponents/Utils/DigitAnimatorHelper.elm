module CompoundComponents.Utils.DigitAnimatorHelper exposing (valueFormattedStringToDigits, valueFormattedStringToRandomDigits)

import CompoundComponents.DisplayCurrency exposing (DisplayCurrency(..))
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (class, property)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


valueFormattedStringToDigits : Maybe String -> String -> DisplayCurrency -> Bool -> List (Html msg)
valueFormattedStringToDigits maybePrevString totalValueFormattedString displayCurrency removeActiveClass =
    let
        decimalsIndexOffset =
            String.split "." totalValueFormattedString
                |> List.drop 1
                |> List.head
                |> Maybe.map String.toList
                |> Maybe.map List.length
                |> Maybe.withDefault 0
    in
    --TODO: need to handle the other currency formats like EU where , and . are reversed.
    totalValueFormattedString
        |> String.toList
        |> List.reverse
        |> List.indexedMap
            (\index character ->
                let
                    charAsString =
                        String.fromChar character

                    aciveClass =
                        if removeActiveClass then
                            ""

                        else
                            " active"

                    { digitExtraClass, containerExtraClass, textContent, numDigits } =
                        case String.toInt charAsString of
                            Just digitAsInt ->
                                let
                                    digitSizeExtraClass_ =
                                        (if digitAsInt == 0 then
                                            " one"

                                         else
                                            ""
                                        )
                                            ++ (if index - decimalsIndexOffset < 0 then
                                                    " decimals"

                                                else
                                                    ""
                                               )

                                    digitClass_ =
                                        if index - 6 < 0 then
                                            " digit-one"

                                        else if index - 10 < 0 then
                                            " digit-ten"

                                        else if index - 14 < 0 then
                                            " digit-hundred"

                                        else
                                            " digit-thousand"

                                    digitsArrayFromZero target =
                                        List.range 0 target
                                            |> List.reverse
                                            |> List.map String.fromInt

                                    digitsRange =
                                        case maybePrevString of
                                            Just prevString ->
                                                if String.length prevString == String.length totalValueFormattedString then
                                                    let
                                                        prevReversedString =
                                                            String.reverse prevString
                                                    in
                                                    case String.toInt (String.slice index (index + 1) prevReversedString) of
                                                        Just prevCharAsInt ->
                                                            let
                                                                resultText =
                                                                    if digitAsInt < prevCharAsInt then
                                                                        (List.range prevCharAsInt 9
                                                                            ++ List.range 0 digitAsInt
                                                                        )
                                                                            |> List.reverse
                                                                            |> List.map String.fromInt

                                                                    else
                                                                        List.range prevCharAsInt digitAsInt
                                                                            |> List.reverse
                                                                            |> List.map String.fromInt
                                                            in
                                                            resultText

                                                        Nothing ->
                                                            digitsArrayFromZero digitAsInt

                                                else
                                                    digitsArrayFromZero digitAsInt

                                            Nothing ->
                                                digitsArrayFromZero digitAsInt
                                in
                                { digitExtraClass = digitSizeExtraClass_
                                , containerExtraClass = digitClass_
                                , textContent = String.join " " digitsRange
                                , numDigits = List.length digitsRange - 1
                                }

                            Nothing ->
                                let
                                    ( digitSizeExtraClass_, digitClass_, textContent_ ) =
                                        if character == ',' then
                                            ( " comma", "", charAsString )

                                        else if character == '.' then
                                            ( " comma decimals", "", charAsString )

                                        else
                                            ( "", "", charAsString )
                                in
                                { digitExtraClass = digitSizeExtraClass_
                                , containerExtraClass = digitClass_
                                , textContent = textContent_
                                , numDigits = 0
                                }
                in
                div [ class ("digit" ++ digitExtraClass) ]
                    [ div
                        [ class ("digit-holder" ++ containerExtraClass ++ aciveClass)
                        , property "style" ("--num-digits: " ++ String.fromInt numDigits ++ ";")
                        ]
                        [ text textContent ]
                    ]
            )
        |> List.reverse



-- This code generates an array of digits with semi random digit classes
-- which are animated into view roulette style. The extra class attributes
-- defines a different animation style that was chose totally randomly by
-- Torrey :D
-- ie: $2,193
--   digit [ class "digit digit-thousand"] [ text "0 1 2"]
--   digit [ class "digit comma"] [ text ","]
--   digit [ class "digit digit-hundred"] [ text "0 1"]
--   digit [ class "digit digit-hundred"] [ text "0 1 2 3 4 5 6 7 8 9"]
--   digit [ class "digit digit-thousand"] [ text "0 1 2 3"]


valueFormattedStringToRandomDigits : String -> List (Html msg)
valueFormattedStringToRandomDigits totalValueFormattedString =
    totalValueFormattedString
        |> String.toList
        |> List.map
            (\character ->
                let
                    charAsString =
                        String.fromChar character

                    ( digitExtraClass, containerExtraClass, textContent ) =
                        case String.toInt charAsString of
                            Just digitAsInt ->
                                let
                                    digitsRange =
                                        List.range 0 digitAsInt
                                            |> List.reverse
                                            |> List.map String.fromInt

                                    digitClass =
                                        case digitAsInt of
                                            2 ->
                                                " digit-thousand"

                                            7 ->
                                                " digit-thousand"

                                            9 ->
                                                " digit-hundred"

                                            1 ->
                                                " digit-hundred"

                                            4 ->
                                                " digit-ten"

                                            5 ->
                                                " digit-ten"

                                            _ ->
                                                " digit-thousand"
                                in
                                ( "", digitClass, String.join " " digitsRange )

                            Nothing ->
                                if character == ',' || character == '.' then
                                    ( " comma", "", charAsString )

                                else
                                    ( "", "", charAsString )
                in
                div [ class ("digit" ++ digitExtraClass) ]
                    [ div [ class ("digit-holder" ++ containerExtraClass ++ " active") ]
                        [ text textContent ]
                    ]
            )
