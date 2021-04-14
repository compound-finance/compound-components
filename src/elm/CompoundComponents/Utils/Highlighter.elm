port module CompoundComponents.Utils.Highlighter exposing
    ( highlight
    , highlightInline
    , highlightScroll
    , highlightSol
    , highlighted
    )

import Html exposing (Html, code, span, text)
import SyntaxHighlight exposing (gitHub, javascript, solidity, toBlockHtml, toInlineHtml, useTheme)


port highlightScroll : ( String, String ) -> Cmd msg


highlighted : Html msg
highlighted =
    span [] []


highlight : String -> Html msg
highlight jsCode =
    jsCode
        |> javascript
        |> Result.map (toBlockHtml (Just 1))
        |> Result.withDefault (code [] [ text jsCode ])


highlightSol : String -> Html msg
highlightSol solCode =
    solCode
        |> solidity
        |> Result.map (toBlockHtml (Just 1))
        |> Result.withDefault (code [] [ text solCode ])


highlightInline : String -> Html msg
highlightInline jsCode =
    jsCode
        |> javascript
        |> Result.map toInlineHtml
        |> Result.withDefault (code [] [ text jsCode ])
