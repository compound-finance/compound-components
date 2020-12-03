module TestAlert exposing (testNetworkAlert)

import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (attribute)


class : String -> Attribute msg
class className =
    attribute "class" className


testNetworkAlert : String -> Html msg
testNetworkAlert alertMessage =
    div [ class "alert" ] [ text alertMessage ]
