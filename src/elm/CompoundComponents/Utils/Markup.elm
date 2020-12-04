module CompoundComponents.Utils.Markup exposing (checked, disabled, selected)

import Html exposing (Attribute, Html)
import Html.Attributes
import Json.Encode


disabled : Attribute msg
disabled =
    Html.Attributes.property "disabled" (Json.Encode.bool True)


selected : Attribute msg
selected =
    Html.Attributes.property "selected" (Json.Encode.bool True)


checked : Attribute msg
checked =
    Html.Attributes.property "checked" (Json.Encode.bool True)
