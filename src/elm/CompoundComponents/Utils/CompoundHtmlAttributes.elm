module CompoundComponents.Utils.CompoundHtmlAttributes exposing
    ( HrefLinkType(..)
    , autocomplete
    , class
    , data
    , disabled
    , for
    , href
    , id
    , onClickStopPropagation
    , onError
    , placeholder
    , property
    , src
    , style
    , target
    , type_
    , value
    )

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Html.Events exposing (custom, on)
import Json.Decode as Decode exposing (succeed)


type HrefLinkType
    = PageNavigation
    | Internal
    | External


autocomplete : Bool -> Attribute msg
autocomplete isOn =
    let
        valueString =
            if isOn then
                "on"

            else
                "off"
    in
    attribute "autocomplete" valueString


class : String -> Attribute msg
class className =
    attribute "class" className



--TODO: Not sure how to do disabled as an attribute or if we even want that?


data : String -> Attribute msg
data data_ =
    attribute "data" data_


disabled : Bool -> Attribute msg
disabled bool =
    Html.Attributes.disabled bool


for : String -> Attribute msg
for forValue =
    attribute "for" forValue


href : HrefLinkType -> String -> List (Attribute msg)
href linkType linkString =
    let
        relAttributes =
            case linkType of
                Internal ->
                    [ attribute "rel" "internal" ]

                External ->
                    [ attribute "rel" "external" ]

                PageNavigation ->
                    []
    in
    relAttributes ++ [ attribute "href" linkString ]


id : String -> Attribute msg
id idName =
    attribute "id" idName


placeholder : String -> Attribute msg
placeholder placeholderValue =
    attribute "placeholder" placeholderValue


property : String -> String -> Attribute msg
property propertyName propertyValue =
    attribute propertyName propertyValue


src : String -> Attribute msg
src srcName =
    attribute "src" srcName



--TODO: Not sure how to do styles as attributes?


style : String -> String -> Attribute msg
style styleName styleValue =
    Html.Attributes.style styleName styleValue


target : String -> Attribute msg
target targetName =
    attribute "target" targetName


type_ : String -> Attribute msg
type_ typeUnderscoreValue =
    attribute "type" typeUnderscoreValue


value : String -> Attribute msg
value valueName =
    attribute "value" valueName


onError : msg -> Attribute msg
onError msg =
    on "error" (succeed msg)



-- Events


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    custom "click"
        (Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
