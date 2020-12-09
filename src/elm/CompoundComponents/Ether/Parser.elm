module CompoundComponents.Ether.Parser exposing (encode, parse)

import CompoundComponents.Ether.Spec as Spec
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , backtrackable
        , end
        , float
        , keyword
        , lazy
        , number
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        )



-- TODO: This might have more complex parsing, e.g. function types


parse : String -> Result (List DeadEnd) Spec.Spec
parse =
    run parser


parser : Parser Spec.Spec
parser =
    oneOf
        [ valueType
        ]
        |. end


valueType : Parser Spec.Spec
valueType =
    oneOf
        [ tuple
        , list
        , simpleValueType
        ]


simpleValueType : Parser Spec.Spec
simpleValueType =
    oneOf
        [ uint
        , string
        , address
        ]


{-| TODO: Handle multiple sized uints
-}
uint : Parser Spec.Spec
uint =
    succeed identity
        |. symbol "uint"
        |= oneOf
            [ number
                { int = Just Spec.UInt
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                , float = Nothing
                }
            , succeed (Spec.UInt 256)
                |. symbol ""
            ]


string : Parser Spec.Spec
string =
    succeed Spec.String
        |. keyword "string"


address : Parser Spec.Spec
address =
    succeed Spec.Address
        |. keyword "address"


list : Parser Spec.Spec
list =
    succeed Spec.List
        |= backtrackable (lazy (\_ -> simpleValueType))
        |. symbol "[]"


tuple : Parser Spec.Spec
tuple =
    succeed Spec.Tuple
        |. symbol "("
        |= tupleInner
        |. symbol ")"


tupleInner : Parser (List Spec.Spec)
tupleInner =
    oneOf
        [ succeed (::)
            |= lazy (\_ -> valueType)
            |= oneOf
                [ succeed identity
                    |. symbol ","
                    |= lazy (\_ -> tupleInner)
                , succeed []
                ]
        , succeed []
        ]


encode : Spec.Spec -> String
encode =
    Spec.canonical
