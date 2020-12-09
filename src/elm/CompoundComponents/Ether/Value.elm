module CompoundComponents.Ether.Value exposing (Value(..), toSpec)

import BigInt exposing (BigInt)
import CompoundComponents.Ether.Address as Address exposing (Address)
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import CompoundComponents.Ether.Spec as Spec


type Value
    = String String
    | UInt Int BigInt
    | Bytes Hex
    | Address Address
    | List Spec.Spec (List Value)
    | Tuple (List Value)


toSpec : Value -> Spec.Spec
toSpec val =
    case val of
        String _ ->
            Spec.String

        UInt sz _ ->
            Spec.UInt sz

        Bytes _ ->
            Spec.Bytes

        Address _ ->
            Spec.Address

        List spec _ ->
            Spec.List spec

        Tuple values ->
            Spec.Tuple (List.map toSpec values)
