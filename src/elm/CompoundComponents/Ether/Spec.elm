module CompoundComponents.Ether.Spec exposing (Spec(..), canonical, isDynamic)

import CompoundComponents.Ether.Address as Address exposing (Address)


type Spec
    = String
    | UInt Int
    | Address
    | Bytes
    | List Spec
    | Tuple (List Spec)


isDynamic : Spec -> Bool
isDynamic spec =
    case spec of
        String ->
            True

        UInt _ ->
            False

        Bytes ->
            True

        List s ->
            True

        Address ->
            False

        Tuple t ->
            t
                |> List.map isDynamic
                |> List.any identity


canonical : Spec -> String
canonical spec =
    case spec of
        String ->
            "string"

        UInt sz ->
            "uint" ++ String.fromInt sz

        Bytes ->
            "bytes"

        Address ->
            "address"

        List s ->
            canonical s ++ "[]"

        Tuple t ->
            let
                inner =
                    t
                        |> List.map canonical
                        |> String.join ","
            in
            "(" ++ inner ++ ")"
