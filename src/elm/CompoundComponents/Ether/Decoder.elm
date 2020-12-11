module CompoundComponents.Ether.Decoder exposing (..)

import BigInt exposing (BigInt)
import CompoundComponents.Ether.Address as Address exposing (Address)
import CompoundComponents.Ether.Coder as Coder
import CompoundComponents.Ether.Helpers exposing (combineResults)
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import CompoundComponents.Ether.Spec as Spec exposing (Spec)
import CompoundComponents.Ether.Value as Value exposing (Value)


type Decoder a
    = Decoder Spec (Value -> Result String a)


decode : Decoder a -> String -> Result String a
decode decoder str =
    case decoder of
        Decoder spec mapper ->
            str
                |> Hex.parseHex
                |> Result.andThen (Coder.decode spec)
                |> Result.andThen mapper


decodeHex : Decoder a -> Hex -> Result String a
decodeHex decoder hex =
    case decoder of
        Decoder spec mapper ->
            hex
                |> Coder.decode spec
                |> Result.andThen mapper


decodeListHex : Decoder a -> Hex -> Result String a
decodeListHex decoder hex =
    case decoder of
        Decoder (Spec.Tuple l) _ ->
            Hex.appendResults
                (Hex.parseHex "0x0000000000000000000000000000000000000000000000000000000000000020")
                (Ok hex)
                |> Result.andThen (decodeHex decoder)

        _ ->
            Err "decodeList requires spec tuple type"


uint : Decoder BigInt
uint =
    uintSized 256


uintSized : Int -> Decoder BigInt
uintSized sz =
    Decoder (Spec.UInt sz)
        (\v ->
            case v of
                Value.UInt valueSz i ->
                    if valueSz <= sz then
                        Ok i

                    else
                        Err ("Decode size mismatch, value: " ++ String.fromInt valueSz ++ ", spec: " ++ String.fromInt sz)

                _ ->
                    Err "Spec value mismatch for uint"
        )


string : Decoder String
string =
    Decoder Spec.String
        (\v ->
            case v of
                Value.String s ->
                    Ok s

                _ ->
                    Err "Spec value mismatch for string"
        )


address : Decoder Address
address =
    Decoder Spec.Address
        (\v ->
            case v of
                Value.Address a ->
                    Ok a

                _ ->
                    Err "Spec value mismatch for address"
        )


list : Decoder a -> Decoder (List a)
list subDecoder =
    case subDecoder of
        Decoder subType subMapper ->
            Decoder (Spec.List subType)
                (\v ->
                    case v of
                        Value.List listSubType l ->
                            l
                                |> List.map subMapper
                                |> combineResults

                        _ ->
                            Err "Spec value mismatch for list"
                )


map : (a -> b) -> Decoder a -> Decoder b
map f d =
    case d of
        Decoder t m ->
            Decoder t (m >> Result.map f)


andThen : (a -> Result String b) -> Decoder a -> Decoder b
andThen f d =
    case d of
        Decoder t m ->
            Decoder t (m >> Result.andThen f)


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f x y =
    case ( x, y ) of
        ( Decoder typeA mapA, Decoder typeB mapB ) ->
            Decoder (Spec.Tuple [ typeA, typeB ])
                (\v ->
                    case v of
                        Value.Tuple [ valueA, valueB ] ->
                            Result.map2 f (mapA valueA) (mapB valueB)

                        _ ->
                            Err "Spec value mismatch for tuple"
                )


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f x y z =
    case ( x, y, z ) of
        ( Decoder typeA mapA, Decoder typeB mapB, Decoder typeC mapC ) ->
            Decoder (Spec.Tuple [ typeA, typeB, typeC ])
                (\v ->
                    case v of
                        Value.Tuple [ valueA, valueB, valueC ] ->
                            Result.map3 f (mapA valueA) (mapB valueB) (mapC valueC)

                        _ ->
                            Err "Spec value mismatch for tuple"
                )


map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 f x y z z1 =
    case ( x, y, z ) of
        ( Decoder typeA mapA, Decoder typeB mapB, Decoder typeC mapC ) ->
            case z1 of
                Decoder typeD mapD ->
                    Decoder (Spec.Tuple [ typeA, typeB, typeC, typeD ])
                        (\v ->
                            case v of
                                Value.Tuple [ valueA, valueB, valueC, valueD ] ->
                                    Result.map4 f (mapA valueA) (mapB valueB) (mapC valueC) (mapD valueD)

                                _ ->
                                    Err "Spec value mismatch for tuple"
                        )


getSpec : Decoder a -> Spec
getSpec d =
    case d of
        Decoder s m ->
            s


setSpec : Spec -> Decoder a -> Decoder a
setSpec s d =
    case d of
        Decoder _ m ->
            Decoder s m
