module CompoundComponents.Ether.Address exposing (Address(..), fromHex, parseHex, toHex, toString, zero)

import CompoundComponents.Ether.Hex as Hex exposing (Hex)


type Address
    = Address Hex


{-| TODO: Check length of address and maybe chomp excess 0's?
-}
fromHex : Hex -> Result String Address
fromHex hex =
    Ok (Address hex)


parseHex : String -> Result String Address
parseHex str =
    str
        |> Hex.parseHex
        |> Result.andThen fromHex


toHex : Address -> Hex
toHex address =
    case address of
        Address hex ->
            hex


toString : Address -> String
toString =
    toHex >> Hex.toString


zero : Address
zero =
    Hex.fromIntList (List.repeat 20 0)
        |> Address
