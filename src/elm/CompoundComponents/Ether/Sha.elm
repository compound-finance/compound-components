module CompoundComponents.Ether.Sha exposing (sha3)

import Bytes.Encode
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import Keccak.Bytes as Keccak


sha3 : String -> Hex
sha3 input =
    input
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Keccak.ethereum_keccak_256
        |> Hex.fromIntList
        |> Hex.padToWord
