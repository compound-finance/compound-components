module CompoundComponents.Ether.FunctionSpec exposing (FunctionSpec, canonical, decodeResult, encodeCall, functionSig)

import BigInt exposing (BigInt)
import CompoundComponents.Ether.Coder as Coder
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import CompoundComponents.Ether.Sha exposing (sha3)
import CompoundComponents.Ether.Spec as Spec exposing (Spec)
import CompoundComponents.Ether.Value as Value exposing (Value)


type alias FunctionSpec =
    { name : String
    , input : List Spec
    , output : Maybe Spec
    }


functionSig : FunctionSpec -> Hex
functionSig sig =
    sha3 (canonical sig)
        |> Hex.slice 4
        |> Tuple.first


canonical : FunctionSpec -> String
canonical { name, input } =
    name ++ Spec.canonical (Spec.Tuple input)


{-| TODO: Should Value be a Value or a List of Value??
-}
encodeCall : String -> List Value -> Result String Hex
encodeCall name values =
    let
        functionSpec =
            { name = name
            , input = List.map Value.toSpec values
            , output = Nothing
            }
    in
    Hex.appendResults
        (Ok (functionSig functionSpec))
        -- Note: This is a quick hack to remove the first "position" of the tuple
        --       since apparently this is _not_ part of the encoded ABI data
        --       It would probably have made sense if the ABI-encoded function call
        --       where just a tuple, but instead it acts like a tuple, but it's really
        --       just a raw list of encoded values.
        (let
            valueTuple =
                Value.Tuple values

            isDynamic =
                valueTuple |> Value.toSpec |> Spec.isDynamic
         in
         if isDynamic then
            Coder.encode valueTuple
                |> Result.andThen Hex.readWord
                |> Result.map Tuple.second

         else
            Coder.encode valueTuple
        )


{-| TODO: This function isn't very useful on its own right now
But herein lies the rub- it would be very nice if Spec corresponded
directly to Value. But I think to do anything like that, we'd be
getting into Idris level type logic.
-}
decodeResult : Spec -> Hex -> Result String Value
decodeResult spec hex =
    Coder.decode spec hex
