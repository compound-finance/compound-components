module CompoundComponents.Ether.Hex exposing (..)

import BigInt exposing (BigInt)
import CompoundComponents.Ether.Helpers
    exposing
        ( andThenFirst
        , combineLists
        , splitString
        )
import Hex as HexLib
import Json.Decode


type Hex
    = Hex String


wordLen : Int
wordLen =
    32


isEmpty : Hex -> Bool
isEmpty hex =
    case hex of
        Hex str ->
            String.length str == 0


length : Hex -> Int
length hex =
    case hex of
        Hex str ->
            let
                hexLen =
                    String.length str
            in
            hexLen // 2


toString : Hex -> String
toString hex =
    case hex of
        Hex str ->
            "0x" ++ str


toRawString : Hex -> String
toRawString hex =
    case hex of
        Hex str ->
            str


{-| TODO: Note: what do we do when we see a value that's too high? We should decide if this needs to be a Result
-}
fromIntList : List Int -> Hex
fromIntList l =
    String.concat (List.map (HexLib.toString >> String.padLeft 2 '0') l)
        |> Hex


fromInt : Int -> Hex
fromInt int =
    Hex (HexLib.toString int)


fromBigInt : BigInt -> Hex
fromBigInt bigInt =
    Hex (BigInt.toHexString bigInt)


padToWord : Hex -> Hex
padToWord hex =
    case hex of
        Hex str ->
            let
                wordStrLen =
                    wordLen * 2

                strLen =
                    String.length str

                padLen =
                    wordStrLen
                        - modBy wordStrLen strLen
            in
            if padLen == wordStrLen && strLen > 0 then
                hex

            else
                Hex (String.repeat padLen "0" ++ str)


empty : Hex
empty =
    Hex ""


append : Hex -> Hex -> Hex
append a b =
    case ( a, b ) of
        ( Hex ha, Hex hb ) ->
            Hex (ha ++ hb)


appendResults : Result a Hex -> Result a Hex -> Result a Hex
appendResults a b =
    case ( a, b ) of
        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err

        ( Ok (Hex ha), Ok (Hex hb) ) ->
            Ok (Hex (ha ++ hb))


parseHex : String -> Result String Hex
parseHex str =
    -- TODO: Check it matches hex chars
    if String.left 2 str /= "0x" then
        Err "Hex should start with 0x"

    else
        let
            rest =
                String.dropLeft 2 str
        in
        if String.all Char.isHexDigit rest then
            Ok (Hex rest)

        else
            Err ("Invalid hex string " ++ rest)


readWord : Hex -> Result String ( Hex, Hex )
readWord hex =
    case hex of
        Hex str ->
            if String.length str < 64 then
                if str == "" then
                    Err "Insufficent data to read next word"

                else
                    Err ("Insufficent data to read next word, remaining: " ++ str)

            else
                Ok ( Hex (String.left 64 str), Hex (String.dropLeft 64 str) )


readWordAndThen : (Hex -> Result String a) -> Hex -> Result String ( a, Hex )
readWordAndThen f hex =
    hex
        |> readWord
        |> andThenFirst f


readInt : Hex -> Result String ( Int, Hex )
readInt =
    readWordAndThen parseInt


readBigInt : Hex -> Result String ( BigInt, Hex )
readBigInt =
    readWordAndThen parseBigInt


word : Hex -> Result String Hex
word hex =
    case hex of
        Hex hexStr ->
            let
                delta =
                    64 - String.length hexStr
            in
            if delta < 0 then
                Err ("Value does not fit in word " ++ hexStr)

            else
                Ok (Hex (String.repeat delta "0" ++ hexStr))


wordFromBigInt : BigInt -> Result String Hex
wordFromBigInt =
    BigInt.toHexString
        >> Hex
        >> word


wordFromInt : Int -> Result String Hex
wordFromInt =
    HexLib.toString
        >> Hex
        >> word


parseBigInt : Hex -> Result String BigInt
parseBigInt hex =
    case hex of
        Hex str ->
            BigInt.fromHexString str
                |> Result.fromMaybe ("Invalid hex string " ++ str)


parseInt : Hex -> Result String Int
parseInt hex =
    case hex of
        Hex str ->
            HexLib.fromString str
                |> Result.mapError (\_ -> "Invalid hex string " ++ str)


hexToString : Hex -> Result String String
hexToString hex =
    case hex of
        Hex str ->
            List.range 0 (length hex - 1)
                |> List.map (\i -> String.slice (i * 2) (i * 2 + 2) str)
                |> List.map HexLib.fromString
                |> List.foldl combineLists (Ok [])
                |> Result.map (List.map Char.fromCode)
                |> Result.map (List.foldl String.cons "")


stringToHex : String -> Hex
stringToHex str =
    str
        |> String.foldl (\el acc -> Char.toCode el :: acc) []
        |> List.map HexLib.toString
        |> List.foldl (++) ""
        |> Hex


{-| Can we make this more efficient?
-}
slice : Int -> Hex -> ( Hex, Hex )
slice pos hex =
    case hex of
        Hex str ->
            splitString (pos * 2) str
                |> Tuple.mapBoth Hex Hex


{-| TODO: Is this operation more complex
-}
toEthereum : Hex -> String
toEthereum hex =
    toString hex


{-| TODO: Is this operation more complex
-}
fromEthereum : String -> Result String Hex
fromEthereum str =
    parseHex str


decoder : Json.Decode.Decoder Hex
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case parseHex str of
                    Ok hex_ ->
                        Json.Decode.succeed hex_

                    Err err ->
                        Json.Decode.fail err
            )
