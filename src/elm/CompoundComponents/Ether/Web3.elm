port module CompoundComponents.Ether.Web3 exposing (sendTransaction, sendTransactionWithValue)

import BigInt exposing (BigInt)
import CompoundComponents.Ether.Address as Address exposing (Address)
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import Json.Decode
import Json.Encode


type alias TransactionRequest =
    { from : Address
    , to : Address
    , data : Hex
    }


port etherSendTransactionPort : ( String, Int, Json.Encode.Value ) -> Cmd msg


sendTransaction : String -> Int -> TransactionRequest -> Cmd msg
sendTransaction txModule txId { from, to, data } =
    let
        txParams =
            Json.Encode.object
                [ ( "from", Json.Encode.string (Address.toString from) )
                , ( "to", Json.Encode.string (Address.toString to) )
                , ( "data", Json.Encode.string (Hex.toString data) )
                , ( "value", Json.Encode.string "0" )
                ]
    in
    etherSendTransactionPort ( txModule, txId, txParams )


sendTransactionWithValue : BigInt -> String -> Int -> TransactionRequest -> Cmd msg
sendTransactionWithValue value txModule txId { from, to, data } =
    let
        txParams =
            Json.Encode.object
                [ ( "from", Json.Encode.string (Address.toString from) )
                , ( "to", Json.Encode.string (Address.toString to) )
                , ( "data", Json.Encode.string (Hex.toString data) )
                , ( "value", Json.Encode.string (BigInt.toString value) )
                ]
    in
    etherSendTransactionPort ( txModule, txId, txParams )
