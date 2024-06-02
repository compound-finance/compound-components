port module CompoundComponents.Ether.BNTransaction exposing (..)

import Array exposing (Array)
import CompoundComponents.Console as Console
import CompoundComponents.Eth.Decoders exposing (decodeNetwork)
import CompoundComponents.Eth.Ethereum exposing (Account(..), CustomerAddress(..))
import CompoundComponents.Eth.Network as Network exposing (Network, networkId)
import CompoundComponents.Ether.Address as EtherAddress exposing (Address(..))
import CompoundComponents.Ether.Helpers exposing (collapseResult)
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import CompoundComponents.Ether.Web3
import CompoundComponents.Functions exposing (handleError)
import CompoundComponents.Utils.Time as UtilsTime
import Json.Decode
import Set exposing (Set)
import Task
import Time


getTxModule : Network -> CustomerAddress -> String
getTxModule network (Customer accountString) =
    "BNTransactions-" ++ Network.networkName network ++ "-" ++ accountString


{-| txSent: Transaction has been sent to the network
txPool: Transaction is in the mempool and is pending
txConfirmed: Transaction has been mined
txFailed: Transaction has failed
txSpeedUp: A new transaction has been submitted with the same nonce and a higher gas price, replacing the original transaction
txCancel: A new transaction has been submitted with the same nonce, a higher gas price, a value of zero and sent to an external address (not a contract)
txDropped: Transaction was dropped from the mempool without being added to a block
-}
type TxStatus
    = AwaitingSig
    | Rejected
    | Sent
    | Pending
    | Confirmed
    | Failed
    | SpeedUp
    | Cancel
    | Dropped


type alias Transaction =
    { txModuleId : String
    , timestamp : Maybe Time.Posix
    , network : Network
    , txId : Int
    , txHash : Maybe Hex
    , txStatus : TxStatus
    , fromAddress : Address
    , toAddress : Address
    , function : String
    , args : List String
    }


type alias TxStatusUpdate =
    { txHash : Hex
    , txStatus : TxStatus
    , maybeBlockNumber : Maybe Int
    }


type alias BNTransactionState =
    { currentTransactionId : Int
    , transactions : List Transaction
    , bnTransactionTime : Maybe Time.Posix
    , errors : List String
    }


type BNTransactionMsg
    = SetTransactionHash ( String, Int, Hex )
    | TransactionStateChange ( String, Int, TxStatusUpdate )
    | TransactionRejected ( String, Int )
    | Tick Time.Posix
    | SetTransactionsFromStorage (List Transaction)
    | ClearTransactions
    | Error String


newTransaction : Network -> Address -> Address -> String -> List String -> BNTransactionState -> Transaction
newTransaction network fromAddress toAddress function args bnTransactionState =
    let
        customerAddress =
            Customer (EtherAddress.toString fromAddress)
    in
    { txModuleId = getTxModule network customerAddress
    , timestamp = bnTransactionState.bnTransactionTime
    , network = network
    , txId = bnTransactionState.currentTransactionId
    , txHash = Nothing
    , txStatus = AwaitingSig
    , fromAddress = fromAddress
    , toAddress = toAddress
    , function = function
    , args = args
    }


appendTrx : BNTransactionState -> Maybe Transaction -> BNTransactionState
appendTrx oldState maybeTrx =
    let
        ( updatedTransactions, newTransactionId ) =
            case maybeTrx of
                Just trx ->
                    ( trx :: oldState.transactions, trx.txId + 1 )

                Nothing ->
                    ( oldState.transactions, oldState.currentTransactionId )
    in
    { oldState
        | currentTransactionId = newTransactionId
        , transactions = updatedTransactions
    }


init : ( BNTransactionState, Cmd BNTransactionMsg )
init =
    ( { currentTransactionId = 0
      , transactions = []
      , bnTransactionTime = Nothing
      , errors = []
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , askStoredBNTransactions
        ]
    )


newNetworkCmd : Network -> BNTransactionState -> Cmd BNTransactionMsg
newNetworkCmd newNetwork bnTransactionState =
    let
        -- Because we can only use web3 on the current network, we can only really
        -- redundantly confirm trxs for the network we are currently on.
        watchPendingTxCmds =
            bnTransactionState.transactions
                |> List.filter
                    (\trx ->
                        (trx.txStatus == Sent)
                            || (trx.txStatus == Pending)
                            || (trx.txStatus == AwaitingSig)
                            && (trx.network == newNetwork)
                    )
                |> List.map
                    (\pendingTx ->
                        pendingTx.txHash
                            |> Maybe.map
                                (\hash ->
                                    watchTransaction pendingTx.txModuleId pendingTx.txId (Hex.toString hash)
                                )
                            |> Maybe.withDefault Cmd.none
                    )
                |> Cmd.batch
    in
    watchPendingTxCmds


update : Maybe Network -> Account -> BNTransactionMsg -> BNTransactionState -> ( BNTransactionState, Cmd BNTransactionMsg )
update maybeNetwork account msg ({ transactions } as state) =
    let
        userTXModule =
            getUserTxModule maybeNetwork account
    in
    case msg of
        SetTransactionHash ( txModule, txId, txHash ) ->
            let
                ( updatedTransactions, maybeUpdatedTransaction ) =
                    updateTransactionHashByTxId txModule txId txHash transactions

                txToStoreCmd =
                    maybeUpdatedTransaction
                        |> Maybe.map storeBNTransaction
                        |> Maybe.withDefault Cmd.none
            in
            ( { state | transactions = updatedTransactions }
            , txToStoreCmd
            )

        TransactionStateChange ( txModule, txId, txStatusUpdate ) ->
            ( let
                updatedTransactions =
                    updateTransactionStatusByTxId txModule txId txStatusUpdate transactions
              in
              { state | transactions = updatedTransactions }
            , storeBNTransactionUpdate txModule txId txStatusUpdate
            )

        TransactionRejected ( txModule, txId ) ->
            ( let
                updatedTransactions =
                    updateRejectedTransactionByTxId txModule txId transactions
              in
              { state | transactions = updatedTransactions }
            , Cmd.none
            )

        Tick time ->
            ( { state | bnTransactionTime = Just time }
            , Cmd.none
            )

        SetTransactionsFromStorage loadedTransactions ->
            let
                highestTransactionId =
                    loadedTransactions
                        |> List.foldl
                            (\trx largestId ->
                                if trx.txId > largestId then
                                    trx.txId

                                else
                                    largestId
                            )
                            0

                pendingOnlyTransactions =
                    loadedTransactions
                        |> List.filter
                            (\trx ->
                                (trx.txStatus == Sent)
                                    || (trx.txStatus == Pending)
                                    || (trx.txStatus == AwaitingSig)
                            )
            in
            ( { state
                | transactions = pendingOnlyTransactions
                , currentTransactionId = highestTransactionId + 1
              }
            , Cmd.none
            )

        ClearTransactions ->
            ( { state | transactions = [] }, askClearBNTransactions )

        Error error ->
            ( { state | errors = error :: state.errors }, Console.error error )


subscriptions : Sub BNTransactionMsg
subscriptions =
    Sub.batch
        [ setTransactionHash (collapseResult SetTransactionHash Error)
        , transactionStateChange (collapseResult TransactionStateChange Error)
        , transactionRejected (collapseResult TransactionRejected Error)
        , Time.every (1000.0 * 1.0 * toFloat UtilsTime.seconds) Tick
        , giveStoredBNTransactions (handleError (\_ -> ClearTransactions) SetTransactionsFromStorage)
        ]


etherscanUrl : Transaction -> Maybe String
etherscanUrl { network, txHash } =
    case txHash of
        Just txHashVal ->
            Network.getEtherscanDomain network
                |> Maybe.map
                    (\domain ->
                        "https://" ++ domain ++ "/tx/" ++ Hex.toString txHashVal
                    )

        _ ->
            Nothing


updateTransactionHashByTxId : String -> Int -> Hex -> List Transaction -> ( List Transaction, Maybe Transaction )
updateTransactionHashByTxId txModuleId txId txHash transactions =
    let
        setTxHash : Transaction -> Hex -> Transaction
        setTxHash trx hash =
            let
                status =
                    if trx.txStatus == AwaitingSig then
                        Pending

                    else
                        trx.txStatus
            in
            { trx | txHash = Just hash, txStatus = status }
    in
    transactions
        |> List.foldl
            (\trx ( accList, accUpdatedTx ) ->
                if txModuleId == trx.txModuleId && txId == trx.txId then
                    let
                        updatedTx =
                            setTxHash trx txHash
                    in
                    ( updatedTx :: accList, Just updatedTx )

                else
                    ( trx :: accList, accUpdatedTx )
            )
            ( [], Nothing )


updateTransactionStatusByTxId : String -> Int -> TxStatusUpdate -> List Transaction -> List Transaction
updateTransactionStatusByTxId txModuleId txId txStatusUpdate transactions =
    let
        setTxStatus : Transaction -> TxStatusUpdate -> Transaction
        setTxStatus trx { txHash, txStatus } =
            { trx | txHash = Just txHash, txStatus = txStatus }
    in
    transactions
        |> List.map
            (\trx ->
                if txModuleId == trx.txModuleId && txId == trx.txId then
                    setTxStatus trx txStatusUpdate

                else
                    trx
            )


updateRejectedTransactionByTxId : String -> Int -> List Transaction -> List Transaction
updateRejectedTransactionByTxId txModuleId txId transactions =
    let
        setTxRejected : Transaction -> Transaction
        setTxRejected trx =
            { trx | txStatus = Rejected }
    in
    transactions
        |> List.map
            (\trx ->
                if txModuleId == trx.txModuleId && txId == trx.txId then
                    setTxRejected trx

                else
                    trx
            )


statusEncoder : TxStatus -> String
statusEncoder txStatus =
    case txStatus of
        AwaitingSig ->
            "awaiting_sig"

        Pending ->
            "pending"

        Sent ->
            "sent"

        SpeedUp ->
            "speedup"

        Cancel ->
            "cancel"

        Confirmed ->
            "confirmed"

        Failed ->
            "failed"

        Dropped ->
            "dropped"

        Rejected ->
            "rejected"


statusDecoder : Json.Decode.Decoder TxStatus
statusDecoder =
    Json.Decode.string
        |> Json.Decode.map
            (\status ->
                case status of
                    "pending" ->
                        Ok Pending

                    "sent" ->
                        Ok Sent

                    "speedup" ->
                        Ok SpeedUp

                    "cancel" ->
                        Ok Cancel

                    "confirmed" ->
                        Ok Confirmed

                    "failed" ->
                        Ok Failed

                    "dropped" ->
                        Ok Dropped

                    "awaiting_sig" ->
                        Ok AwaitingSig

                    "rejected" ->
                        Ok Rejected

                    _ ->
                        Err ("Unknown status: " ++ status)
            )
        |> Json.Decode.andThen (collapseResult Json.Decode.succeed Json.Decode.fail)


statusMessage : TxStatus -> String
statusMessage status =
    case status of
        AwaitingSig ->
            "Transaction awaiting signature"

        Pending ->
            "Transaction is pending"

        Sent ->
            "Transaction awaiting inclusion in block"

        SpeedUp ->
            "Transaction has been resubmitted with a higher gas fee"

        Cancel ->
            "Transaction has been canceled"

        Confirmed ->
            "Transaction confirmed"

        Failed ->
            "Transaction failed"

        Dropped ->
            "Transaction has been dropped from mempool"

        Rejected ->
            "Transaction was rejected"


getPendingBNTransactionsForAccount : Maybe Network -> Account -> BNTransactionState -> List Transaction
getPendingBNTransactionsForAccount maybeNetwork account { transactions } =
    let
        userTxModule =
            getUserTxModule maybeNetwork account
    in
    transactions
        |> List.filter
            (\trx ->
                trx.txModuleId
                    == userTxModule
                    && (trx.txStatus == Sent || trx.txStatus == Pending)
            )


getUserTxModule : Maybe Network -> Account -> String
getUserTxModule maybeNetwork account =
    case ( maybeNetwork, account ) of
        ( Just network, Acct customer _ _ ) ->
            getTxModule network customer

        _ ->
            "UnknownModule"


port etherWatchTransactionPort : ( String, Int, String ) -> Cmd msg


watchTransaction : String -> Int -> String -> Cmd msg
watchTransaction txModule txId txHash =
    etherWatchTransactionPort ( txModule, txId, txHash )


port storeBNTransactionPort : { txModuleId : String, timestamp : Int, network : Int, txId : Int, txHash : String, txStatus : String, fromAddress : String, toAddress : String, func : String, args : List String } -> Cmd msg


storeBNTransaction : Transaction -> Cmd msg
storeBNTransaction { txModuleId, timestamp, network, txId, txHash, txStatus, fromAddress, toAddress, function, args } =
    case ( timestamp, txHash ) of
        ( Just actualTimestamp, Just actualTxHash ) ->
            storeBNTransactionPort
                { txModuleId = txModuleId
                , timestamp = Time.posixToMillis actualTimestamp
                , network = networkId network
                , txId = txId
                , txHash = Hex.toString actualTxHash
                , txStatus = statusEncoder txStatus
                , fromAddress = EtherAddress.toString fromAddress
                , toAddress = EtherAddress.toString toAddress
                , func = function
                , args = args
                }

        _ ->
            Cmd.none


port storeBNTransactionUpdatePort : { txModuleId : String, txId : Int, txHash : String, txStatus : String } -> Cmd msg


storeBNTransactionUpdate : String -> Int -> TxStatusUpdate -> Cmd msg
storeBNTransactionUpdate txModuleId txId { txHash, txStatus } =
    storeBNTransactionUpdatePort
        { txModuleId = txModuleId
        , txId = txId
        , txHash = Hex.toString txHash
        , txStatus = statusEncoder txStatus
        }


port askStoredBNTransactionsPort : {} -> Cmd msg


askStoredBNTransactions : Cmd msg
askStoredBNTransactions =
    askStoredBNTransactionsPort {}


port giveStoredBNTransactionsPort : (Json.Decode.Value -> msg) -> Sub msg


giveStoredBNTransactions : (Result Json.Decode.Error (List Transaction) -> msg) -> Sub msg
giveStoredBNTransactions wrapper =
    let
        hexDecoder =
            Json.Decode.string
                |> Json.Decode.andThen
                    (\hexString ->
                        case Hex.parseHex hexString of
                            Ok hexValue ->
                                Json.Decode.succeed hexValue

                            Err msg ->
                                Json.Decode.fail ("Error decoding BN Transaction : " ++ msg)
                    )

        decoder =
            Json.Decode.list
                (let
                    stage1 =
                        Json.Decode.map8 Transaction
                            (Json.Decode.field "txModuleId" Json.Decode.string)
                            (Json.Decode.field "timestamp" (Json.Decode.nullable (Json.Decode.int |> Json.Decode.map Time.millisToPosix)))
                            (Json.Decode.field "network" decodeNetwork)
                            (Json.Decode.field "txId" Json.Decode.int)
                            (Json.Decode.field "txHash" (Json.Decode.nullable hexDecoder))
                            (Json.Decode.field "txStatus" statusDecoder)
                            (Json.Decode.field "fromAddress" hexDecoder |> Json.Decode.map Address)
                            (Json.Decode.field "toAddress" hexDecoder |> Json.Decode.map Address)

                    transactionDecoder =
                        Json.Decode.map3
                            (<|)
                            stage1
                            (Json.Decode.field "func" Json.Decode.string)
                            (Json.Decode.field "args" (Json.Decode.list Json.Decode.string))
                 in
                 transactionDecoder
                )
    in
    giveStoredBNTransactionsPort
        (Json.Decode.decodeValue decoder >> wrapper)


port askClearBNTransactionsPort : {} -> Cmd msg


askClearBNTransactions : Cmd msg
askClearBNTransactions =
    askClearBNTransactionsPort {}


port etherTransactionHashPort : (Json.Decode.Value -> msg) -> Sub msg


setTransactionHash : (Result String ( String, Int, Hex ) -> msg) -> Sub msg
setTransactionHash wrapper =
    etherTransactionHashPort
        (Json.Decode.decodeValue
            (Json.Decode.map3 (\x y z -> ( x, y, z ))
                (Json.Decode.at [ "txModule" ] Json.Decode.string)
                (Json.Decode.at [ "txId" ] Json.Decode.int)
                (Json.Decode.at [ "txHash" ] Hex.decoder)
            )
            >> wrapError
            >> wrapper
        )


port etherTransactionStatePort : (Json.Decode.Value -> msg) -> Sub msg


transactionStateChange : (Result String ( String, Int, TxStatusUpdate ) -> msg) -> Sub msg
transactionStateChange wrapper =
    etherTransactionStatePort
        (Json.Decode.decodeValue
            (Json.Decode.map5 (\txModule txId txHash status maybeBlockNumber -> ( txModule, txId, { txHash = txHash, txStatus = status, maybeBlockNumber = maybeBlockNumber } ))
                (Json.Decode.at [ "txModule" ] Json.Decode.string)
                (Json.Decode.at [ "txId" ] Json.Decode.int)
                (Json.Decode.at [ "txHash" ] Hex.decoder)
                (Json.Decode.at [ "status" ] statusDecoder)
                (Json.Decode.at [ "blockNumber" ] (Json.Decode.maybe Json.Decode.int))
            )
            >> wrapError
            >> wrapper
        )


port etherTransactionRejectedPort : (Json.Decode.Value -> msg) -> Sub msg


transactionRejected : (Result String ( String, Int ) -> msg) -> Sub msg
transactionRejected wrapper =
    etherTransactionRejectedPort
        (Json.Decode.decodeValue
            (Json.Decode.map2 (\txModule txId -> ( txModule, txId ))
                (Json.Decode.at [ "txModule" ] Json.Decode.string)
                (Json.Decode.at [ "txId" ] Json.Decode.int)
            )
            >> wrapError
            >> wrapper
        )


wrapError : Result Json.Decode.Error a -> Result String a
wrapError =
    Result.mapError Json.Decode.errorToString
