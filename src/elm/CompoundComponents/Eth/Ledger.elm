module CompoundComponents.Eth.Ledger exposing (LedgerAccount(..), intToLedgerAccount, ledgerAccountToInt)


type LedgerAccount
    = Cash
    | Borrow
    | Supply



-- Helpers


ledgerAccountToInt : LedgerAccount -> Int
ledgerAccountToInt ledgerAccount =
    case ledgerAccount of
        Cash ->
            0

        Borrow ->
            1

        Supply ->
            2


intToLedgerAccount : Int -> Result String LedgerAccount
intToLedgerAccount ledgerAccount =
    case ledgerAccount of
        0 ->
            Ok Cash

        1 ->
            Ok Borrow

        2 ->
            Ok Supply

        _ ->
            Err ("unknown ledger account: " ++ String.fromInt ledgerAccount)
