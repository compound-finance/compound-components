module Strings.Translations exposing (..)


type Lang
    = En


getLnFromCode : String -> Lang
getLnFromCode code =
    case code of
        "en" ->
            En

        _ ->
            En


connect_wallet : Lang -> String
connect_wallet lang =
    case lang of
        En ->
            "Connect Wallet"


to_start_using_compound : Lang -> String
to_start_using_compound lang =
    case lang of
        En ->
            "To start using Compound"


metamask : Lang -> String
metamask lang =
    case lang of
        En ->
            "Metamask"


choose_wallet_terms_part1 : Lang -> String
choose_wallet_terms_part1 lang =
    case lang of
        En ->
            "By connecting, I accept Compound's "


choose_wallet_terms_part2 : Lang -> String
choose_wallet_terms_part2 lang =
    case lang of
        En ->
            "Terms of Service"


follow_wallet_link_instructions : Lang -> String
follow_wallet_link_instructions lang =
    case lang of
        En ->
            "Follow Coinbase Instructions"


scan_qr_code : Lang -> String
scan_qr_code lang =
    case lang of
        En ->
            "You may need to scan the wallet link QR code."


plugin_ledger_enter_pin : Lang -> String
plugin_ledger_enter_pin lang =
    case lang of
        En ->
            "Plug in Ledger & Enter Pin"


ledger_connection_failed : Lang -> String
ledger_connection_failed lang =
    case lang of
        En ->
            "Ledger Connection Failed"


ledger_connection_failed_instruction_1 : Lang -> String
ledger_connection_failed_instruction_1 lang =
    case lang of
        En ->
            "1. Unlock your ledger and open the ETH application."


ledger_connection_failed_instruction_2 : Lang -> String
ledger_connection_failed_instruction_2 lang =
    case lang of
        En ->
            "2. Verify Contract Data & Browser Support are enabled in ETH settings."


ledger_connection_failed_instruction_3 : Lang -> String
ledger_connection_failed_instruction_3 lang =
    case lang of
        En ->
            "3. If Browser Support is not an option in settings, update to latest firmware."


try_again : Lang -> String
try_again lang =
    case lang of
        En ->
            "Try Again"


open_ethereum_application : Lang -> String
open_ethereum_application lang =
    case lang of
        En ->
            "Open Ethereum application and make sure Contract Data and Browser Support are enabled."


unlock_wallet : Lang -> String
unlock_wallet lang =
    case lang of
        En ->
            "Unlock Wallet"


click_extension : Lang -> String
click_extension lang =
    case lang of
        En ->
            "You may need to click the extension."


select_address : Lang -> String
select_address lang =
    case lang of
        En ->
            "Select Address"


legacy : Lang -> String
legacy lang =
    case lang of
        En ->
            "Legacy"


ledger_live : Lang -> String
ledger_live lang =
    case lang of
        En ->
            "Ledger Live"


select : Lang -> String
select lang =
    case lang of
        En ->
            "Select"


none : Lang -> String
none lang =
    case lang of
        En ->
            "None"


ledger : Lang -> String
ledger lang =
    case lang of
        En ->
            "Ledger"


coinbase_wallet : Lang -> String
coinbase_wallet lang =
    case lang of
        En ->
            "Coinbase Wallet"


wallet_connect : Lang -> String
wallet_connect lang =
    case lang of
        En ->
            "Wallet Connect"


get_started : Lang -> String
get_started lang =
    case lang of
        En ->
            "Get Started"
