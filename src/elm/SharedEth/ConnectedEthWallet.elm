port module SharedEth.ConnectedEthWallet exposing
    ( ChooseWalletState(..)
    , ConnectionState(..)
    , InternalMsg
    , LedgerAcccountData
    , Model
    , Translator
    , WalletProviderType(..)
    , chooseWalletView
    , giveAccount
    , giveFinishedLedgerRetrieval
    , giveLedgerAccountAdddress
    , giveTrxProviderType
    , init
    , resetModel
    , subscriptions
    , translator
    , update
    )

import Decimal exposing (Decimal)
import Html exposing (Html, a, button, div, h4, h5, p, span, text)
import Html.Events exposing (onClick)
import Json.Decode
import SharedElm.Functions exposing (handleError)
import SharedElm.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, onClickStopPropagation)
import SharedElm.Utils.Markup
import SharedElm.Utils.NumberFormatter exposing (formatTokenBalanceWithSymbol)
import SharedEth.Decoders
import SharedEth.Ethereum exposing (Account(..), CustomerAddress(..), getCustomerAddressString, shortenedAddressString)
import SharedEth.Network exposing (Network, networkFromId, networkId)
import Strings.Translations as Translations


type WalletProviderType
    = Metamask
    | WalletLink
    | Ledger
    | OtherWeb3Browser
    | None


type ConnectionState
    = Disconnected
    | Connecting
    | ConnectedAcct CustomerAddress


type ChooseWalletState
    = ChooseProvider
    | LoadingLegerAccounts
    | LedgerConnectionError
    | ChooseLedgerAccount
    | AttemptingConnectToWallet
    | WalletConnectedChooseHidden


type alias Model =
    { chooseWalletState : ChooseWalletState
    , selectedProvider : Maybe WalletProviderType
    , connectionState : Maybe ConnectionState
    , connectionNetwork : Maybe Network
    , chooseLedgerAcccountState : ConnectingLedgerState
    , errors : List String
    }


type alias ConnectingLedgerState =
    { isVisible : Bool
    , ledgerAccounts : LedgerAccounts
    , pathSelectorActive : Bool
    , addressSelectorActive : Bool
    , chooseLedgerInLegacyMode : Bool
    , choosenLedgerAccount : Maybe LedgerAcccountData
    }


type alias LedgerAcccountData =
    { derivationPath : String
    , accountAddress : Maybe CustomerAddress
    , ethBalance : Maybe Decimal
    }


type alias LedgerAccounts =
    { legacyAccounts : List LedgerAcccountData
    , liveAccounts : List LedgerAcccountData
    }


type InternalMsg
    = SelectWalletProvider WalletProviderType
    | SetWalletProvider (Maybe WalletProviderType)
    | SetAccount (Maybe CustomerAddress)
    | SetNetwork (Maybe Network)
    | ReceivedLedgerAccountAddress LedgerAcccountData
    | LedgerAccountsRetrievalDone Bool
    | ToggleLedgerAccountTypeDropdown
    | ToggleLedgerAccountSelectorDropwdown
    | SelectLedgerAccountType Bool
    | SelectLedgerAccount LedgerAcccountData
    | SelectLedgerAccountFinished
    | ResetToChooseProvider
    | Error String


type Msg
    = ForSelf InternalMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal


emptyLedgerAccountData : String -> LedgerAcccountData
emptyLedgerAccountData derivationPath =
    { derivationPath = derivationPath
    , accountAddress = Nothing
    , ethBalance = Nothing
    }


resetLedgerState : ConnectingLedgerState
resetLedgerState =
    let
        legacyDerivationPaths =
            [ "44'/60'/0'/0"
            , "44'/60'/0'/1"
            , "44'/60'/0'/2"
            ]

        liveDerivationPaths =
            [ "44'/60'/0'/0/0"
            , "44'/60'/1'/0/0"
            , "44'/60'/2'/0/0"
            ]

        emptyLedgerAccounts =
            { legacyAccounts =
                legacyDerivationPaths
                    |> List.map (\path -> emptyLedgerAccountData path)
            , liveAccounts =
                liveDerivationPaths
                    |> List.map (\path -> emptyLedgerAccountData path)
            }
    in
    { isVisible = False
    , ledgerAccounts = emptyLedgerAccounts
    , pathSelectorActive = False
    , addressSelectorActive = False
    , chooseLedgerInLegacyMode = True
    , choosenLedgerAccount = Nothing
    }


resetModel : Model -> Model
resetModel model =
    { model
        | chooseWalletState = ChooseProvider
        , selectedProvider = Just None
        , chooseLedgerAcccountState = resetLedgerState
    }


init : ( Model, Cmd Msg )
init =
    let
        newEmptyModel =
            { chooseWalletState = ChooseProvider
            , selectedProvider = Just None
            , connectionState = Nothing
            , connectionNetwork = Nothing
            , chooseLedgerAcccountState = resetLedgerState
            , errors = []
            }
    in
    ( newEmptyModel
    , tryConnect True
    )


update : InternalMsg -> Model -> ( Model, Cmd msg )
update internalMsg model =
    case internalMsg of
        SelectWalletProvider walletProvider ->
            let
                ( updatedModel, connectedWalletCmd ) =
                    if walletProvider == Ledger && (model.chooseWalletState == ChooseProvider || model.chooseWalletState == LedgerConnectionError) then
                        --Ledger requires an extra step before we can set the provider
                        ( model, askRetrieveLedgerAccounts model )

                    else
                        selectWalletProvider model walletProvider ""

                updatedChooseWalletState =
                    case ( model.chooseWalletState, walletProvider ) of
                        ( ChooseProvider, Ledger ) ->
                            LoadingLegerAccounts

                        ( LedgerConnectionError, Ledger ) ->
                            LoadingLegerAccounts

                        ( ChooseProvider, _ ) ->
                            AttemptingConnectToWallet

                        _ ->
                            model.chooseWalletState
            in
            ( { model | chooseWalletState = updatedChooseWalletState }, connectedWalletCmd )

        -- This can be called from autoconnect
        SetWalletProvider walletProvider ->
            ( { model | selectedProvider = walletProvider }, Cmd.none )

        SetAccount Nothing ->
            --TODO:
            ( model, Cmd.none )

        SetAccount (Just newAccount) ->
            let
                updatedConnectionState =
                    Just (ConnectedAcct newAccount)

                updatedChooseWalletState =
                    if model.chooseWalletState == AttemptingConnectToWallet || model.chooseWalletState == ChooseLedgerAccount then
                        WalletConnectedChooseHidden

                    else
                        model.chooseWalletState
            in
            ( { model
                | connectionState = updatedConnectionState
                , chooseWalletState = updatedChooseWalletState
              }
            , Cmd.none
            )

        SetNetwork network ->
            ( { model | connectionNetwork = network }, Cmd.none )

        _ ->
            --TODO: Finish moving me over.
            ( model, Cmd.none )



-- Views


chooseWalletView : Translations.Lang -> Model -> Html Msg
chooseWalletView userLanguage ({ chooseWalletState } as model) =
    case chooseWalletState of
        ChooseProvider ->
            div [ class "copy" ]
                [ span [ class "mark" ] []
                , h4 [] [ text (Translations.connect_wallet userLanguage) ]
                , p [ class "center-text" ] [ text (Translations.to_start_using_compound userLanguage) ]
                , div [ class "connect-choices" ]
                    [ a [ class "connect-item", onClick <| ForSelf <| SelectWalletProvider WalletLink ]
                        [ span [ class "icon coinbase" ] []
                        , h5 [ class "connect-item-text" ] [ text (Translations.coinbase_wallet userLanguage) ]
                        , span [ class "arrow big green" ] []
                        ]
                    , div [ class "line" ] []
                    , a [ class "connect-item", onClick <| ForSelf <| SelectWalletProvider Ledger ]
                        [ span [ class "icon ledger" ] []
                        , h5 [ class "connect-item-text" ] [ text (Translations.ledger userLanguage) ]
                        , span [ class "arrow big green" ] []
                        ]
                    , div [ class "line" ] []
                    , a [ class "connect-item", onClick <| ForSelf <| SelectWalletProvider Metamask ]
                        [ span [ class "icon metamask" ] []
                        , h5 [ class "connect-item-text" ] [ text (Translations.metamask userLanguage) ]
                        , span [ class "arrow big green" ] []
                        ]
                    ]
                ]

        ChooseLedgerAccount ->
            selectLedgerAddressModal userLanguage model

        LoadingLegerAccounts ->
            connectingModal userLanguage (Just Ledger)

        AttemptingConnectToWallet ->
            connectingModal userLanguage model.selectedProvider

        LedgerConnectionError ->
            ledgerConnectionErrorModal userLanguage model

        WalletConnectedChooseHidden ->
            text ""


connectingModal : Translations.Lang -> Maybe WalletProviderType -> Html Msg
connectingModal userLanguage maybeSelectedProvider =
    let
        ( headerText, instructionsText ) =
            case maybeSelectedProvider of
                Just WalletLink ->
                    ( Translations.follow_wallet_link_instructions userLanguage
                    , Translations.scan_qr_code userLanguage
                    )

                Just Ledger ->
                    ( Translations.plugin_ledger_enter_pin userLanguage
                    , Translations.open_ethereum_application userLanguage
                    )

                _ ->
                    ( Translations.unlock_wallet userLanguage
                    , Translations.click_extension userLanguage
                    )
    in
    div [ class "copy" ]
        [ span [ class "mark" ] []
        , h4 [] [ text headerText ]
        , p [ class "center-text" ] [ text instructionsText ]
        , div [ class "connecting-ring" ]
            [ div [] [] ]
        , div [ class "terms-agreement" ]
            [ p [ class "small" ]
                [ text (Translations.choose_wallet_terms_part1 userLanguage)
                , text " "
                , a [] [ text (Translations.choose_wallet_terms_part2 userLanguage) ]
                ]
            ]
        ]


selectLedgerAddressModal : Translations.Lang -> Model -> Html Msg
selectLedgerAddressModal userLanguage model =
    let
        ( ledgerTypeDropdownActiveClass, ledgerAccountDropdownActiveClass ) =
            case ( model.chooseLedgerAcccountState.pathSelectorActive, model.chooseLedgerAcccountState.addressSelectorActive ) of
                ( True, _ ) ->
                    ( "active", "" )

                ( False, True ) ->
                    ( "", "active" )

                _ ->
                    ( "", "" )

        ( ledgerAccountSelectedString, accountSelectorOptions ) =
            if model.chooseLedgerAcccountState.chooseLedgerInLegacyMode then
                ( Translations.legacy userLanguage, model.chooseLedgerAcccountState.ledgerAccounts.legacyAccounts )

            else
                ( Translations.ledger_live userLanguage, model.chooseLedgerAcccountState.ledgerAccounts.liveAccounts )

        ( accountChosenClass, selectAddressString, selectButton ) =
            case model.chooseLedgerAcccountState.choosenLedgerAccount of
                Just ledgerAccount ->
                    let
                        ledgerAddressPrettyString =
                            case ledgerAccount.accountAddress of
                                Just accountAddress ->
                                    shortenedAddressString 4 4 (getCustomerAddressString accountAddress)

                                Nothing ->
                                    Translations.none userLanguage
                    in
                    ( " chosen"
                    , ledgerAddressPrettyString
                    , button [ class "button main", onClick <| ForSelf <| SelectLedgerAccountFinished ]
                        [ text (Translations.select userLanguage) ]
                    )

                _ ->
                    ( ""
                    , Translations.select_address userLanguage
                    , button [ class "button main", SharedElm.Utils.Markup.disabled ]
                        [ text (Translations.select userLanguage) ]
                    )
    in
    div [ class "copy" ]
        [ span [ class "mark" ] []
        , h4 [] [ text (Translations.select_address userLanguage) ]
        , div [ class "dropdown dropdown--big", onClickStopPropagation <| ForSelf <| ToggleLedgerAccountTypeDropdown ]
            [ div [ class ("dropdown__selected dropdown__selected--light chosen " ++ ledgerTypeDropdownActiveClass) ]
                [ p [ class "small" ] [ text ledgerAccountSelectedString ]
                ]
            , div [ class ("dropdown__options dropdown__options--light " ++ ledgerTypeDropdownActiveClass) ]
                [ div [ class "dropdown__option dropdown__option--light", onClick <| ForSelf <| SelectLedgerAccountType True ]
                    [ p [ class "small" ] [ text (Translations.legacy userLanguage) ]
                    ]
                , div [ class "line" ] []
                , div [ class "dropdown__option dropdown__option--light", onClick <| ForSelf <| SelectLedgerAccountType False ]
                    [ p [ class "small" ] [ text (Translations.ledger_live userLanguage) ]
                    ]
                ]
            ]
        , div [ class "dropdown dropdown--big", onClickStopPropagation <| ForSelf <| ToggleLedgerAccountSelectorDropwdown ]
            [ div [ class ("dropdown__selected dropdown__selected--light " ++ ledgerAccountDropdownActiveClass ++ accountChosenClass) ]
                [ p [ class "small" ] [ text selectAddressString ]
                ]
            , div [ class ("dropdown__options dropdown__options--light " ++ ledgerAccountDropdownActiveClass) ]
                (accountSelectorOptions
                    |> List.map
                        (\ledgerAccount ->
                            let
                                ledgerAddressString =
                                    case ledgerAccount.accountAddress of
                                        Just accountAddress ->
                                            shortenedAddressString 2 4 (getCustomerAddressString accountAddress)

                                        Nothing ->
                                            Translations.none userLanguage

                                ethBalanceString =
                                    case ledgerAccount.ethBalance of
                                        Just ethBalance ->
                                            formatTokenBalanceWithSymbol ethBalance "ETH"

                                        _ ->
                                            Translations.none userLanguage
                            in
                            div [ class "dropdown__option dropdown__option--light", onClick <| ForSelf <| SelectLedgerAccount ledgerAccount ]
                                [ p [ class "small" ] [ text ledgerAddressString ]
                                , p [ class "small align-right" ] [ text ethBalanceString ]
                                ]
                        )
                    |> List.intersperse (div [ class "line" ] [])
                )
            ]
        , selectButton
        ]


ledgerConnectionErrorModal : Translations.Lang -> Model -> Html Msg
ledgerConnectionErrorModal userLanguage model =
    div [ class "copy" ]
        [ span [ class "mark mark--error" ] []
        , h4 [] [ text (Translations.ledger_connection_failed userLanguage) ]
        , div [ class "bullet-points" ]
            [ p [] [ text (Translations.ledger_connection_failed_instruction_1 userLanguage) ]
            , p [] [ text (Translations.ledger_connection_failed_instruction_2 userLanguage) ]
            , p [] [ text (Translations.ledger_connection_failed_instruction_3 userLanguage) ]
            ]
        , div [ class "connect-choices" ]
            [ div [ class "line" ] []
            , a [ class "connect-item", onClick <| ForSelf <| SelectWalletProvider Ledger ]
                [ span [ class "icon ledger" ] []
                , h5 [ class "connect-item-text" ] [ text (Translations.try_again userLanguage) ]
                , span [ class "arrow big green" ] []
                ]
            , div [ class "line" ] []
            ]
        ]



-- Ports


port tryConnect : Bool -> Cmd msg


port chooseProvider : (Bool -> msg) -> Sub msg


port retrieveLedgerAccounts : { derivationPaths : List String } -> Cmd msg


askRetrieveLedgerAccounts : Model -> Cmd msg
askRetrieveLedgerAccounts model =
    let
        allDerivationPaths =
            List.concat
                [ model.chooseLedgerAcccountState.ledgerAccounts.legacyAccounts
                    |> List.map .derivationPath
                , model.chooseLedgerAcccountState.ledgerAccounts.liveAccounts
                    |> List.map .derivationPath
                ]
    in
    retrieveLedgerAccounts { derivationPaths = allDerivationPaths }


port giveLedgerAccount : (Json.Decode.Value -> msg) -> Sub msg


giveLedgerAccountAdddress : (Result Json.Decode.Error LedgerAcccountData -> msg) -> Sub msg
giveLedgerAccountAdddress wrapper =
    let
        decoder =
            Json.Decode.map3 LedgerAcccountData
                (Json.Decode.field "derivationPath" Json.Decode.string)
                (Json.Decode.field "account" (Json.Decode.maybe SharedEth.Decoders.decodeCustomerAddress))
                (Json.Decode.field "ethBalance" (Json.Decode.maybe SharedEth.Decoders.decimal))
    in
    giveLedgerAccount
        (Json.Decode.decodeValue decoder >> wrapper)


port giveRetrievedAllLedgerAccounts : (Json.Decode.Value -> msg) -> Sub msg


giveFinishedLedgerRetrieval : (Result Json.Decode.Error Bool -> msg) -> Sub msg
giveFinishedLedgerRetrieval wrapper =
    let
        decoder =
            Json.Decode.bool
    in
    giveRetrievedAllLedgerAccounts
        (Json.Decode.decodeValue decoder >> wrapper)


selectWalletProvider : Model -> WalletProviderType -> String -> ( Model, Cmd msg )
selectWalletProvider model newProviderType ledgerDerivationPath =
    ( { model | selectedProvider = Just newProviderType, connectionState = Just Connecting }
    , askChangeTrxProvider model newProviderType ledgerDerivationPath
    )


port changeTrxProviderType : { newProviderType : Int, ledgerDerivationPath : String } -> Cmd msg


askChangeTrxProvider : Model -> WalletProviderType -> String -> Cmd msg
askChangeTrxProvider model newProviderType ledgerDerivationPath =
    let
        newProviderTypeId =
            case newProviderType of
                Ledger ->
                    1

                WalletLink ->
                    2

                None ->
                    0

                _ ->
                    3
    in
    changeTrxProviderType { newProviderType = newProviderTypeId, ledgerDerivationPath = ledgerDerivationPath }


port changeNetworkIdPort : { newNetworkId : Int } -> Cmd msg


askChangeNetworkId : Int -> Cmd msg
askChangeNetworkId newNetworkId =
    changeNetworkIdPort { newNetworkId = newNetworkId }


port giveAccountWeb3Port : (Json.Decode.Value -> msg) -> Sub msg


giveAccount : (Result Json.Decode.Error (Maybe CustomerAddress) -> msg) -> Sub msg
giveAccount wrapper =
    let
        decoder =
            Json.Decode.field "account" (Json.Decode.maybe SharedEth.Decoders.decodeCustomerAddress)
    in
    giveAccountWeb3Port
        (Json.Decode.decodeValue decoder >> wrapper)



-- Asking for account Eth balance


port askAccountBalancePort : { customerAddress : String } -> Cmd msg


askAccountBalance : CustomerAddress -> Cmd msg
askAccountBalance (Customer customerAddress) =
    askAccountBalancePort { customerAddress = customerAddress }


port giveAccountBalancePort : (Json.Decode.Value -> msg) -> Sub msg


giveAccountBalance : (Result Json.Decode.Error Decimal -> msg) -> Sub msg
giveAccountBalance wrapper =
    let
        decoder =
            Json.Decode.field "balance" SharedEth.Decoders.decimal
    in
    giveAccountBalancePort
        (Json.Decode.decodeValue decoder >> wrapper)


port setNetworkIdPort : { networkId : Int } -> Cmd msg


port askNetworkPort : {} -> Cmd msg


askNetwork : Cmd msg
askNetwork =
    askNetworkPort {}


port giveNetworkPort : (Json.Decode.Value -> msg) -> Sub msg


giveNetwork : (Result Json.Decode.Error (Maybe Network) -> msg) -> Sub msg
giveNetwork wrapper =
    let
        decoder =
            Json.Decode.field "network" (Json.Decode.maybe (Json.Decode.map networkFromId Json.Decode.int))
    in
    giveNetworkPort
        (Json.Decode.decodeValue decoder >> wrapper)


port giveTrxProviderTypePort : (Json.Decode.Value -> msg) -> Sub msg


providerTypeFromId : Int -> Maybe WalletProviderType
providerTypeFromId id =
    case id of
        0 ->
            Just None

        1 ->
            Just Ledger

        2 ->
            Just WalletLink

        3 ->
            Just OtherWeb3Browser

        _ ->
            Nothing


giveTrxProviderType : (Result Json.Decode.Error (Maybe WalletProviderType) -> msg) -> Sub msg
giveTrxProviderType wrapper =
    let
        decoder =
            Json.Decode.field "providerType" (Json.Decode.map providerTypeFromId Json.Decode.int)
    in
    giveTrxProviderTypePort
        (Json.Decode.decodeValue decoder >> wrapper)



-- Subscriptions


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ giveFinishedLedgerRetrieval (handleError (Json.Decode.errorToString >> (ForSelf << Error)) (ForSelf << LedgerAccountsRetrievalDone))
        , giveLedgerAccountAdddress (handleError (Json.Decode.errorToString >> (ForSelf << Error)) (ForSelf << ReceivedLedgerAccountAddress))
        , giveAccount (handleError (Json.Decode.errorToString >> (ForSelf << Error)) (ForSelf << SetAccount))
        , giveNetwork (handleError (Json.Decode.errorToString >> (ForSelf << Error)) (ForSelf << SetNetwork))
        , giveTrxProviderType (handleError (Json.Decode.errorToString >> (ForSelf << Error)) (ForSelf << SetWalletProvider))
        , chooseProvider (\_ -> ForSelf <| ResetToChooseProvider)
        ]
