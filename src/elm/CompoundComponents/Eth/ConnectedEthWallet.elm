port module CompoundComponents.Eth.ConnectedEthWallet exposing
    ( ChooseWalletState(..)
    , ConnectionState(..)
    , InternalMsg(..)
    , LedgerAcccountData
    , Model
    , Translator
    , WalletProviderType(..)
    , chooseWalletView
    , giveAccount
    , giveFinishedLedgerRetrieval
    , giveLedgerAccountAdddress
    , giveTrxProviderType
    , handleBack
    , init
    , resetAndChooseProvider
    , resetModel
    , subscriptions
    , translator
    , tryConnect
    , update
    )

import CompoundComponents.Console as Console
import CompoundComponents.Eth.Decoders
import CompoundComponents.Eth.Ethereum exposing (Account(..), CustomerAddress(..), getCustomerAddressString, shortenedAddressString)
import CompoundComponents.Eth.Network exposing (Network, networkFromId, networkId)
import CompoundComponents.Eth.ProviderInfo as EthProviderInfo
import CompoundComponents.Functions exposing (handleError)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, onClickStopPropagation)
import CompoundComponents.Utils.Markup
import CompoundComponents.Utils.NumberFormatter exposing (formatTokenBalanceWithSymbol)
import Decimal exposing (Decimal)
import Html exposing (Html, a, button, div, h3, h4, h5, p, span, text)
import Html.Events exposing (onClick)
import Json.Decode
import Strings.Translations as Translations


type WalletProviderType
    = Metamask
    | WalletLink
    | Ledger
    | OtherWeb3Browser
    | WalletConnect
    | None


type ConnectionState
    = Disconnected
    | Connecting
    | ConnectedAcct CustomerAddress


type ChooseWalletState
    = FirstTimeAutoconnecting
    | ChooseProvider
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
    , providerType : EthProviderInfo.ProviderType
    , ledgerWalletConnectForceRopsten : Bool
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
    | RequestShowTerms
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
        | chooseWalletState = WalletConnectedChooseHidden
        , chooseLedgerAcccountState = resetLedgerState
    }


resetAndChooseProvider : Model -> Model
resetAndChooseProvider model =
    { model
        | chooseWalletState = ChooseProvider
        , chooseLedgerAcccountState = resetLedgerState
    }


handleBack : Model -> Model
handleBack model =
    let
        updatedChooseWalletState =
            if
                model.chooseWalletState
                    == AttemptingConnectToWallet
                    || model.chooseWalletState
                    == ChooseLedgerAccount
                    || model.chooseWalletState
                    == LedgerConnectionError
            then
                ChooseProvider

            else
                model.chooseWalletState
    in
    { model | chooseWalletState = updatedChooseWalletState }


init : Bool -> String -> ( Model, Cmd Msg )
init ledgerWalletForceRopsten providerTypeString =
    let
        newEmptyModel =
            { chooseWalletState = FirstTimeAutoconnecting
            , selectedProvider = Just None
            , connectionState = Nothing
            , connectionNetwork = Nothing
            , chooseLedgerAcccountState = resetLedgerState
            , providerType = EthProviderInfo.detectProvider providerTypeString
            , ledgerWalletConnectForceRopsten = ledgerWalletForceRopsten
            , errors = []
            }
    in
    ( newEmptyModel
    , Cmd.none
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
                        selectWalletProvider model walletProvider "" model.ledgerWalletConnectForceRopsten

                updatedChooseWalletState =
                    case ( model.chooseWalletState, walletProvider ) of
                        ( ChooseProvider, Ledger ) ->
                            LoadingLegerAccounts

                        ( LedgerConnectionError, Ledger ) ->
                            LoadingLegerAccounts

                        ( ChooseProvider, _ ) ->
                            AttemptingConnectToWallet

                        _ ->
                            updatedModel.chooseWalletState
            in
            ( { updatedModel | chooseWalletState = updatedChooseWalletState }, connectedWalletCmd )

        -- This can be called from autoconnect
        SetWalletProvider walletProvider ->
            ( { model | selectedProvider = walletProvider }, Cmd.none )

        SetAccount Nothing ->
            --TODO:
            ( model, Cmd.none )

        SetAccount (Just newAccount) ->
            let
                updatedSelectedProvider =
                    case ( model.selectedProvider, model.providerType ) of
                        ( Nothing, EthProviderInfo.MetaMask ) ->
                            Just Metamask

                        ( Just None, EthProviderInfo.MetaMask ) ->
                            Just Metamask

                        ( Nothing, _ ) ->
                            Just OtherWeb3Browser

                        ( Just None, _ ) ->
                            Just OtherWeb3Browser

                        _ ->
                            model.selectedProvider

                updatedConnectionState =
                    Just (ConnectedAcct newAccount)

                updatedChooseWalletState =
                    if model.chooseWalletState == AttemptingConnectToWallet || model.chooseWalletState == ChooseLedgerAccount then
                        WalletConnectedChooseHidden

                    else
                        model.chooseWalletState
            in
            ( { model
                | chooseWalletState = updatedChooseWalletState
                , connectionState = updatedConnectionState
                , selectedProvider = updatedSelectedProvider
              }
            , Cmd.none
            )

        SetNetwork network ->
            ( { model | connectionNetwork = network }, Cmd.none )

        ReceivedLedgerAccountAddress accountData ->
            let
                oldLedgerAccounts =
                    model.chooseLedgerAcccountState.ledgerAccounts

                updatedLegacyAccounts =
                    oldLedgerAccounts.legacyAccounts
                        |> List.map
                            (\legacyAccount ->
                                if legacyAccount.derivationPath == accountData.derivationPath then
                                    accountData

                                else
                                    legacyAccount
                            )

                updatedLiveAccounts =
                    oldLedgerAccounts.liveAccounts
                        |> List.map
                            (\liveAccount ->
                                if liveAccount.derivationPath == accountData.derivationPath then
                                    accountData

                                else
                                    liveAccount
                            )

                updatedLedgerAccounts =
                    { oldLedgerAccounts | legacyAccounts = updatedLegacyAccounts, liveAccounts = updatedLiveAccounts }

                oldLedgerAccountState =
                    model.chooseLedgerAcccountState

                updatedChooseLedgerAcccountState =
                    { oldLedgerAccountState | ledgerAccounts = updatedLedgerAccounts }
            in
            ( { model | chooseLedgerAcccountState = updatedChooseLedgerAcccountState }, Cmd.none )

        LedgerAccountsRetrievalDone success ->
            if model.chooseWalletState == LoadingLegerAccounts then
                if success then
                    ( { model | chooseWalletState = ChooseLedgerAccount }, Cmd.none )

                else
                    ( { model | chooseWalletState = LedgerConnectionError }, Cmd.none )

            else
                ( model, Cmd.none )

        ToggleLedgerAccountTypeDropdown ->
            let
                oldChooseLedgerAccountState =
                    model.chooseLedgerAcccountState

                updatedPathSelectorActive =
                    not oldChooseLedgerAccountState.pathSelectorActive

                updatedAddressSelectorActive =
                    if updatedPathSelectorActive then
                        False

                    else
                        oldChooseLedgerAccountState.addressSelectorActive

                updatedChooseLedgerAccountState =
                    { oldChooseLedgerAccountState | pathSelectorActive = updatedPathSelectorActive, addressSelectorActive = updatedAddressSelectorActive }
            in
            ( { model | chooseLedgerAcccountState = updatedChooseLedgerAccountState }, Cmd.none )

        ToggleLedgerAccountSelectorDropwdown ->
            let
                oldChooseLedgerAccountState =
                    model.chooseLedgerAcccountState

                updatedAddressSelectorActive =
                    not oldChooseLedgerAccountState.addressSelectorActive

                updatedChooseLedgerAccountState =
                    { oldChooseLedgerAccountState | addressSelectorActive = updatedAddressSelectorActive }
            in
            ( { model | chooseLedgerAcccountState = updatedChooseLedgerAccountState }, Cmd.none )

        SelectLedgerAccountType useLegacy ->
            let
                oldChooseLedgerAccountState =
                    model.chooseLedgerAcccountState

                updatedChooseLedgerAccountState =
                    { oldChooseLedgerAccountState | chooseLedgerInLegacyMode = useLegacy }
            in
            ( { model | chooseLedgerAcccountState = updatedChooseLedgerAccountState }, Cmd.none )

        SelectLedgerAccount ledgerAccount ->
            let
                updatedChooseLedgerAccountState =
                    let
                        oldChooseLedgerAccountState =
                            model.chooseLedgerAcccountState
                    in
                    { oldChooseLedgerAccountState | choosenLedgerAccount = Just ledgerAccount }
            in
            ( { model | chooseLedgerAcccountState = updatedChooseLedgerAccountState }, Cmd.none )

        SelectLedgerAccountFinished ->
            let
                ( updatedModel, ledgerCmd ) =
                    case ( model.chooseWalletState, model.chooseLedgerAcccountState.choosenLedgerAccount ) of
                        ( ChooseLedgerAccount, Just ledgerAccount ) ->
                            selectWalletProvider model Ledger ledgerAccount.derivationPath model.ledgerWalletConnectForceRopsten

                        _ ->
                            ( model, Cmd.none )

                updatedChooseLedgerAccountState =
                    let
                        oldChooseLedgerModalState =
                            model.chooseLedgerAcccountState
                    in
                    { oldChooseLedgerModalState | pathSelectorActive = False, addressSelectorActive = False }
            in
            ( { updatedModel | chooseLedgerAcccountState = updatedChooseLedgerAccountState }, ledgerCmd )

        ResetToChooseProvider ->
            ( { model | chooseWalletState = ChooseProvider }
            , Cmd.none
            )

        -- This can be used by parents to show an appropriate page by app.
        RequestShowTerms ->
            ( model, Cmd.none )

        Error error ->
            ( { model | errors = model.errors }, Console.log error )



-- Views


inCopyBackArrow : Bool -> Html Msg
inCopyBackArrow isCompoundChain =
    if isCompoundChain then
        div [ class "connect-wallet-copy__back-arrow-holder" ]
            [ div [ class "connect-wallet-copy__back-arrow-holder__back-arrow", onClick <| ForSelf <| ResetToChooseProvider ]
                []
            ]

    else
        text ""


markSpan : Bool -> Html Msg
markSpan isCompoundChain =
    if isCompoundChain then
        text ""

    else
        span [ class "connect-wallet-copy__mark" ] []


termsView : Translations.Lang -> Bool -> Html Msg
termsView userLanguage isCompoundChain =
    if isCompoundChain then
        text ""

    else
        div [ class "terms-agreement" ]
            [ p [ class "small" ]
                [ text (Translations.choose_wallet_terms_part1 userLanguage)
                , text " "
                , a [ onClick <| ForSelf <| RequestShowTerms ] [ text (Translations.choose_wallet_terms_part2 userLanguage) ]
                ]
            ]


connectItemView : Translations.Lang -> Bool -> WalletProviderType -> Html Msg
connectItemView userLanguage isCompoundChain providerType =
    let
        connectItemClass =
            if isCompoundChain then
                "connect-item connect-item--box-border"

            else
                "connect-item"

        ( iconClass, itemText ) =
            case providerType of
                WalletLink ->
                    ( " connect-wallet-icon--coinbase"
                    , Translations.coinbase_wallet userLanguage
                    )

                Ledger ->
                    ( " connect-wallet-icon--ledger"
                    , Translations.ledger userLanguage
                    )

                WalletConnect ->
                    ( " connect-wallet-icon--wallet-connect"
                    , Translations.wallet_connect userLanguage
                    )

                _ ->
                    ( " connect-wallet-icon--metamask"
                    , Translations.metamask userLanguage
                    )
    in
    a [ class connectItemClass, onClick <| ForSelf <| SelectWalletProvider providerType ]
        [ span [ class ("connect-wallet-icon" ++ iconClass) ] []
        , h5 [ class "connect-item-text" ] [ text itemText ]
        , span [ class "arrow big green" ] []
        ]


chooseWalletView : Translations.Lang -> Bool -> Model -> Html Msg
chooseWalletView userLanguage isCompoundChain ({ chooseWalletState } as model) =
    let
        walletCopy =
            case chooseWalletState of
                ChooseProvider ->
                    let
                        { headerDescriptions, lineDivider } =
                            if isCompoundChain then
                                { headerDescriptions =
                                    [ div [ class "connect-wallet-copy__no-panel-header" ]
                                        [ p [] [ text (Translations.get_started userLanguage) ]
                                        , h3 [ class "connect-wallet-copy__no-panel-header__title" ] [ text (Translations.connect_wallet userLanguage) ]
                                        ]
                                    ]
                                , lineDivider = []
                                }

                            else
                                { headerDescriptions =
                                    [ h4 [] [ text (Translations.connect_wallet userLanguage) ]
                                    , p [ class "center-text" ] [ text (Translations.to_start_using_compound userLanguage) ]
                                    ]
                                , lineDivider = [ div [ class "line" ] [] ]
                                }

                        ledgerItem =
                            if isCompoundChain then
                                []

                            else
                                [ connectItemView userLanguage isCompoundChain Ledger ]
                                    ++ lineDivider

                        coinbaseWalletItem =
                            if isCompoundChain then
                                []

                            else
                                [ div [ class "line" ] []
                                , connectItemView userLanguage isCompoundChain WalletLink
                                ]
                    in
                    div [ class "connect-wallet-copy connect-wallet-copy--small-top" ]
                        ([ markSpan isCompoundChain
                         ]
                            ++ headerDescriptions
                            ++ [ div [ class "connect-choices" ]
                                    ([ connectItemView userLanguage isCompoundChain Metamask
                                     ]
                                        ++ lineDivider
                                        ++ ledgerItem
                                        ++ [ connectItemView userLanguage isCompoundChain WalletConnect
                                           ]
                                        ++ coinbaseWalletItem
                                    )
                               ]
                            ++ [ termsView userLanguage isCompoundChain ]
                        )

                ChooseLedgerAccount ->
                    selectLedgerAddressModal userLanguage isCompoundChain model

                LoadingLegerAccounts ->
                    connectingModal userLanguage (Just Ledger) isCompoundChain

                AttemptingConnectToWallet ->
                    connectingModal userLanguage model.selectedProvider isCompoundChain

                LedgerConnectionError ->
                    ledgerConnectionErrorModal userLanguage isCompoundChain model

                WalletConnectedChooseHidden ->
                    text ""

                FirstTimeAutoconnecting ->
                    text ""
    in
    div []
        [ walletCopy
        ]


connectingModal : Translations.Lang -> Maybe WalletProviderType -> Bool -> Html Msg
connectingModal userLanguage maybeSelectedProvider isCompoundChain =
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

        showBorderClass =
            if isCompoundChain then
                " connect-wallet-copy--show-border"

            else
                ""
    in
    div [ class ("connect-wallet-copy" ++ showBorderClass) ]
        [ inCopyBackArrow isCompoundChain
        , markSpan isCompoundChain
        , h4 [] [ text headerText ]
        , p [] [ text instructionsText ]
        , div [ class "connecting-ring" ]
            [ div [] [] ]
        , termsView userLanguage isCompoundChain
        ]


selectLedgerAddressModal : Translations.Lang -> Bool -> Model -> Html Msg
selectLedgerAddressModal userLanguage isCompoundChain model =
    let
        showBorderClass =
            if isCompoundChain then
                " connect-wallet-copy--show-border"

            else
                ""

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
                    , button [ class "button main", CompoundComponents.Utils.Markup.disabled ]
                        [ text (Translations.select userLanguage) ]
                    )
    in
    div [ class ("connect-wallet-copy" ++ showBorderClass) ]
        [ inCopyBackArrow isCompoundChain
        , markSpan isCompoundChain
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
        , termsView userLanguage isCompoundChain
        ]


ledgerConnectionErrorModal : Translations.Lang -> Bool -> Model -> Html Msg
ledgerConnectionErrorModal userLanguage isCompoundChain model =
    let
        showBorderClass =
            if isCompoundChain then
                " connect-wallet-copy--show-border"

            else
                ""
    in
    div [ class ("connect-wallet-copy" ++ showBorderClass) ]
        [ inCopyBackArrow isCompoundChain
        , span [ class "connect-wallet-copy__mark connect-wallet-copy__mark--error" ] []
        , h4 [] [ text (Translations.ledger_connection_failed userLanguage) ]
        , div [ class "bullet-points" ]
            [ p [] [ text (Translations.ledger_connection_failed_instruction_1 userLanguage) ]
            , p [] [ text (Translations.ledger_connection_failed_instruction_2 userLanguage) ]
            , p [] [ text (Translations.ledger_connection_failed_instruction_3 userLanguage) ]
            ]
        , div [ class "connect-choices" ]
            [ div [ class "line" ] []
            , a [ class "connect-item", onClick <| ForSelf <| SelectWalletProvider Ledger ]
                [ span [ class "connect-wallet-icon connect-wallet-icon--ledger" ] []
                , h5 [ class "connect-item-text" ] [ text (Translations.try_again userLanguage) ]
                , span [ class "arrow big green" ] []
                ]
            , div [ class "line" ] []
            ]
        , termsView userLanguage isCompoundChain
        ]



-- Ports


port tryConnect : Bool -> Cmd msg


port chooseProvider : (Bool -> msg) -> Sub msg


port retrieveLedgerAccounts : { derivationPaths : List String, ledgerConnectRopsten : Bool } -> Cmd msg


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
    retrieveLedgerAccounts { derivationPaths = allDerivationPaths, ledgerConnectRopsten = model.ledgerWalletConnectForceRopsten }


port giveLedgerAccount : (Json.Decode.Value -> msg) -> Sub msg


giveLedgerAccountAdddress : (Result Json.Decode.Error LedgerAcccountData -> msg) -> Sub msg
giveLedgerAccountAdddress wrapper =
    let
        decoder =
            Json.Decode.map3 LedgerAcccountData
                (Json.Decode.field "derivationPath" Json.Decode.string)
                (Json.Decode.field "account" (Json.Decode.maybe CompoundComponents.Eth.Decoders.decodeCustomerAddress))
                (Json.Decode.field "ethBalance" (Json.Decode.maybe CompoundComponents.Eth.Decoders.decimal))
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


selectWalletProvider : Model -> WalletProviderType -> String -> Bool -> ( Model, Cmd msg )
selectWalletProvider model newProviderType ledgerDerivationPath ledgerWalletForceRopsten =
    ( { model | selectedProvider = Just newProviderType, connectionState = Just Connecting }
    , askChangeTrxProvider model newProviderType ledgerDerivationPath ledgerWalletForceRopsten
    )


port changeTrxProviderType : { newProviderType : Int, ledgerDerivationPath : String, ledgerWalletConnectRopsten : Bool } -> Cmd msg


askChangeTrxProvider : Model -> WalletProviderType -> String -> Bool -> Cmd msg
askChangeTrxProvider model newProviderType ledgerDerivationPath ledgerWalletForceRopsten =
    let
        newProviderTypeId =
            case newProviderType of
                Ledger ->
                    1

                WalletLink ->
                    2

                WalletConnect ->
                    4

                None ->
                    0

                _ ->
                    3
    in
    changeTrxProviderType { newProviderType = newProviderTypeId, ledgerDerivationPath = ledgerDerivationPath, ledgerWalletConnectRopsten = ledgerWalletForceRopsten }


port changeNetworkIdPort : { newNetworkId : Int } -> Cmd msg


askChangeNetworkId : Int -> Cmd msg
askChangeNetworkId newNetworkId =
    changeNetworkIdPort { newNetworkId = newNetworkId }


port giveAccountWeb3Port : (Json.Decode.Value -> msg) -> Sub msg


giveAccount : (Result Json.Decode.Error (Maybe CustomerAddress) -> msg) -> Sub msg
giveAccount wrapper =
    let
        decoder =
            Json.Decode.field "account" (Json.Decode.maybe CompoundComponents.Eth.Decoders.decodeCustomerAddress)
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
            Json.Decode.field "balance" CompoundComponents.Eth.Decoders.decimal
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

        4 ->
            Just WalletConnect

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
