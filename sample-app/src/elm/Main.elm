port module Main exposing (main)

--import Strings.Translations as Translations

import Browser exposing (Document)
import CompoundComponents.Console as Console
import CompoundComponents.Eth.ConnectedEthWallet as ConnectedEthWallet
import CompoundComponents.Eth.Ethereum as Ethereum exposing (getCustomerAddressString)
import CompoundComponents.Eth.Network exposing (Network(..), networkName)
import CompoundComponents.Functions exposing (handleError)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, style)
import CompoundComponents.Utils.Time
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, h3, h4, label, p, span, text)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Strings.Translations as Translations
import Time
import Url


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { connectedEthWalletModel : ConnectedEthWallet.Model
    , userLanguage : Translations.Lang
    }


type Msg
    = ConnectedEthWalletMsg ConnectedEthWallet.InternalMsg
    | Error String
    | HideAndResetConnectModal Bool
    | ConnectModalBack
    | ShowConnectModal
    | UrlChange (Maybe Url.Url)


connectedEthWalletTranslator : ConnectedEthWallet.Translator Msg
connectedEthWalletTranslator =
    ConnectedEthWallet.translator
        { onInternalMessage = ConnectedEthWalletMsg
        }


init : Flags -> ( Model, Cmd Msg )
init {} =
    let
        ( initConnectedEthWalletModel, initConnectedEthWalletCmd ) =
            ConnectedEthWallet.init False "test"
    in
    ( { connectedEthWalletModel = initConnectedEthWalletModel
      , userLanguage = Translations.En
      }
    , Cmd.batch
        [ Cmd.map connectedEthWalletTranslator initConnectedEthWalletCmd
        , ConnectedEthWallet.tryConnect False
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectedEthWalletMsg connectedEthWalletMsg ->
            let
                ( updatedConnectedEthWalletModel, connectedEthWalletCmd ) =
                    ConnectedEthWallet.update connectedEthWalletMsg model.connectedEthWalletModel
            in
            ( { model
                | connectedEthWalletModel = updatedConnectedEthWalletModel
              }
            , Cmd.batch
                [ Cmd.map connectedEthWalletTranslator connectedEthWalletCmd
                ]
            )

        Error error ->
            ( model, Console.log error )

        HideAndResetConnectModal _ ->
            let
                updatedConnectedEthWalletModel =
                    ConnectedEthWallet.resetModel model.connectedEthWalletModel
            in
            ( { model | connectedEthWalletModel = updatedConnectedEthWalletModel }
            , Cmd.none
            )

        ConnectModalBack ->
            let
                updatedConnectedEthWalletModel =
                    ConnectedEthWallet.handleBack model.connectedEthWalletModel
            in
            ( { model | connectedEthWalletModel = updatedConnectedEthWalletModel }, Cmd.none )

        ShowConnectModal ->
            let
                oldConnectedEthWalletModal =
                    model.connectedEthWalletModel

                updatedConnectedEthWalletModel =
                    { oldConnectedEthWalletModal | chooseWalletState = ConnectedEthWallet.ChooseProvider }
            in
            ( { model
                | connectedEthWalletModel = updatedConnectedEthWalletModel
              }
            , Cmd.none
            )

        UrlChange _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { connectedEthWalletModel, userLanguage } =
    let
        headerHasBack =
            case connectedEthWalletModel.chooseWalletState of
                ConnectedEthWallet.FirstTimeAutoconnecting ->
                    False

                ConnectedEthWallet.ChooseProvider ->
                    False

                ConnectedEthWallet.ChooseLedgerAccount ->
                    True

                ConnectedEthWallet.LoadingLegerAccounts ->
                    True

                ConnectedEthWallet.AttemptingConnectToWallet ->
                    True

                ConnectedEthWallet.LedgerConnectionError ->
                    True

                ConnectedEthWallet.WalletConnectedChooseHidden ->
                    False

        headerView =
            div [ class "header" ]
                ((if headerHasBack then
                    [ a [ class "back-arrow", onClick ConnectModalBack ]
                        [ span [ class "icon" ] []
                        ]
                    ]

                  else
                    []
                 )
                    ++ [ div [ class "close-x" ]
                            [ button [ onClick (HideAndResetConnectModal False) ] []
                            ]
                       ]
                )

        connectedWalletView =
            if connectedEthWalletModel.chooseWalletState /= ConnectedEthWallet.WalletConnectedChooseHidden then
                div [ class "connect-wallet modal" ]
                    [ div [ class "cover active" ] []
                    , div [ class "container-small" ]
                        [ div [ class "accent neutral" ] []
                        , div [ class "legacy-panel" ]
                            [ headerView
                            , Html.map connectedEthWalletTranslator (ConnectedEthWallet.chooseWalletView userLanguage False connectedEthWalletModel)
                            ]
                        ]
                    ]

            else
                let
                    providerString =
                        case connectedEthWalletModel.selectedProvider of
                            Just ConnectedEthWallet.Metamask ->
                                "Metamask"

                            Just ConnectedEthWallet.WalletLink ->
                                "Coinbase"

                            Just ConnectedEthWallet.Ledger ->
                                "Ledger"

                            Just ConnectedEthWallet.OtherWeb3Browser ->
                                "Other"

                            Just ConnectedEthWallet.WalletConnect ->
                                "Wallet Connect"

                            _ ->
                                "None"

                    networkString =
                        connectedEthWalletModel.connectionNetwork
                            |> Maybe.map networkName
                            |> Maybe.withDefault "No Network"

                    connectedAddressString =
                        case connectedEthWalletModel.connectionState of
                            Just (ConnectedEthWallet.ConnectedAcct customerAddress) ->
                                getCustomerAddressString customerAddress

                            Just ConnectedEthWallet.Connecting ->
                                "Still Connecting"

                            _ ->
                                "Disconnected"
                in
                div []
                    [ text "WOOHOO Connected!"
                    , div []
                        [ text ("provider: " ++ providerString) ]
                    , div []
                        [ text ("network: " ++ networkString) ]
                    , div []
                        [ text ("address: " ++ connectedAddressString) ]
                    , button [ onClick ShowConnectModal ]
                        [ text "Show Connect Modal" ]
                    ]
    in
    div [ id "main" ]
        [ connectedWalletView ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onUrlChange (Url.fromString >> UrlChange)
        , Sub.map connectedEthWalletTranslator ConnectedEthWallet.subscriptions
        ]



---- NAVIGATION ----


port onUrlChange : (String -> msg) -> Sub msg
