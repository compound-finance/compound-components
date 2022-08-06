module CompoundComponents.Eth.Ethereum exposing
    ( Account(..)
    , AssetAddress(..)
    , ContractAddress(..)
    , CustomerAddress(..)
    , EtherscanLinkValue(..)
    , TrxHash
    , areContractsEqual
    , assetAddressToContractAddress
    , blocksPerDay
    , blocksPerHour
    , contractAddressToAssetAddress
    , etherscanLink
    , etherscanUrl
    , getAssetAddressString
    , getContractAddressString
    , getCustomerAddressString
    , isValidAddress
    , shortenedAddressString
    , zeroAddress
    )

import CompoundComponents.Eth.Network exposing (Network(..))
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), href, target)
import Decimal exposing (Decimal)
import Html exposing (Attribute, Html, a)
import Regex



-- TYPES


type alias TrxHash =
    String


type EtherscanLinkValue
    = TransactionHash TrxHash
    | TokenAddress String
    | EtherscanAddress String


type ContractAddress
    = Contract String


type AssetAddress
    = Asset String


type CustomerAddress
    = Customer String


type Account
    = UnknownAcct
    | NoAccount
    | Acct CustomerAddress (Maybe Decimal) (Maybe String)


--- FUNCTIONS


zeroAddress : String
zeroAddress =
    "0x0000000000000000000000000000000000000000"


isValidAddress : String -> Bool
isValidAddress addressString =
    let
        ethereumAddressRegex =
            Regex.fromString "^0x[a-fA-F0-9]{40}$"
                |> Maybe.withDefault Regex.never
    in
    Regex.contains ethereumAddressRegex addressString


etherscanUrl : Network -> EtherscanLinkValue -> Maybe String
etherscanUrl network urlValue =
    let
        ( linkType, linkValue ) =
            case urlValue of
                TransactionHash suffix ->
                    ( "tx", suffix )

                TokenAddress suffix ->
                    ( "token", suffix )

                EtherscanAddress suffix ->
                    ( "address", suffix )
    in
    case network of
        MainNet ->
            Just ("https://etherscan.io/" ++ linkType ++ "/" ++ linkValue)

        Rinkeby ->
            Just ("https://rinkeby.etherscan.io/" ++ linkType ++ "/" ++ linkValue)

        Kovan ->
            Just ("https://kovan.etherscan.io/" ++ linkType ++ "/" ++ linkValue)

        Ropsten ->
            Just ("https://ropsten.etherscan.io/" ++ linkType ++ "/" ++ linkValue)

        Goerli ->
            Just ("https://goerli.etherscan.io/" ++ linkType ++ "/" ++ linkValue)

        _ ->
            Nothing


etherscanLink : Maybe Network -> EtherscanLinkValue -> List (Attribute msg) -> List (Html msg) -> Html msg
etherscanLink maybeNetwork value attributes =
    case maybeNetwork of
        Just network ->
            case etherscanUrl network value of
                Just etherscanLinkVal ->
                    a (target "_blank" :: href External etherscanLinkVal ++ attributes)

                Nothing ->
                    a attributes

        Nothing ->
            a attributes


blocksPerDay : Int
blocksPerDay =
    floor (24.0 * 60.0 * 60.0 / 15.0)


blocksPerHour : Float -> Int
blocksPerHour n =
    floor (n * 24.0 * 60.0 / 15.0)


assetAddressToContractAddress : AssetAddress -> ContractAddress
assetAddressToContractAddress assetAddress =
    case assetAddress of
        Asset addr ->
            Contract addr


contractAddressToAssetAddress : ContractAddress -> AssetAddress
contractAddressToAssetAddress contractAddress =
    case contractAddress of
        Contract addr ->
            Asset addr


getContractAddressString : ContractAddress -> String
getContractAddressString contractAddress =
    case contractAddress of
        Contract addressString ->
            addressString


getCustomerAddressString : CustomerAddress -> String
getCustomerAddressString customerAddress =
    case customerAddress of
        Customer addressString ->
            addressString


getAssetAddressString : AssetAddress -> String
getAssetAddressString assetAddress =
    case assetAddress of
        Asset addressString ->
            addressString


areContractsEqual : ContractAddress -> ContractAddress -> Bool
areContractsEqual (Contract addressA) (Contract addressB) =
    String.toLower addressA == String.toLower addressB



--- Helpers


shortenedAddressString : Int -> Int -> String -> String
shortenedAddressString numberOfDigitsLeft numberOfDigitsRight address =
    String.left (numberOfDigitsLeft + 2) address ++ "..." ++ String.right numberOfDigitsRight address
