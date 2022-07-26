module CompoundComponents.Eth.ProviderInfo exposing (ProviderType(..), detectProvider, hasProvider)


type ProviderType
    = None
    | BraveWallet
    | Tally
    | CoinbaseMobile
    | MetaMask
    | MetaMaskMobile
    | ImToken
    | Unknown


detectProvider : String -> ProviderType
detectProvider providerType =
    case providerType of
        "none" ->
            None

        "tally" ->
            Tally
        
        "brave_wallet" ->
            BraveWallet

        "coinbase_mobile" ->
            CoinbaseMobile

        "meta_mask" ->
            MetaMask

        "meta_mask_mobile" ->
            MetaMaskMobile

        "im_token" ->
            ImToken

        _ ->
            Unknown


hasProvider : ProviderType -> Bool
hasProvider providerType =
    providerType /= None
