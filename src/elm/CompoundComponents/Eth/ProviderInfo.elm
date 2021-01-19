module CompoundComponents.Eth.ProviderInfo exposing (ProviderType(..), detectProvider, hasProvider)


type ProviderType
    = None
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
