port module CompoundComponents.Console exposing (error, errorExt, log)

-- Log message to console


port log : String -> Cmd msg


error : String -> Cmd msg
error err =
    log ("Error: " ++ err)


errorExt : String -> String -> Cmd msg
errorExt info err =
    log ("Error " ++ info ++ ": " ++ err)
