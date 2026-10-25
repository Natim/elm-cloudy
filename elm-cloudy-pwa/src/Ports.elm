port module Ports exposing
    ( connectivityChanged
    , nowReceived
    , nowRequest
    , storageLoad
    , storageLoaded
    , storageSaveLogbooks
    , storageSaveLogs
    , storageSaveOutbox
    , storageSaveSession
    )

import Json.Decode
import Json.Encode


port storageLoad : () -> Cmd msg


port storageLoaded : (Json.Decode.Value -> msg) -> Sub msg


port storageSaveSession : Json.Encode.Value -> Cmd msg


port storageSaveLogbooks : Json.Encode.Value -> Cmd msg


port storageSaveLogs : Json.Encode.Value -> Cmd msg


port storageSaveOutbox : Json.Encode.Value -> Cmd msg


port connectivityChanged : (Bool -> msg) -> Sub msg


port nowRequest : () -> Cmd msg


port nowReceived : (String -> msg) -> Sub msg
