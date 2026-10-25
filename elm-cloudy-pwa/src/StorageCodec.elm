module StorageCodec exposing (decodeLogs, decodeOutbox, encodeLogs, encodeOutbox)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LocalLog
import Outbox


encodeLogs : Dict.Dict String (List LocalLog.LocalLog) -> Value
encodeLogs dict =
    dict
        |> Dict.toList
        |> List.map (\( key, logs ) -> ( key, Encode.list LocalLog.encode logs ))
        |> Encode.object


decodeLogs : Decoder (Dict.Dict String (List LocalLog.LocalLog))
decodeLogs =
    Decode.dict (Decode.list LocalLog.decode)


encodeOutbox : List Outbox.OutboxOp -> Value
encodeOutbox ops =
    Encode.list Outbox.encodeOp ops


decodeOutbox : Decoder (List Outbox.OutboxOp)
decodeOutbox =
    Decode.list Outbox.decodeOp
