module Cloudy.Request exposing (Request(..), delete, get, patch, post, put, send, sendEmpty)

import Cloudy.Client exposing (Client, authHeader)
import Cloudy.Internal.Url as Url
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type Request a
    = JsonRequest
        { client : Client
        , method : String
        , path : List String
        , query : List ( String, String )
        , body : Http.Body
        , decoder : Decode.Decoder a
        }
    | EmptyRequest
        { client : Client
        , method : String
        , path : List String
        , query : List ( String, String )
        }


send : (Result Http.Error a -> msg) -> Request a -> Cmd msg
send toMsg request =
    case request of
        JsonRequest req ->
            Http.request
                { method = req.method
                , headers = authHeader req.client
                , url = Url.build req.client.baseUrl req.path req.query
                , body = req.body
                , expect = Http.expectJson toMsg req.decoder
                , timeout = Nothing
                , tracker = Nothing
                }

        EmptyRequest _ ->
            Cmd.none


sendEmpty : (Result Http.Error () -> msg) -> Request () -> Cmd msg
sendEmpty toMsg request =
    case request of
        EmptyRequest req ->
            Http.request
                { method = req.method
                , headers = authHeader req.client
                , url = Url.build req.client.baseUrl req.path req.query
                , body = Http.emptyBody
                , expect = Http.expectWhatever toMsg
                , timeout = Nothing
                , tracker = Nothing
                }

        JsonRequest _ ->
            Cmd.none


get : Client -> List String -> List ( String, String ) -> Decode.Decoder a -> Request a
get client path query decoder =
    JsonRequest
        { client = client
        , method = "GET"
        , path = path
        , query = query
        , body = Http.emptyBody
        , decoder = decoder
        }


post : Client -> List String -> Encode.Value -> Decode.Decoder a -> Request a
post client path value decoder =
    JsonRequest
        { client = client
        , method = "POST"
        , path = path
        , query = []
        , body = Http.jsonBody value
        , decoder = decoder
        }


put : Client -> List String -> Encode.Value -> Decode.Decoder a -> Request a
put client path value decoder =
    JsonRequest
        { client = client
        , method = "PUT"
        , path = path
        , query = []
        , body = Http.jsonBody value
        , decoder = decoder
        }


patch : Client -> List String -> Encode.Value -> Decode.Decoder a -> Request a
patch client path value decoder =
    JsonRequest
        { client = client
        , method = "PATCH"
        , path = path
        , query = []
        , body = Http.jsonBody value
        , decoder = decoder
        }


delete : Client -> List String -> Request ()
delete client path =
    EmptyRequest
        { client = client
        , method = "DELETE"
        , path = path
        , query = []
        }
