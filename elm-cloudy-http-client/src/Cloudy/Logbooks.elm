module Cloudy.Logbooks exposing (create, get, list, patch)

import Cloudy.Client exposing (Client)
import Cloudy.Data.Logbook as Logbook exposing (Logbook, LogbookCreate, LogbookPatch)
import Cloudy.Request as Request exposing (Request(..))
import Json.Decode as Decode


list : Client -> Request (List Logbook)
list client =
    Request.get client [ "logbooks" ] [] (Decode.list Logbook.decoder)


get : Client -> Int -> Request Logbook
get client logbookId =
    Request.get client [ "logbooks", String.fromInt logbookId ] [] Logbook.decoder


create : Client -> LogbookCreate -> Request Logbook
create client payload =
    Request.post client [ "logbooks" ] (Logbook.encodeCreate payload) Logbook.decoder


patch : Client -> Int -> LogbookPatch -> Request Logbook
patch client logbookId payload =
    Request.patch client [ "logbooks", String.fromInt logbookId ] (Logbook.encodePatch payload) Logbook.decoder
