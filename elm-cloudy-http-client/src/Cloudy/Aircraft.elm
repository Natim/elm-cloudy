module Cloudy.Aircraft exposing (create, list, patch, remove, replace)

import Cloudy.Client exposing (Client)
import Cloudy.Data.Aircraft as Aircraft exposing (Aircraft, AircraftCreate, AircraftPatch)
import Cloudy.Request as Request exposing (Request(..))
import Json.Decode as Decode


list : Client -> Int -> Request (List Aircraft)
list client logbookId =
    Request.get client [ "logbooks", String.fromInt logbookId, "aircraft" ] [] (Decode.list Aircraft.decoder)


create : Client -> Int -> AircraftCreate -> Request Aircraft
create client logbookId payload =
    Request.post client [ "logbooks", String.fromInt logbookId, "aircraft" ] (Aircraft.encodeCreate payload) Aircraft.decoder


replace : Client -> Int -> Int -> AircraftCreate -> Request Aircraft
replace client logbookId aircraftId payload =
    Request.put client [ "logbooks", String.fromInt logbookId, "aircraft", String.fromInt aircraftId ] (Aircraft.encodeReplace payload) Aircraft.decoder


patch : Client -> Int -> Int -> AircraftPatch -> Request Aircraft
patch client logbookId aircraftId payload =
    Request.patch client [ "logbooks", String.fromInt logbookId, "aircraft", String.fromInt aircraftId ] (Aircraft.encodePatch payload) Aircraft.decoder


remove : Client -> Int -> Int -> Request ()
remove client logbookId aircraftId =
    Request.delete client [ "logbooks", String.fromInt logbookId, "aircraft", String.fromInt aircraftId ]
