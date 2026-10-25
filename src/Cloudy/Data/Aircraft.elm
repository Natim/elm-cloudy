module Cloudy.Data.Aircraft exposing (Aircraft, AircraftCreate, AircraftPatch, decoder, encodeCreate, encodePatch, encodeReplace)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Aircraft =
    { id : Int
    , registration : String
    , aircraftType : Maybe String
    , airport : Maybe String
    , photoUrl : Maybe String
    }


type alias AircraftCreate =
    { registration : String
    , aircraftType : Maybe String
    , airport : Maybe String
    }


type alias AircraftPatch =
    { registration : Maybe String
    , aircraftType : Maybe String
    , airport : Maybe String
    }


decoder : Decoder Aircraft
decoder =
    Decode.map5 Aircraft
        (Decode.field "id" Decode.int)
        (Decode.field "registration" Decode.string)
        (Decode.maybe (Decode.field "aircraft_type" Decode.string))
        (Decode.maybe (Decode.field "airport" Decode.string))
        (Decode.maybe (Decode.field "photo_url" Decode.string))


encodeCreate : AircraftCreate -> Value
encodeCreate =
    encodeReplace


encodeReplace : AircraftCreate -> Value
encodeReplace payload =
    Encode.object
        (List.filterMap identity
            [ Just ( "registration", Encode.string payload.registration )
            , Maybe.map (\v -> ( "aircraft_type", Encode.string v )) payload.aircraftType
            , Maybe.map (\v -> ( "airport", Encode.string v )) payload.airport
            ]
        )


encodePatch : AircraftPatch -> Value
encodePatch payload =
    Encode.object
        (List.filterMap identity
            [ Maybe.map (\v -> ( "registration", Encode.string v )) payload.registration
            , Maybe.map (\v -> ( "aircraft_type", Encode.string v )) payload.aircraftType
            , Maybe.map (\v -> ( "airport", Encode.string v )) payload.airport
            ]
        )
