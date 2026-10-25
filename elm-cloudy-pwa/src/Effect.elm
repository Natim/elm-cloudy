module Effect exposing (Effect(..))

import Cloudy.Data.Log exposing (LogEntry)


type Effect
    = RefreshLogbooks
    | FlushOutbox
    | RefreshLogs Int
    | RefreshAircraft Int
    | StartNewLog Int
    | EditLog Int String
    | StartNewAircraft Int
    | OpenAircraftEditor Int Int
    | OpenAircraft
