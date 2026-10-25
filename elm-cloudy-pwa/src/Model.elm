module Model exposing
    ( AircraftEditor
    , EditorState
    , LoginForm
    , Model
    , init
    )

import Browser.Navigation as Nav
import Cloudy.Data.Aircraft exposing (Aircraft)
import Cloudy.Data.Logbook exposing (Logbook)
import Cloudy.Data.LogbookCategory exposing (LogbookCategory)
import Dict
import File
import LocalLog exposing (LocalLog)
import Outbox exposing (OutboxOp)
import Ports
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , route : Route
    , online : Bool
    , session : Maybe Session
    , logbooks : List Logbook
    , logs : Dict.Dict String (List LocalLog)
    , outbox : List OutboxOp
    , loginForm : LoginForm
    , editor : Maybe EditorState
    , navOpen : Bool
    , selectedLogbookId : Maybe Int
    , aircraft : Dict.Dict String (List Aircraft)
    , aircraftEditor : Maybe AircraftEditor
    , pendingAircraft : Maybe ( Int, Int )
    , error : Maybe String
    , busy : Bool
    , logFetchId : Int
    , logPaging : Maybe Int
    , logsPartial : Maybe Int
    , storageReady : Bool
    , pendingNew : Maybe ( Int, LogbookCategory )
    }


type alias LoginForm =
    { email : String
    , password : String
    , baseUrl : String
    , advancedOpen : Bool
    }


type alias EditorState =
    { logbookId : Int
    , category : LogbookCategory
    , localLog : LocalLog
    , dateDraft : Maybe String
    , uploadingFile : Bool
    , pendingFile : Maybe File.File
    }


type alias AircraftEditor =
    { logbookId : Int
    , aircraftId : Maybe Int
    , registration : String
    , aircraftType : String
    , airport : String
    , confirmingDelete : Bool
    , uploadingPhoto : Bool
    , pendingPhoto : Maybe File.File
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd msg )
init _ url key =
    ( { key = key
      , route = Route.fromUrl url
      , online = True
      , session = Nothing
      , logbooks = []
      , logs = Dict.empty
      , outbox = []
      , loginForm = { email = "", password = "", baseUrl = "https://cloudy.fr/api/v1", advancedOpen = False }
      , editor = Nothing
      , navOpen = False
      , selectedLogbookId = Nothing
      , aircraft = Dict.empty
      , aircraftEditor = Nothing
      , pendingAircraft = Nothing
      , error = Nothing
      , busy = False
      , logFetchId = 0
      , logPaging = Nothing
      , logsPartial = Nothing
      , storageReady = False
      , pendingNew = Nothing
      }
    , Ports.storageLoad ()
    )
