module Model exposing
    ( AircraftEditor
    , EditorState
    , LoginForm
    , Model
    , Msg(..)
    , init
    )

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Cloudy.Data.Aircraft exposing (Aircraft)
import Cloudy.Data.AttachedFile exposing (AttachedFile)
import Cloudy.Data.Log exposing (LogEntry)
import Cloudy.Data.LogListPage as LogListPage
import Cloudy.Data.Logbook exposing (Logbook)
import Cloudy.Data.LogbookCategory exposing (LogbookCategory)
import Cloudy.Data.Report exposing (ReportEntry)
import Cloudy.Data.Token exposing (TokenResponse)
import Dict
import File
import Http
import Json.Decode
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
    , reports : Dict.Dict String ReportEntry
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


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | StorageLoaded Json.Decode.Value
    | ConnectivityChanged Bool
    | LoginEmailChanged String
    | LoginPasswordChanged String
    | LoginBaseUrlChanged String
    | LoginAdvancedToggled Bool
    | LoginSubmitted
    | GotToken (Result Http.Error TokenResponse)
    | RefreshLogbooks
    | GotLogbooks (Result Http.Error (List Logbook))
    | SelectLogbook Int
    | BackToLogbooks
    | RefreshLogs Int
    | GotLogsPage Int LogbookCategory (Result Http.Error LogListPage.LogListPage)
    | GotReports Int (Result Http.Error ReportEntry)
    | StartNewLog Int
    | NowReceived String
    | EditLog Int String
    | EditorChanged LogEntry
    | DateTextChanged String
    | DatePicked String
    | DateTextBlurred
    | GotLogFile File.File
    | GotLogFileUpload (Result Http.Error AttachedFile)
    | SaveEditor
    | CancelEditor
    | ToggleNav
    | CloseNav
    | MenuLogbookChanged String
    | OpenDashboard
    | OpenFlights
    | OpenAircraft
    | RefreshAircraft Int
    | GotAircraft Int (Result Http.Error (List Aircraft))
    | StartNewAircraft Int
    | OpenAircraftEditor Int Int
    | AircraftRegistrationChanged String
    | AircraftTypeChanged String
    | AircraftAirportChanged String
    | GotAircraftPhoto File.File
    | GotAircraftPhotoUpload Int Int Bool (Result Http.Error AttachedFile)
    | SaveAircraft
    | GotSavedAircraft Int (Result Http.Error Aircraft)
    | AskDeleteAircraft
    | DeleteAircraft
    | GotDeletedAircraft Int Int (Result Http.Error ())
    | FlushOutbox
    | GotOutboxCreate String (Result Http.Error LogEntry)
    | GotOutboxReplace (Result Http.Error LogEntry)
    | GotOutboxDelete (Result Http.Error ())
    | Logout


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , route = Route.fromUrl url
      , online = True
      , session = Nothing
      , logbooks = []
      , logs = Dict.empty
      , reports = Dict.empty
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
      , storageReady = False
      , pendingNew = Nothing
      }
    , Ports.storageLoad ()
    )
