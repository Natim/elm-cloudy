module Model exposing
    ( EditorState
    , LoginForm
    , Model
    , Msg(..)
    , init
    )

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Cloudy.Data.Log exposing (LogEntry)
import Cloudy.Data.Logbook as Logbook exposing (Logbook)
import Cloudy.Data.LogListPage as LogListPage
import Cloudy.Data.LogbookCategory exposing (LogbookCategory)
import Cloudy.Data.Token as Token exposing (TokenResponse)
import Dict
import Http
import Json.Decode
import LocalLog exposing (LocalLog)
import Outbox exposing (OutboxOp)
import Persisted exposing (LoadedState)
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
    , error : Maybe String
    , busy : Bool
    , storageReady : Bool
    , pendingNew : Maybe ( Int, LogbookCategory )
    }


type alias LoginForm =
    { email : String
    , password : String
    , baseUrl : String
    }


type alias EditorState =
    { logbookId : Int
    , category : LogbookCategory
    , localLog : LocalLog
    }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | StorageLoaded Json.Decode.Value
    | ConnectivityChanged Bool
    | LoginEmailChanged String
    | LoginPasswordChanged String
    | LoginBaseUrlChanged String
    | LoginSubmitted
    | GotToken (Result Http.Error TokenResponse)
    | RefreshLogbooks
    | GotLogbooks (Result Http.Error (List Logbook))
    | SelectLogbook Int
    | BackToLogbooks
    | RefreshLogs Int
    | GotLogsPage Int LogbookCategory (Result Http.Error LogListPage.LogListPage)
    | StartNewLog Int
    | NowReceived String
    | EditLog Int String
    | EditorChanged LogEntry
    | SaveEditor
    | CancelEditor
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
      , outbox = []
      , loginForm = { email = "", password = "", baseUrl = "https://www.cloudy.fr/api/v1" }
      , editor = Nothing
      , error = Nothing
      , busy = False
      , storageReady = False
      , pendingNew = Nothing
      }
    , Ports.storageLoad ()
    )
