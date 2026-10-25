module Cloudy exposing
    ( createAircraft
    , createLog
    , createLogbook
    , deleteAircraft
    , deleteLog
    , getLog
    , getLogbook
    , getReports
    , listAircraft
    , listLogbooks
    , listLogs
    , patchAircraft
    , patchLog
    , patchLogbook
    , replaceAircraft
    , replaceLog
    , replaceReports
    , token
    )

import Cloudy.Aircraft as AircraftApi
import Cloudy.Auth as AuthApi
import Cloudy.Client exposing (Client)
import Cloudy.Data.Aircraft as Aircraft
import Cloudy.Data.Log as Log exposing (LogEntry)
import Cloudy.Data.LogListPage as LogListPage exposing (LogListPage)
import Cloudy.Data.Logbook as Logbook exposing (Logbook, LogbookCreate, LogbookPatch)
import Cloudy.Data.LogbookCategory exposing (LogbookCategory)
import Cloudy.Data.Report as Report exposing (ReportEntry)
import Cloudy.Data.Token as Token exposing (TokenRequest, TokenResponse)
import Cloudy.Logbooks as Logbooks
import Cloudy.Logs as Logs exposing (ListQuery)
import Cloudy.Reports as Reports
import Cloudy.Request exposing (Request(..))
import Json.Encode as Encode


token : Client -> TokenRequest -> Request TokenResponse
token =
    AuthApi.token


listLogbooks : Client -> Request (List Logbook)
listLogbooks =
    Logbooks.list


getLogbook : Client -> Int -> Request Logbook
getLogbook =
    Logbooks.get


createLogbook : Client -> LogbookCreate -> Request Logbook
createLogbook =
    Logbooks.create


patchLogbook : Client -> Int -> LogbookPatch -> Request Logbook
patchLogbook =
    Logbooks.patch


listLogs : Client -> LogbookCategory -> Int -> ListQuery -> Request LogListPage
listLogs =
    Logs.list


getLog : Client -> LogbookCategory -> Int -> Int -> Request LogEntry
getLog =
    Logs.get


createLog : Client -> Int -> LogEntry -> Request LogEntry
createLog =
    Logs.create


replaceLog : Client -> LogbookCategory -> Int -> Int -> LogEntry -> Request LogEntry
replaceLog =
    Logs.replace


patchLog : Client -> LogbookCategory -> Int -> Int -> Encode.Value -> Request LogEntry
patchLog =
    Logs.patch


deleteLog : Client -> Int -> Int -> Request ()
deleteLog =
    Logs.remove


listAircraft : Client -> Int -> Request (List Aircraft.Aircraft)
listAircraft =
    AircraftApi.list


createAircraft : Client -> Int -> Aircraft.AircraftCreate -> Request Aircraft.Aircraft
createAircraft =
    AircraftApi.create


replaceAircraft : Client -> Int -> Int -> Aircraft.AircraftCreate -> Request Aircraft.Aircraft
replaceAircraft =
    AircraftApi.replace


patchAircraft : Client -> Int -> Int -> Aircraft.AircraftPatch -> Request Aircraft.Aircraft
patchAircraft =
    AircraftApi.patch


deleteAircraft : Client -> Int -> Int -> Request ()
deleteAircraft =
    AircraftApi.remove


getReports : Client -> LogbookCategory -> Int -> Request ReportEntry
getReports =
    Reports.get


replaceReports : Client -> Int -> ReportEntry -> Request ReportEntry
replaceReports =
    Reports.replace
