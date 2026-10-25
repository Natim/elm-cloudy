module Tests exposing (all)

import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.LogbookCategory as LogbookCategory
import Expect
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
import LogbookFormat
import MediaUrl
import Outbox
import Stats
import Test exposing (Test, describe, test)
import TimeFormat


all : Test
all =
    describe "elm-cloudy-pwa"
        [ describe "TimeFormat"
            [ test "minutes round trip" <|
                \_ ->
                    Expect.equal (Just 90) (TimeFormat.minutesFromHhmm "1:30")
            , test "minutesToHhmm pads" <|
                \_ ->
                    Expect.equal "1:05" (TimeFormat.minutesToHhmm 65)
            , test "ISO date renders as DD/MM/YYYY" <|
                \_ ->
                    Expect.equal "22/10/2026" (TimeFormat.formatDdMmYyyy "2026-10-22T14:12:06.511667+00:00")
            , test "DD/MM/YYYY parses to an ISO day" <|
                \_ ->
                    Expect.equal (Just "2026-10-22") (TimeFormat.parseDdMmYyyy "22/10/2026")
            , test "changing the day keeps the original time" <|
                \_ ->
                    Expect.equal "2026-10-23T14:12:06.511667+00:00" (TimeFormat.applyIsoDate "2026-10-23" "2026-10-22T14:12:06.511667+00:00")
            ]
        , describe "LogbookFormat"
            [ test "short date uses the paper logbook form" <|
                \_ ->
                    Expect.equal "7 sep" (LogbookFormat.shortDate "2026-09-07T12:00:00")
            , test "route replaces nature when both airports are set" <|
                \_ ->
                    Expect.equal "LFKA - LFLV" (LogbookFormat.flightLabel "LFKA" "LFLV" "Nav" "")
            , test "nature is kept when there is no route" <|
                \_ ->
                    Expect.equal "Vol Local Intégration Triple-A" (LogbookFormat.flightLabel "" "" "Vol Local Intégration Triple-A" "")
            , test "zero minutes stay blank" <|
                \_ ->
                    Expect.equal "" (LogbookFormat.showMinutes 0)
            , test "minutes render as H:MM" <|
                \_ ->
                    Expect.equal "1:36" (LogbookFormat.showMinutes 96)
            , test "report totals keep non-zero times" <|
                \_ ->
                    Expect.equal "209:30" (LogbookFormat.showReportTime "209:30")
            , test "zero report totals stay blank" <|
                \_ ->
                    Expect.equal "" (LogbookFormat.showReportTime "0:00")
            ]
        , describe "MediaUrl"
            [ test "aircraft photos use the API host" <|
                \_ ->
                    Expect.equal "https://cloudy.fr/media/upload/PXL_20220105_103509212.jpg"
                        (MediaUrl.absoluteUrl "https://cloudy.fr/api/v1" "/media/upload/PXL_20220105_103509212.jpg")
            , test "absolute photo URLs stay unchanged" <|
                \_ ->
                    Expect.equal "https://cloudy.fr/media/upload/scan.jpg"
                        (MediaUrl.absoluteUrl "https://cloudy.fr/api/v1" "https://cloudy.fr/media/upload/scan.jpg")
            ]
        , describe "Outbox"
            [ test "create success updates matching log" <|
                \_ ->
                    let
                        entry =
                            LogHelpers.emptyEntry LogbookCategory.PlaneDgac 1 "2020-01-01T12:00:00"

                        localLog =
                            { clientId = "c-1"
                            , serverId = Nothing
                            , syncStatus = PendingCreate
                            , category = LogbookCategory.PlaneDgac
                            , entry = entry
                            }

                        updated =
                            Outbox.applyCreateSuccess "c-1" 99 entry [ localLog ]
                    in
                    case List.head updated of
                        Just log ->
                            Expect.all
                                [ \_ -> Expect.equal (Just 99) log.serverId
                                , \_ -> Expect.equal Synced log.syncStatus
                                ]
                                log

                        Nothing ->
                            Expect.fail "missing log"
            , test "dropHead removes first op" <|
                \_ ->
                    Expect.equal
                        []
                        (Outbox.dropHead [ Outbox.DeleteOp { logbookId = 1, serverId = 2 } ])
            ]
        , describe "LogHelpers"
            [ test "selecting a registration fills type and both airports" <|
                \_ ->
                    let
                        filled =
                            LogHelpers.applyRegistration
                                [ { registration = "F-GAAA", aircraftType = Just "DR400", airport = Just "LFPN" } ]
                                "f-gaaa"
                                { registration = "", aircraftType = "", departure = "", arrival = "" }
                    in
                    Expect.all
                        [ \_ -> Expect.equal "F-GAAA" filled.registration
                        , \_ -> Expect.equal "DR400" filled.aircraftType
                        , \_ -> Expect.equal "LFPN" filled.departure
                        , \_ -> Expect.equal "LFPN" filled.arrival
                        ]
                        ()
            , test "an unknown registration leaves type and airports unchanged" <|
                \_ ->
                    let
                        filled =
                            LogHelpers.applyRegistration
                                [ { registration = "F-GAAA", aircraftType = Just "DR400", airport = Just "LFPN" } ]
                                "F-ZZZZ"
                                { registration = "", aircraftType = "C172", departure = "LFPO", arrival = "LFPG" }
                    in
                    Expect.all
                        [ \_ -> Expect.equal "F-ZZZZ" filled.registration
                        , \_ -> Expect.equal "C172" filled.aircraftType
                        , \_ -> Expect.equal "LFPO" filled.departure
                        , \_ -> Expect.equal "LFPG" filled.arrival
                        ]
                        ()
            , test "blank aircraft details do not erase fields already entered" <|
                \_ ->
                    let
                        filled =
                            LogHelpers.applyRegistration
                                [ { registration = "F-GAAA", aircraftType = Nothing, airport = Just "  " } ]
                                "F-GAAA"
                                { registration = "", aircraftType = "C172", departure = "LFPO", arrival = "LFPG" }
                    in
                    Expect.all
                        [ \_ -> Expect.equal "F-GAAA" filled.registration
                        , \_ -> Expect.equal "C172" filled.aircraftType
                        , \_ -> Expect.equal "LFPO" filled.departure
                        , \_ -> Expect.equal "LFPG" filled.arrival
                        ]
                        ()
            ]
        , describe "Stats"
            [ test "block time ignores instrument and groups by aircraft type" <|
                \_ ->
                    let
                        summary =
                            Stats.summarize [ sample 60 "F-GAAA" "DR400", sample 30 "F-GZZZ" "dr400", sample 15 "F-GBBB" "C172" ] Nothing
                    in
                    Expect.all
                        [ \_ -> Expect.equal 3 summary.flights
                        , \_ -> Expect.equal (Just 105) summary.minutes
                        , \_ -> Expect.equal (Just 3) summary.takeoffs
                        , \_ ->
                            case summary.byAircraft of
                                first :: second :: [] ->
                                    Expect.all
                                        [ \_ -> Expect.equal "DR400" first.aircraftType
                                        , \_ -> Expect.equal 2 first.flights
                                        , \_ -> Expect.equal 90 first.minutes
                                        , \_ -> Expect.equal "C172" second.aircraftType
                                        ]
                                        ()

                                _ ->
                                    Expect.fail "expected two aircraft types"
                        ]
                        summary
            ]
        ]


sample : Int -> String -> String -> LocalLog
sample minutes registration aircraftType =
    let
        entry =
            case LogHelpers.emptyEntry LogbookCategory.PlaneDgac 1 "2020-01-01T12:00:00" of
                PlaneDgacEntry log ->
                    PlaneDgacEntry
                        { log
                            | crew_day_minutes = minutes
                            , aircraft_registration = registration
                            , aircraft_type = aircraftType
                            , takeoff_count = 1
                            , landing_count = 1
                            , instrument_pilot_minutes = 15
                        }

                other ->
                    other
    in
    { clientId = "c-" ++ registration
    , serverId = Just 1
    , syncStatus = Synced
    , category = LogbookCategory.PlaneDgac
    , entry = entry
    }
