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
            , test "create success drops the server copy of the new flight" <|
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

                        serverCopy =
                            { clientId = "s-99"
                            , serverId = Just 99
                            , syncStatus = Synced
                            , category = LogbookCategory.PlaneDgac
                            , entry = entry
                            }

                        updated =
                            Outbox.applyCreateSuccess "c-1" 99 entry [ serverCopy, localLog ]
                    in
                    Expect.equal [ "c-1" ] (List.map .clientId updated)
            , test "a pending update is not listed twice when the server page arrives" <|
                \_ ->
                    let
                        entry =
                            LogHelpers.emptyEntry LogbookCategory.PlaneDgac 1 "2020-01-01T12:00:00"

                        pending =
                            { clientId = "s-7"
                            , serverId = Just 7
                            , syncStatus = PendingUpdate
                            , category = LogbookCategory.PlaneDgac
                            , entry = entry
                            }

                        serverCopy =
                            { pending | syncStatus = Synced }

                        other =
                            { clientId = "s-8"
                            , serverId = Just 8
                            , syncStatus = Synced
                            , category = LogbookCategory.PlaneDgac
                            , entry = entry
                            }

                        merged =
                            Outbox.mergeServerPage True [ serverCopy, other ] [ pending ]
                    in
                    Expect.equal [ ( Just 7, PendingUpdate ), ( Just 8, Synced ) ]
                        (List.map (\log -> ( log.serverId, log.syncStatus )) merged)
            , test "replace success keeps a single synced flight" <|
                \_ ->
                    let
                        entry =
                            case LogHelpers.emptyEntry LogbookCategory.PlaneDgac 1 "2020-01-01T12:00:00" of
                                PlaneDgacEntry log ->
                                    PlaneDgacEntry { log | id = 7 }

                                other ->
                                    other

                        pending =
                            { clientId = "s-7"
                            , serverId = Just 7
                            , syncStatus = PendingUpdate
                            , category = LogbookCategory.PlaneDgac
                            , entry = entry
                            }

                        serverCopy =
                            { pending | clientId = "s-7b", syncStatus = Synced }

                        updated =
                            Outbox.applyReplaceSuccess 7 entry [ pending, serverCopy ]
                    in
                    case updated of
                        [ log ] ->
                            Expect.all
                                [ \_ -> Expect.equal Synced log.syncStatus
                                , \_ -> Expect.equal (Just 7) log.serverId
                                ]
                                log

                        _ ->
                            Expect.fail "expected one flight"
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
                            Stats.summarize [ sample 60 "F-GAAA" "DR400", sample 30 "F-GZZZ" "dr400", sample 15 "F-GBBB" "C172" ]
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
                        , \_ -> Expect.equal [ ( "Équipage jour", "1:45" ), ( "Instrument", "0:45" ) ] summary.details
                        , \_ -> Expect.equal [] summary.byClass
                        ]
                        summary
            , test "splits FCL time between SEP and MEP" <|
                \_ ->
                    let
                        summary =
                            Stats.summarize
                                [ fclSample 60 40 20 True False
                                , fclSample 30 30 0 False True
                                , fclSample 15 0 15 False False
                                ]
                    in
                    Expect.all
                        [ \_ ->
                            Expect.equal
                                [ { label = "Temps de vol", sep = 60, mep = 30 }
                                , { label = "Pilote seul", sep = 60, mep = 30 }
                                , { label = "Commandant de bord", sep = 40, mep = 30 }
                                , { label = "Double commande", sep = 20, mep = 0 }
                                ]
                                summary.byClass
                        , \_ -> Expect.equal 15 summary.unclassifiedMinutes
                        ]
                        ()
            , test "splits DGAC single-engine and multi-engine time" <|
                \_ ->
                    Stats.summarize [ dgacClassSample ]
                        |> .byClass
                        |> Expect.equal
                            [ { label = "Temps de vol", sep = 60, mep = 30 }
                            , { label = "Double commande", sep = 40, mep = 10 }
                            , { label = "Commandant de bord", sep = 20, mep = 10 }
                            , { label = "Copilote", sep = 0, mep = 10 }
                            , { label = "Nuit", sep = 0, mep = 15 }
                            ]
            , test "keeps short types when there are ten or fewer" <|
                \_ ->
                    Stats.shownAircraft (List.map (\n -> typed 60 (String.fromInt n)) (List.range 1 10))
                        |> List.length
                        |> Expect.equal 10
            , test "hides types under five hours when there are more than ten" <|
                \_ ->
                    let
                        rows =
                            typed 300 "DR400"
                                :: typed 299 "C172"
                                :: List.map (\n -> typed 10 ("T" ++ String.fromInt n)) (List.range 1 9)
                    in
                    Stats.shownAircraft rows
                        |> List.map .aircraftType
                        |> Expect.equal [ "DR400" ]
            , test "last complete month is the previous calendar month" <|
                \_ ->
                    let
                        today =
                            { year = 2026, month = 11, day = 8 }
                    in
                    Expect.all
                        [ \_ ->
                            Expect.equal "Octobre 2026" (Stats.periodText Stats.LastCompleteMonth today)
                        , \_ ->
                            Stats.inPeriod Stats.LastCompleteMonth today monthWindow
                                |> List.map (.entry >> LogHelpers.flightDate)
                                |> Expect.equal [ "2026-10-01T08:00:00", "2026-10-31T18:00:00" ]
                        ]
                        ()
            , test "January uses the previous December" <|
                \_ ->
                    Expect.equal "Décembre 2025"
                        (Stats.periodText Stats.LastCompleteMonth { year = 2026, month = 1, day = 3 })
            , test "a leap-year February stays complete" <|
                \_ ->
                    Stats.inPeriod Stats.LastCompleteMonth
                        { year = 2024, month = 3, day = 1 }
                        [ dated "2024-02-29T12:00:00", dated "2024-03-01T12:00:00" ]
                        |> List.map (.entry >> LogHelpers.flightDate)
                        |> Expect.equal [ "2024-02-29T12:00:00" ]
            , test "the last 12 months include today and the same day a year earlier" <|
                \_ ->
                    let
                        today =
                            { year = 2026, month = 11, day = 8 }
                    in
                    Expect.all
                        [ \_ ->
                            Expect.equal "8 nov. 2025 – 8 nov. 2026" (Stats.periodText Stats.Last12Months today)
                        , \_ ->
                            Stats.inPeriod Stats.Last12Months
                                today
                                [ dated "2025-11-07T12:00:00", dated "2025-11-08T12:00:00", dated "2026-11-08T12:00:00", dated "2026-11-09T12:00:00" ]
                                |> List.map (.entry >> LogHelpers.flightDate)
                                |> Expect.equal [ "2025-11-08T12:00:00", "2026-11-08T12:00:00" ]
                        ]
                        ()
            , test "the last 24 months clamp a missing February 29" <|
                \_ ->
                    Expect.equal "28 fév. 2022 – 29 fév. 2024"
                        (Stats.periodText Stats.Last24Months { year = 2024, month = 2, day = 29 })
            , test "all keeps every flight" <|
                \_ ->
                    Stats.inPeriod Stats.All { year = 2026, month = 11, day = 8 } monthWindow
                        |> List.length
                        |> Expect.equal 4
            ]
        ]


typed : Int -> String -> Stats.AircraftTotal
typed minutes aircraftType =
    { aircraftType = aircraftType
    , flights = 1
    , minutes = minutes
    }


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


fclSample : Int -> Int -> Int -> Bool -> Bool -> LocalLog
fclSample total pic dual se me =
    let
        entry =
            case LogHelpers.emptyEntry LogbookCategory.PlaneFcl1080 1 "2020-01-01T12:00:00" of
                PlaneFcl1080Entry log ->
                    PlaneFcl1080Entry
                        { log
                            | total_time_flight = total
                            , single_pilot_time = total
                            , pilot_time_in_cmd = pic
                            , pilot_time_dual = dual
                            , single_pilot_se = se
                            , single_pilot_me = me
                        }

                other ->
                    other
    in
    { clientId = "fcl-" ++ String.fromInt total
    , serverId = Just 1
    , syncStatus = Synced
    , category = LogbookCategory.PlaneFcl1080
    , entry = entry
    }


dgacClassSample : LocalLog
dgacClassSample =
    let
        entry =
            case LogHelpers.emptyEntry LogbookCategory.PlaneDgac 1 "2020-01-01T12:00:00" of
                PlaneDgacEntry log ->
                    PlaneDgacEntry
                        { log
                            | single_engine_day_dual_minutes = 40
                            , single_engine_day_captain_minutes = 20
                            , multi_engine_day_captain_minutes = 10
                            , multi_engine_day_co_pilot_minutes = 5
                            , multi_engine_night_dual_minutes = 10
                            , multi_engine_night_co_pilot_minutes = 5
                        }

                other ->
                    other
    in
    { clientId = "dgac-class"
    , serverId = Just 1
    , syncStatus = Synced
    , category = LogbookCategory.PlaneDgac
    , entry = entry
    }


dated : String -> LocalLog
dated flightDate =
    { clientId = flightDate
    , serverId = Just 1
    , syncStatus = Synced
    , category = LogbookCategory.PlaneDgac
    , entry = LogHelpers.emptyEntry LogbookCategory.PlaneDgac 1 flightDate
    }


monthWindow : List LocalLog
monthWindow =
    [ dated "2026-09-30T12:00:00"
    , dated "2026-10-01T08:00:00"
    , dated "2026-10-31T18:00:00"
    , dated "2026-11-01T08:00:00"
    ]
