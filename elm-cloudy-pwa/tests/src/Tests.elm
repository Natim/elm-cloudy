module Tests exposing (all)

import Cloudy.Data.LogbookCategory as LogbookCategory
import Expect
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
import LogbookFormat
import Outbox
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
        ]
