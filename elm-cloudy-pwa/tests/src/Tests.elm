module Tests exposing (all)

import Cloudy.Data.LogbookCategory as LogbookCategory
import Expect
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
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
