module Update.Flights exposing (Msg(..), update)

import Browser.Navigation as Nav
import Cloudy
import Cloudy.Client as Client
import Cloudy.Data.LogListPage as LogListPage
import Cloudy.Data.LogbookCategory as LogbookCategory
import Cloudy.Request as Request
import Dict
import Effect exposing (Effect(..))
import Http
import LocalLog exposing (LocalLog)
import LogHelpers
import LogSync
import Model exposing (Model)
import Outbox
import Route exposing (Route(..))
import Session
import Update.Support as Support


type FetchMode
    = Full
    | Since String


type Msg
    = Refresh Int
    | GotPage Int LogbookCategory.LogbookCategory Int FetchMode (Maybe String) (Result Http.Error LogListPage.LogListPage)
    | StartNew Int
    | Edit Int String


update : Msg -> Model -> ( Model, Cmd Msg, List Effect )
update msg model =
    case msg of
        StartNew logbookId ->
            ( model, Cmd.none, [ StartNewLog logbookId ] )

        Edit logbookId clientId ->
            ( model, Cmd.none, [ EditLog logbookId clientId ] )

        Refresh logbookId ->
            case ( model.session, Support.findLogbook model logbookId ) of
                ( Just session, Just book ) ->
                    let
                        mode =
                            if LogSync.isReady model.logSync model.logs logbookId then
                                case LogSync.watermark model.logSync model.logs logbookId of
                                    Just since ->
                                        Since since

                                    Nothing ->
                                        Full

                            else
                                Full
                    in
                    startFetch session book.category mode logbookId model

                _ ->
                    case model.session of
                        Nothing ->
                            ( { model | route = Login }
                            , Nav.pushUrl model.key (Route.toPath Login)
                            , []
                            )

                        Just _ ->
                            ( model, Cmd.none, [] )

        GotPage logbookId category generation mode pageToken result ->
            if generation /= model.logFetchId then
                ( model, Cmd.none, [] )

            else
                case result of
                    Ok page ->
                        let
                            key =
                                String.fromInt logbookId

                            incoming =
                                mergePage category page

                            logs =
                                Dict.insert key (mergeLogs mode pageToken incoming key model.logs) model.logs

                            watermark =
                                List.foldl (\log acc -> LogSync.later acc (LogHelpers.updatedAt log.entry)) model.syncWatermark incoming

                            busy =
                                if pageToken == Nothing then
                                    False

                                else
                                    model.busy

                            loaded =
                                { model | logs = logs, error = Nothing, busy = busy, syncWatermark = watermark }
                        in
                        case page.nextPageToken of
                            Just nextToken ->
                                case model.session of
                                    Just session ->
                                        ( { loaded | logPaging = paging mode logbookId }
                                        , Cmd.batch
                                            [ Support.persistLogs logs
                                            , fetchLogsPage (Session.toClient session) category logbookId generation mode (Just nextToken)
                                            ]
                                        , []
                                        )

                                    Nothing ->
                                        ( { loaded | logPaging = Nothing, logsPartial = Just logbookId }
                                        , Support.persistLogs logs
                                        , []
                                        )

                            Nothing ->
                                let
                                    cursors =
                                        Dict.insert key { complete = True, updatedAt = watermark } model.logSync
                                in
                                ( { loaded | logPaging = Nothing, logsPartial = clearPartial model.logsPartial logbookId, logSync = cursors }
                                , Cmd.batch [ Support.persistLogs logs, Support.persistSync cursors ]
                                , []
                                )

                    Err err ->
                        if Support.unauthorized err then
                            let
                                ( next, cmd ) =
                                    Support.logout { model | busy = False, logPaging = Nothing }
                            in
                            ( next, cmd, [] )

                        else if incrementalRejected mode err then
                            case ( model.session, Support.findLogbook model logbookId ) of
                                ( Just session, Just book ) ->
                                    startFetch session book.category Full logbookId model

                                _ ->
                                    ( model, Cmd.none, [] )

                        else
                            ( { model
                                | busy = False
                                , logPaging = Nothing
                                , syncWatermark = ""
                                , logsPartial =
                                    if pageToken == Nothing || isSince mode then
                                        model.logsPartial

                                    else
                                        Just logbookId
                                , error =
                                    if showsFlights model.route logbookId then
                                        Just (Support.httpErrorMessage "vols" err)

                                    else
                                        model.error
                              }
                            , Cmd.none
                            , []
                            )


startFetch : Session.Session -> LogbookCategory.LogbookCategory -> FetchMode -> Int -> Model -> ( Model, Cmd Msg, List Effect )
startFetch session category mode logbookId model =
    let
        generation =
            model.logFetchId + 1

        key =
            String.fromInt logbookId

        cursors =
            case mode of
                Full ->
                    Dict.insert key { complete = False, updatedAt = "" } model.logSync

                Since _ ->
                    model.logSync

        next =
            { model
                | busy =
                    case mode of
                        Full ->
                            True

                        Since _ ->
                            model.busy
                , logFetchId = generation
                , logPaging = paging mode logbookId
                , logsPartial =
                    case mode of
                        Full ->
                            abandonPaging model logbookId

                        Since _ ->
                            model.logsPartial
                , logSync = cursors
                , syncWatermark =
                    case mode of
                        Full ->
                            ""

                        Since since ->
                            since
            }
    in
    ( next
    , Cmd.batch
        [ case mode of
            Full ->
                Support.persistSync cursors

            Since _ ->
                Cmd.none
        , fetchLogsPage (Session.toClient session) category logbookId generation mode Nothing
        ]
    , []
    )


paging : FetchMode -> Int -> Maybe Int
paging mode logbookId =
    case mode of
        Full ->
            Just logbookId

        Since _ ->
            Nothing


isSince : FetchMode -> Bool
isSince mode =
    case mode of
        Since _ ->
            True

        Full ->
            False


incrementalRejected : FetchMode -> Http.Error -> Bool
incrementalRejected mode err =
    case ( mode, err ) of
        ( Since _, Http.BadStatus code ) ->
            code == 400 || code == 422

        _ ->
            False


mergeLogs : FetchMode -> Maybe String -> List LocalLog -> String -> Dict.Dict String (List LocalLog) -> List LocalLog
mergeLogs mode pageToken incoming key logs =
    case mode of
        Since _ ->
            Outbox.applyServerLogs incoming (Dict.get key logs |> Maybe.withDefault [])

        Full ->
            Support.appendLogPage key (pageToken == Nothing) incoming logs


fetchLogsPage : Client.Client -> LogbookCategory.LogbookCategory -> Int -> Int -> FetchMode -> Maybe String -> Cmd Msg
fetchLogsPage client category logbookId generation mode pageToken =
    Cloudy.listLogs client
        category
        logbookId
        { limit = Just 100
        , pageToken = pageToken
        , dateFrom = Nothing
        , dateTo = Nothing
        , updatedSince =
            case mode of
                Since since ->
                    Just since

                Full ->
                    Nothing
        }
        |> Request.send (GotPage logbookId category generation mode pageToken)


abandonPaging : Model -> Int -> Maybe Int
abandonPaging model logbookId =
    case model.logPaging of
        Just pagingId ->
            if pagingId == logbookId then
                Nothing

            else
                Just pagingId

        Nothing ->
            clearPartial model.logsPartial logbookId


clearPartial : Maybe Int -> Int -> Maybe Int
clearPartial partial logbookId =
    if partial == Just logbookId then
        Nothing

    else
        partial


showsFlights : Route -> Int -> Bool
showsFlights route logbookId =
    case route of
        Logs id ->
            id == logbookId

        Dashboard ->
            True

        EditorNew id ->
            id == logbookId

        EditorEdit id _ ->
            id == logbookId

        _ ->
            False


mergePage : LogbookCategory.LogbookCategory -> LogListPage.LogListPage -> List LocalLog
mergePage category page =
    List.map
        (\entry ->
            { clientId = "s-" ++ String.fromInt (LogHelpers.serverId entry)
            , serverId = Just (LogHelpers.serverId entry)
            , syncStatus = LocalLog.Synced
            , category = category
            , entry = entry
            }
        )
        page.items
