module Update.Flights exposing (Msg(..), update)

import Browser.Navigation as Nav
import Cloudy
import Cloudy.Client as Client
import Cloudy.Data.LogListPage as LogListPage
import Cloudy.Data.LogbookCategory as LogbookCategory
import Cloudy.Data.Report exposing (ReportEntry)
import Cloudy.Request as Request
import Dict
import Effect exposing (Effect(..))
import Http
import LocalLog exposing (LocalLog)
import LogHelpers
import Model exposing (Model)
import Route exposing (Route(..))
import Session
import Update.Support as Support


type Msg
    = Refresh Int
    | GotPage Int LogbookCategory.LogbookCategory Int (Maybe String) (Result Http.Error LogListPage.LogListPage)
    | GotReports Int (Result Http.Error ReportEntry)
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
                        client =
                            Session.toClient session

                        generation =
                            model.logFetchId + 1
                    in
                    ( { model
                        | busy = True
                        , logFetchId = generation
                        , logPaging = Just logbookId
                        , logsPartial = abandonPaging model logbookId
                      }
                    , Cmd.batch
                        [ fetchLogsPage client book.category logbookId generation Nothing
                        , Cloudy.getReports client book.category logbookId
                            |> Request.send (GotReports logbookId)
                        ]
                    , []
                    )

                _ ->
                    case model.session of
                        Nothing ->
                            ( { model | route = Login }
                            , Nav.pushUrl model.key (Route.toPath Login)
                            , []
                            )

                        Just _ ->
                            ( model, Cmd.none, [] )

        GotReports logbookId result ->
            case result of
                Ok report ->
                    ( { model | reports = Dict.insert (String.fromInt logbookId) report model.reports }
                    , Cmd.none
                    , []
                    )

                Err _ ->
                    ( model, Cmd.none, [] )

        GotPage logbookId category generation pageToken result ->
            if generation /= model.logFetchId then
                ( model, Cmd.none, [] )

            else
                case result of
                    Ok page ->
                        let
                            key =
                                String.fromInt logbookId

                            logs =
                                Dict.insert key (Support.appendLogPage key (pageToken == Nothing) (mergePage category page) model.logs) model.logs

                            busy =
                                if pageToken == Nothing then
                                    False

                                else
                                    model.busy

                            loaded =
                                { model | logs = logs, error = Nothing, busy = busy }
                        in
                        case page.nextPageToken of
                            Just nextToken ->
                                case model.session of
                                    Just session ->
                                        ( { loaded | logPaging = Just logbookId }
                                        , Cmd.batch
                                            [ Support.persistLogs logs
                                            , fetchLogsPage (Session.toClient session) category logbookId generation (Just nextToken)
                                            ]
                                        , []
                                        )

                                    Nothing ->
                                        ( { loaded | logPaging = Nothing, logsPartial = Just logbookId }
                                        , Support.persistLogs logs
                                        , []
                                        )

                            Nothing ->
                                ( { loaded | logPaging = Nothing, logsPartial = clearPartial model.logsPartial logbookId }
                                , Support.persistLogs logs
                                , []
                                )

                    Err err ->
                        if Support.unauthorized err then
                            let
                                ( next, cmd ) =
                                    Support.logout { model | busy = False, logPaging = Nothing }
                            in
                            ( next, cmd, [] )

                        else
                            ( { model
                                | busy = False
                                , logPaging = Nothing
                                , logsPartial =
                                    if pageToken == Nothing then
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


fetchLogsPage : Client.Client -> LogbookCategory.LogbookCategory -> Int -> Int -> Maybe String -> Cmd Msg
fetchLogsPage client category logbookId generation pageToken =
    Cloudy.listLogs client category logbookId { limit = Just 100, pageToken = pageToken, dateFrom = Nothing, dateTo = Nothing }
        |> Request.send (GotPage logbookId category generation pageToken)


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
