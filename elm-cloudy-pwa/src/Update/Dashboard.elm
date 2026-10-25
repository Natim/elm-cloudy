module Update.Dashboard exposing (Msg(..), update)

import Browser.Navigation as Nav
import Cloudy
import Cloudy.Data.Logbook as Logbook
import Cloudy.Request as Request
import Effect
import Http
import Model exposing (Model)
import Persisted
import Ports
import Route exposing (Route(..))
import Session
import Update.Support as Support


type Msg
    = RefreshLogbooks
    | GotLogbooks (Result Http.Error (List Logbook.Logbook))
    | LogbookSelected Int
    | NewFlight Int
    | OpenAircraft


update : Msg -> Model -> ( Model, Cmd Msg, List Effect.Effect )
update msg model =
    case msg of
        LogbookSelected _ ->
            ( model, Cmd.none, [] )

        NewFlight logbookId ->
            ( model, Cmd.none, [ Effect.StartNewLog logbookId ] )

        OpenAircraft ->
            ( model, Cmd.none, [ Effect.OpenAircraft ] )

        RefreshLogbooks ->
            case model.session of
                Nothing ->
                    ( { model | route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    , []
                    )

                Just session ->
                    ( { model | busy = True }
                    , Cloudy.listLogbooks (Session.toClient session)
                        |> Request.send GotLogbooks
                    , []
                    )

        GotLogbooks result ->
            case result of
                Ok books ->
                    let
                        selected =
                            case model.selectedLogbookId of
                                Just id ->
                                    if List.any (\book -> book.id == id) books then
                                        Just id

                                    else
                                        List.head books |> Maybe.map .id

                                Nothing ->
                                    List.head books |> Maybe.map .id

                        refresh =
                            case ( model.route, selected ) of
                                ( Dashboard, Just id ) ->
                                    [ Effect.RefreshLogs id, Effect.RefreshAircraft id ]

                                _ ->
                                    []
                    in
                    ( { model | logbooks = books, busy = False, selectedLogbookId = selected }
                    , Ports.storageSaveLogbooks (Persisted.encodeLogbooks books)
                    , refresh
                    )

                Err err ->
                    if Support.unauthorized err then
                        let
                            ( next, cmd ) =
                                Support.logout { model | busy = False }
                        in
                        ( next, cmd, [] )

                    else
                        ( { model | busy = False, error = Just "Impossible de charger les carnets." }, Cmd.none, [] )
