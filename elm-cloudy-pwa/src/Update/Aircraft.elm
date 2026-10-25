module Update.Aircraft exposing (Msg(..), update)

import Browser.Navigation as Nav
import Cloudy
import Cloudy.Data.Aircraft as AircraftData
import Cloudy.Data.AttachedFile exposing (AttachedFile)
import Cloudy.Request as Request
import Dict
import Effect exposing (Effect(..))
import File
import Http
import Model exposing (AircraftEditor, Model)
import Route exposing (Route(..))
import Session
import Update.Support as Support


type Msg
    = Refresh Int
    | GotList Int (Result Http.Error (List AircraftData.Aircraft))
    | StartNew Int
    | OpenEditor Int Int
    | RegistrationChanged String
    | TypeChanged String
    | AirportChanged String
    | GotPhoto File.File
    | GotPhotoUpload Int Int Bool (Result Http.Error AttachedFile)
    | Save
    | GotSaved Int (Result Http.Error AircraftData.Aircraft)
    | AskDelete
    | Delete
    | GotDeleted Int Int (Result Http.Error ())
    | CancelForm


update : Msg -> Model -> ( Model, Cmd Msg, List Effect )
update msg model =
    case msg of
        Refresh logbookId ->
            case model.session of
                Just session ->
                    ( model
                    , Cloudy.listAircraft (Session.toClient session) logbookId
                        |> Request.send (GotList logbookId)
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        GotList logbookId result ->
            case result of
                Ok list ->
                    let
                        aircraft =
                            Dict.insert (String.fromInt logbookId) list model.aircraft

                        opened =
                            case model.pendingAircraft of
                                Just ( pendingId, aircraftId ) ->
                                    if pendingId == logbookId then
                                        case List.filter (\item -> item.id == aircraftId) list |> List.head of
                                            Just found ->
                                                { editor = Just (editorFrom logbookId found), pending = Nothing, error = Nothing, cmd = Cmd.none }

                                            Nothing ->
                                                { editor = Nothing, pending = Nothing, error = Just "Aéronef introuvable.", cmd = Nav.pushUrl model.key (Route.toPath (Aircraft logbookId)) }

                                    else
                                        { editor = model.aircraftEditor, pending = model.pendingAircraft, error = model.error, cmd = Cmd.none }

                                Nothing ->
                                    { editor = model.aircraftEditor, pending = Nothing, error = model.error, cmd = Cmd.none }
                    in
                    ( { model | aircraft = aircraft, aircraftEditor = opened.editor, pendingAircraft = opened.pending, error = opened.error }, opened.cmd, [] )

                Err err ->
                    if Support.unauthorized err then
                        let
                            ( next, cmd ) =
                                Support.logout { model | busy = False }
                        in
                        ( next, cmd, [] )

                    else
                        ( { model
                            | pendingAircraft = Nothing
                            , error =
                                if aircraftRoute model.route || model.pendingAircraft /= Nothing then
                                    Just (Support.httpErrorMessage "aéronefs" err)

                                else
                                    model.error
                          }
                        , Cmd.none
                        , []
                        )

        StartNew logbookId ->
            ( { model
                | aircraftEditor =
                    Just
                        { logbookId = logbookId
                        , aircraftId = Nothing
                        , registration = ""
                        , aircraftType = ""
                        , airport = ""
                        , confirmingDelete = False
                        , uploadingPhoto = False
                        , pendingPhoto = Nothing
                        }
                , pendingAircraft = Nothing
                , selectedLogbookId = Just logbookId
                , navOpen = False
                , error = Nothing
              }
            , if model.route == AircraftNew logbookId then
                Cmd.none

              else
                Nav.pushUrl model.key (Route.toPath (AircraftNew logbookId))
            , []
            )

        OpenEditor logbookId aircraftId ->
            let
                navigate =
                    if model.route == AircraftEdit logbookId aircraftId then
                        Cmd.none

                    else
                        Nav.pushUrl model.key (Route.toPath (AircraftEdit logbookId aircraftId))
            in
            case findAircraft model logbookId aircraftId of
                Just found ->
                    ( { model | aircraftEditor = Just (editorFrom logbookId found), pendingAircraft = Nothing, selectedLogbookId = Just logbookId, navOpen = False, error = Nothing }
                    , Cmd.batch [ navigate, refreshCmd model logbookId ]
                    , []
                    )

                Nothing ->
                    ( { model | pendingAircraft = Just ( logbookId, aircraftId ), selectedLogbookId = Just logbookId, navOpen = False }
                    , Cmd.batch [ navigate, refreshCmd model logbookId ]
                    , []
                    )

        RegistrationChanged value ->
            updateAircraft (\editor -> { editor | registration = value }) model

        TypeChanged value ->
            updateAircraft (\editor -> { editor | aircraftType = value }) model

        AirportChanged value ->
            updateAircraft (\editor -> { editor | airport = value }) model

        GotPhoto file ->
            case ( model.session, model.aircraftEditor, model.online ) of
                ( Just session, Just editor, True ) ->
                    case editor.aircraftId of
                        Just aircraftId ->
                            ( { model | aircraftEditor = Just { editor | uploadingPhoto = True, pendingPhoto = Nothing }, error = Nothing }
                            , sendAircraftPhoto session editor.logbookId aircraftId file False
                            , []
                            )

                        Nothing ->
                            ( { model | aircraftEditor = Just { editor | pendingPhoto = Just file }, error = Nothing }
                            , Cmd.none
                            , []
                            )

                ( Just _, Just _, False ) ->
                    ( { model | error = Just "Connexion requise pour envoyer une photo." }, Cmd.none, [] )

                _ ->
                    ( model, Cmd.none, [] )

        GotPhotoUpload logbookId aircraftId leaveAfter result ->
            finishAircraftPhoto logbookId aircraftId leaveAfter result model

        Save ->
            case ( model.session, model.aircraftEditor ) of
                ( Just session, Just editor ) ->
                    if String.trim editor.registration == "" then
                        ( { model | error = Just "L'immatriculation est obligatoire." }, Cmd.none, [] )

                    else if not model.online then
                        ( { model | error = Just "Connexion requise pour enregistrer un aéronef." }, Cmd.none, [] )

                    else
                        let
                            payload =
                                { registration = String.trim editor.registration
                                , aircraftType = optionalText editor.aircraftType
                                , airport = optionalText editor.airport
                                }

                            client =
                                Session.toClient session

                            request =
                                case editor.aircraftId of
                                    Nothing ->
                                        Cloudy.createAircraft client editor.logbookId payload

                                    Just aircraftId ->
                                        Cloudy.replaceAircraft client editor.logbookId aircraftId payload
                        in
                        ( { model | busy = True, error = Nothing }
                        , request |> Request.send (GotSaved editor.logbookId)
                        , []
                        )

                _ ->
                    ( model, Cmd.none, [] )

        GotSaved logbookId result ->
            case result of
                Ok aircraft ->
                    let
                        key =
                            String.fromInt logbookId

                        list =
                            Dict.get key model.aircraft |> Maybe.withDefault []

                        aircraftDict =
                            Dict.insert key (upsertAircraft list aircraft) model.aircraft
                    in
                    case ( model.session, model.aircraftEditor |> Maybe.andThen .pendingPhoto ) of
                        ( Just session, Just file ) ->
                            ( { model
                                | aircraft = aircraftDict
                                , busy = True
                                , error = Nothing
                                , aircraftEditor =
                                    Maybe.map
                                        (\editor ->
                                            { editor
                                                | aircraftId = Just aircraft.id
                                                , pendingPhoto = Nothing
                                                , uploadingPhoto = True
                                            }
                                        )
                                        model.aircraftEditor
                              }
                            , sendAircraftPhoto session logbookId aircraft.id file True
                            , []
                            )

                        _ ->
                            ( { model | aircraft = aircraftDict, busy = False, aircraftEditor = Nothing, error = Nothing }
                            , Nav.pushUrl model.key (Route.toPath (Aircraft logbookId))
                            , []
                            )

                Err err ->
                    if Support.unauthorized err then
                        let
                            ( next, cmd ) =
                                Support.logout { model | busy = False }
                        in
                        ( next, cmd, [] )

                    else
                        ( { model | busy = False, error = Just (Support.httpErrorMessage "aéronefs" err) }, Cmd.none, [] )

        AskDelete ->
            case model.aircraftEditor of
                Just editor ->
                    ( { model | aircraftEditor = Just { editor | confirmingDelete = True } }, Cmd.none, [] )

                Nothing ->
                    ( model, Cmd.none, [] )

        Delete ->
            case ( model.session, model.aircraftEditor, model.online ) of
                ( Just session, Just editor, True ) ->
                    case editor.aircraftId of
                        Just aircraftId ->
                            ( { model | busy = True, error = Nothing }
                            , Cloudy.deleteAircraft (Session.toClient session) editor.logbookId aircraftId
                                |> Request.sendEmpty (GotDeleted editor.logbookId aircraftId)
                            , []
                            )

                        Nothing ->
                            ( model, Cmd.none, [] )

                ( Just _, Just _, False ) ->
                    ( { model | error = Just "Connexion requise pour supprimer un aéronef." }, Cmd.none, [] )

                _ ->
                    ( model, Cmd.none, [] )

        CancelForm ->
            case model.aircraftEditor of
                Just editor ->
                    ( { model | aircraftEditor = Nothing }
                    , Nav.pushUrl model.key (Route.toPath (Aircraft editor.logbookId))
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [ OpenAircraft ] )

        GotDeleted logbookId aircraftId result ->
            case result of
                Ok () ->
                    let
                        key =
                            String.fromInt logbookId

                        list =
                            Dict.get key model.aircraft
                                |> Maybe.withDefault []
                                |> List.filter (\item -> item.id /= aircraftId)
                    in
                    ( { model | aircraft = Dict.insert key list model.aircraft, busy = False, aircraftEditor = Nothing, error = Nothing }
                    , Nav.pushUrl model.key (Route.toPath (Aircraft logbookId))
                    , []
                    )

                Err err ->
                    if Support.unauthorized err then
                        let
                            ( next, cmd ) =
                                Support.logout { model | busy = False }
                        in
                        ( next, cmd, [] )

                    else
                        ( { model | busy = False, error = Just (Support.httpErrorMessage "aéronefs" err) }, Cmd.none, [] )


refreshCmd : Model -> Int -> Cmd Msg
refreshCmd model logbookId =
    case model.session of
        Just session ->
            Cloudy.listAircraft (Session.toClient session) logbookId
                |> Request.send (GotList logbookId)

        Nothing ->
            Cmd.none


findAircraft : Model -> Int -> Int -> Maybe AircraftData.Aircraft
findAircraft model logbookId aircraftId =
    Dict.get (String.fromInt logbookId) model.aircraft
        |> Maybe.withDefault []
        |> List.filter (\plane -> plane.id == aircraftId)
        |> List.head


editorFrom : Int -> AircraftData.Aircraft -> AircraftEditor
editorFrom logbookId plane =
    { logbookId = logbookId
    , aircraftId = Just plane.id
    , registration = plane.registration
    , aircraftType = Maybe.withDefault "" plane.aircraftType
    , airport = Maybe.withDefault "" plane.airport
    , confirmingDelete = False
    , uploadingPhoto = False
    , pendingPhoto = Nothing
    }


updateAircraft : (AircraftEditor -> AircraftEditor) -> Model -> ( Model, Cmd Msg, List Effect )
updateAircraft change model =
    ( { model
        | aircraftEditor = Maybe.map (\editor -> change { editor | confirmingDelete = False }) model.aircraftEditor
        , error = Nothing
      }
    , Cmd.none
    , []
    )


optionalText : String -> Maybe String
optionalText raw =
    let
        trimmed =
            String.trim raw
    in
    if String.isEmpty trimmed then
        Nothing

    else
        Just trimmed


upsertAircraft : List AircraftData.Aircraft -> AircraftData.Aircraft -> List AircraftData.Aircraft
upsertAircraft list aircraft =
    aircraft :: List.filter (\item -> item.id /= aircraft.id) list


sendAircraftPhoto : Session.Session -> Int -> Int -> File.File -> Bool -> Cmd Msg
sendAircraftPhoto session logbookId aircraftId file leaveAfter =
    Cloudy.uploadAircraftPhoto (Session.toClient session) logbookId aircraftId file
        |> Request.send (GotPhotoUpload logbookId aircraftId leaveAfter)


finishAircraftPhoto : Int -> Int -> Bool -> Result Http.Error AttachedFile -> Model -> ( Model, Cmd Msg, List Effect )
finishAircraftPhoto logbookId aircraftId leaveAfter result model =
    let
        stopUpload editor =
            { editor | uploadingPhoto = False }
    in
    case result of
        Ok attached ->
            let
                key =
                    String.fromInt logbookId

                list =
                    Dict.get key model.aircraft |> Maybe.withDefault []

                aircraft =
                    Dict.insert key (withPhoto aircraftId attached.url list) model.aircraft

                stillOnForm =
                    case model.aircraftEditor of
                        Just editor ->
                            editor.logbookId == logbookId && editor.aircraftId == Just aircraftId

                        Nothing ->
                            False
            in
            if leaveAfter && stillOnForm then
                ( { model | aircraft = aircraft, busy = False, aircraftEditor = Nothing, error = Nothing }
                , Nav.pushUrl model.key (Route.toPath (Aircraft logbookId))
                , []
                )

            else
                ( { model | aircraft = aircraft, busy = False, aircraftEditor = Maybe.map stopUpload model.aircraftEditor, error = Nothing }
                , Cmd.none
                , []
                )

        Err err ->
            if Support.unauthorized err then
                let
                    ( next, cmd ) =
                        Support.logout { model | busy = False }
                in
                ( next, cmd, [] )

            else
                ( { model | busy = False, aircraftEditor = Maybe.map stopUpload model.aircraftEditor, error = Just "Impossible d'envoyer la photo." }
                , Cmd.none
                , []
                )


withPhoto : Int -> String -> List AircraftData.Aircraft -> List AircraftData.Aircraft
withPhoto aircraftId url list =
    List.map
        (\plane ->
            if plane.id == aircraftId then
                { plane | photoUrl = Just url }

            else
                plane
        )
        list


aircraftRoute : Route -> Bool
aircraftRoute route =
    case route of
        Aircraft _ ->
            True

        AircraftNew _ ->
            True

        AircraftEdit _ _ ->
            True

        _ ->
            False
