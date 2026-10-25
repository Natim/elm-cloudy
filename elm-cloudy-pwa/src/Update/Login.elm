module Update.Login exposing (Msg(..), update)

import Browser.Navigation as Nav
import Cloudy
import Cloudy.Client as Client
import Cloudy.Data.Token exposing (TokenResponse)
import Cloudy.Request as Request
import Effect exposing (Effect(..))
import Http
import Model exposing (Model)
import Ports
import Route exposing (Route(..))
import Session


type Msg
    = EmailChanged String
    | PasswordChanged String
    | BaseUrlChanged String
    | AdvancedToggled Bool
    | Submitted
    | GotToken (Result Http.Error TokenResponse)


update : Msg -> Model -> ( Model, Cmd Msg, List Effect )
update msg model =
    case msg of
        EmailChanged v ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | email = v } }, Cmd.none, [] )

        PasswordChanged v ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | password = v } }, Cmd.none, [] )

        BaseUrlChanged v ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | baseUrl = v } }, Cmd.none, [] )

        AdvancedToggled open ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | advancedOpen = open } }, Cmd.none, [] )

        Submitted ->
            let
                client =
                    { baseUrl = model.loginForm.baseUrl, auth = Client.NoAuth }
            in
            ( { model | busy = True, error = Nothing }
            , Cloudy.token client { email = model.loginForm.email, password = model.loginForm.password }
                |> Request.send GotToken
            , []
            )

        GotToken result ->
            case result of
                Ok token ->
                    let
                        session =
                            { baseUrl = model.loginForm.baseUrl
                            , accessToken = token.accessToken
                            , userId = token.userId
                            , email = model.loginForm.email
                            }
                    in
                    ( { model | session = Just session, busy = False, error = Nothing, route = Dashboard }
                    , Cmd.batch
                        [ Ports.storageSaveSession (Session.encode session)
                        , Nav.pushUrl model.key (Route.toPath Dashboard)
                        ]
                    , [ RefreshLogbooks, FlushOutbox ]
                    )

                Err _ ->
                    ( { model | busy = False, error = Just "Identifiants invalides.", route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    , []
                    )
