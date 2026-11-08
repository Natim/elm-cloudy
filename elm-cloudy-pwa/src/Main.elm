module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Model exposing (Model)
import Ports
import Update exposing (Msg(..), requestClock, update)
import Update.Editor as Editor
import Update.Support as Support
import Url exposing (Url)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , view =
            \model ->
                { title = windowTitle model
                , body = [ view model ]
                }
        , update = update
        , subscriptions = subscriptions
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( model, cmd ) =
            Model.init flags url key
    in
    ( model, Cmd.batch [ cmd, requestClock ] )


windowTitle : Model -> String
windowTitle model =
    case Support.chosenLogbook model model.route |> Maybe.andThen (Support.findLogbook model) of
        Just book ->
            "Carnet de vol — Cloudy — " ++ book.name

        Nothing ->
            "Carnet de vol — Cloudy"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.storageLoaded StorageLoaded
        , Ports.connectivityChanged ConnectivityChanged
        , Ports.nowReceived (EditorMsg << Editor.NowReceived)
        ]
