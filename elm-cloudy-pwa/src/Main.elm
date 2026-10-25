module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Model exposing (Model, Msg(..), init)
import Ports
import Update exposing (update)
import Url
import View exposing (view)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , view =
            \model ->
                { title = "Cloudy carnet"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.storageLoaded StorageLoaded
        , Ports.connectivityChanged ConnectivityChanged
        , Ports.nowReceived NowReceived
        ]
