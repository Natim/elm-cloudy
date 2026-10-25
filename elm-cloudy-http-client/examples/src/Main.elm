module Main exposing (main)

import Browser
import Cloudy
import Cloudy.Client as Client
import Cloudy.Request as Request
import Html exposing (Html, div, text)
import Http


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { status : String
    }


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init =
    ( { status = "See README: wire Cloudy.token and Cloudy.send in your app." }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text model.status ]
