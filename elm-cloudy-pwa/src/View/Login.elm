module View.Login exposing (view)

import Html exposing (Html, button, details, div, h1, input, label, p, summary, text)
import Html.Attributes as Attr exposing (class, disabled, type_)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Model exposing (Model)
import Update.Login as Login
import View.Common as Common


view : Model -> Html Login.Msg
view model =
    div [ class "login-screen" ]
        [ div [ class "card login-card" ]
            [ h1 [] [ text "Connexion" ]
            , p [ class "subtitle" ] [ text "Accédez à votre carnet de vol Cloudy." ]
            , div [ class "field" ]
                [ label [] [ text "E-mail" ]
                , input [ Attr.value model.loginForm.email, onInput Login.EmailChanged ] []
                ]
            , div [ class "field" ]
                [ label [] [ text "Mot de passe" ]
                , input [ Attr.type_ "password", Attr.value model.loginForm.password, onInput Login.PasswordChanged ] []
                ]
            , details
                (class "advanced"
                    :: on "toggle" (Decode.at [ "target", "open" ] Decode.bool |> Decode.map Login.AdvancedToggled)
                    :: (if model.loginForm.advancedOpen then
                            [ Attr.attribute "open" "open" ]

                        else
                            []
                       )
                )
                [ summary [] [ text "Paramètres avancés" ]
                , div [ class "field" ]
                    [ label [] [ text "URL de l'API" ]
                    , input [ Attr.value model.loginForm.baseUrl, onInput Login.BaseUrlChanged ] []
                    ]
                ]
            , button [ type_ "button", class "btn", onClick Login.Submitted, disabled model.busy ] [ text "Se connecter" ]
            , Common.maybeError model.error
            , p [ class "hint" ]
                [ text "Les vols saisis hors ligne seront synchronisés au retour du réseau." ]
            ]
        ]
