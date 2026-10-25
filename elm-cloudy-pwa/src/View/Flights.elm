module View.Flights exposing (view)

import Cloudy.Data.Logbook as LogbookData
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory(..))
import Dict
import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Update.Flights as Flights
import View.Logbook as Logbook


view : Model -> Int -> Html Flights.Msg
view model logbookId =
    let
        logs =
            Dict.get (String.fromInt logbookId) model.logs |> Maybe.withDefault []

        book =
            findBook model logbookId

        category =
            book
                |> Maybe.map .category
                |> Maybe.withDefault
                    (logs
                        |> List.head
                        |> Maybe.map .category
                        |> Maybe.withDefault PlaneDgac
                    )
    in
    div []
        [ div [ class "page-head" ]
            [ div []
                [ h2 [ class "page-title" ]
                    [ text
                        (book
                            |> Maybe.map .name
                            |> Maybe.withDefault ("Carnet " ++ String.fromInt logbookId)
                        )
                    ]
                , p [ class "subtitle" ] [ text "Vols enregistrés dans ce carnet." ]
                ]
            , div [ class "page-actions" ]
                [ button [ type_ "button", class "btn", onClick (Flights.StartNew logbookId) ] [ text "+ Ajouter un vol" ] ]
            ]
        , if List.isEmpty logs then
            div [ class "card" ]
                [ p [] [ text "Aucun vol en cache." ] ]

          else
            Logbook.view category logs (\clientId -> Flights.Edit logbookId clientId)
        ]


findBook : Model -> Int -> Maybe LogbookData.Logbook
findBook model id =
    model.logbooks
        |> List.filter (\book -> book.id == id)
        |> List.head
