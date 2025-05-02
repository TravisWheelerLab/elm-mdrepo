module Pages.Simulation.Id_ exposing (Model, Msg, page)

import Api
import Api.SimulationDetail exposing (Simulation)
import Html exposing (Html)
import Html.Attributes exposing (alt, class, src, style)
import Http
import Page exposing (Page)
import Route.Path
import View exposing (View)



page : { id : String } -> Page Model Msg
page params =
    Page.element
        { init = init params
        , update = update
        , subscriptions = subscriptions
        , view = view params
        }


type alias Model =
    { simulationData : Api.Data Simulation }




-- INIT


init : { id : String } -> ( Model, Cmd Msg )
init params =
    ( { simulationData = Api.Loading }
    , Api.SimulationDetail.get
        { id = params.id
        , onResponse = SimulationApiResponded
        }
    )



-- UPDATE


type Msg
    = SimulationApiResponded (Result Http.Error Simulation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SimulationApiResponded (Ok simulation) ->
            ( { model | simulationData = Api.Success simulation }
            , Cmd.none
            )

        SimulationApiResponded (Err errors) ->
            ( { model | simulationData = Api.Failure errors }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : { id : String } -> Model -> View Msg
view params model =
    { title = "Simulation" ++ params.id
    , body =
        [ Html.div [ class "hero is-danger py-6 has-text-centered" ]
            [ Html.h1 [ class "title is-1" ] [ Html.text params.id ]
            , Html.h2 [ class "subtitle is-6 is-underlined" ]
                [ Html.a [ Route.Path.href Route.Path.Explore ]
                    [ Html.text "Back to Simulation List" ]
                ]
            ]
        , case model.simulationData of
            Api.Loading ->
                Html.div [ class "has-text-centered p-6" ]
                    [ Html.text "Loading..." ]

            Api.Success simulations ->
                viewSimulation simulations

            Api.Failure httpError ->
                let
                    _ =
                        Debug.log "error" httpError
                in
                Html.div [ class "has-text-centered p-6" ]
                    [ Html.text <| Api.toUserFriendlyMessage httpError
                    ]
        ]
    }


viewSimulation : Simulation -> Html msg
viewSimulation simulation =
    Html.div [ class "container p-6 has-text-centered" ]
        [ Html.text ("Simulation  " ++ String.fromInt simulation.id)
        ]
