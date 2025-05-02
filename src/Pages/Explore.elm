module Pages.Explore exposing (Model, Msg, page)

import Api
import Api.SimulationList
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Simulation =
    { id : Int
    }


type alias Model =
    { simulationData : Api.Data (List Simulation)
    }



-- TODO: Effect.sendApiRequest?


init : () -> ( Model, Effect Msg )
init () =
    ( { simulationData = Api.Loading }
    , Effect.sendCmd <|
        Api.SimulationList.getSimulations
            { onResponse = SimulationApiResponded
            }
    )



-- UPDATE


type Msg
    = SimulationApiResponded (Result Http.Error (List Simulation))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SimulationApiResponded (Ok simulations) ->
            ( { model | simulationData = Api.Success simulations }
            , Effect.none
            )

        SimulationApiResponded (Err err) ->
            ( { model | simulationData = Api.Failure err }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Simulations"
    , body =
        [ Html.div [ class "hero is-danger py-6 has-text-centered" ]
            [ Html.h1 [ class "title is-1" ] [ Html.text "MDRepo" ]
            ]
        , case model.simulationData of
            Api.Loading ->
                Html.div [ class "has-text-centered p-6" ]
                    [ Html.text "Loading..." ]

            Api.Success simulations ->
                viewSimulationList simulations

            Api.Failure errors ->
                Html.div [ class "has-text-centered p-6" ]
                    [ Html.text "ermagerd" ]

        --[ Html.text (String.join ", " (List.map (\e -> e.message) errors)) ]
        --[ Html.text (Api.toUserFriendlyMessage httpError) ]
        ]
    }


viewSimulationList : List Simulation -> Html Msg
viewSimulationList simulations =
    Html.div [ class "container py-6 p-5" ]
        [ Html.div [ class "columns is-multiline" ]
            [ Html.ul
                []
                (List.map viewSimulation simulations)
            ]
        ]


viewSimulation : Simulation -> Html Msg
viewSimulation simulation =
    let
        link =
            "Simulation " ++ String.fromInt simulation.id

        route =
            Route.Path.Explore_Id_
                { id = String.fromInt simulation.id
                }
    in
    Html.li [] [ Html.a [ Route.Path.href route ] [ Html.text link ] ]
