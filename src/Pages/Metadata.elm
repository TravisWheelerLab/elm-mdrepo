module Pages.Metadata exposing (Model, Msg, page)

import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Components.Header
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class, cols, rows, value)
import Html.Events exposing (onClick, onInput)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task exposing (Task)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { tabState : Tab.State
    , simulation : Simulation
    }


defaultSimulation =
    { shortDescription = "" }


initialModel =
    { tabState = Tab.initialState
    , simulation = defaultSimulation
    }


type alias Simulation =
    { shortDescription : String }


init : () -> ( Model, Effect Msg )
init () =
    ( initialModel
    , Effect.none
    )



-- UPDATE


type Msg
    = TabMsg Tab.State
    | UpdateShortDesc String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateShortDesc newDesc ->
            let
                simulation =
                    model.simulation

                newSim =
                    { simulation | shortDescription = newDesc }
            in
            ( { model | simulation = newSim }
            , Effect.none
            )

        TabMsg state ->
            ( { model | tabState = state }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Tab.subscriptions model.tabState TabMsg



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    Components.Header.view
        { title = "Pages.Metadata"
        , body = viewModel shared model
        , shared = shared
        }


viewModel : Shared.Model -> Model -> List (Html.Html Msg)
viewModel shared model =
    [ Html.div [ class "container" ]
        [ Html.h1 [] [ Html.text "Make TOML" ]
        , Tab.config TabMsg
            |> Tab.withAnimation
            |> Tab.center
            |> Tab.items
                [ Tab.item
                    { id = "tabSimulation"
                    , link = Tab.link [] [ Html.text "Simulation" ]
                    , pane =
                        Tab.pane []
                            [ Html.br [] []
                            , paneSimulation shared model
                            ]
                    }
                , Tab.item
                    { id = "tabToml"
                    , link = Tab.link [] [ Html.text "TOML" ]
                    , pane =
                        Tab.pane []
                            [ Html.br [] []
                            , paneToml shared model
                            ]
                    }
                ]
            |> Tab.view model.tabState
        ]
    ]


paneSimulation : Shared.Model -> Model -> Html.Html Msg
paneSimulation shared model =
    Table.table
        { options = []
        , thead = Table.simpleThead []
        , tbody =
            Table.tbody
                []
                [ Table.tr
                    []
                    [ Table.th [] [ Html.text "Short Desc" ]
                    , Table.th []
                        [ Html.input
                            [ onInput UpdateShortDesc
                            , value model.simulation.shortDescription
                            ]
                            []
                        ]
                    ]
                ]
        }


paneToml : Shared.Model -> Model -> Html.Html Msg
paneToml shared model =
    let
        sim =
            model.simulation
    in
    Html.div [ class "content" ]
        [ Html.textarea [ cols 80, rows 20 ]
            [ Html.text <|
                String.join "\n"
                    [ "[initial]"
                    , "short_description = \"" ++ sim.shortDescription ++ "\""
                    ]
            ]
        ]



{-
   [ Html.div []
   ]
-}
