module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.SimulationList
import Components.Header
import Html exposing (Html)
import Html.Attributes exposing (alt, class, href, src, width)
import Http
import Json.Decode exposing (bool)
import Page exposing (Page)
import Route.Path
import String
import View exposing (View)


type alias Model =
    {}


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp



-- SimulationApiResponded (Result (List { message : String }) (List Simulation))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( {}
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Header.view
        { title = "Simulations"
        , body =
            [ Html.text "Hello" ]
        }
