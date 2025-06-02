module Pages.About exposing (Model, Msg, page)

import Components.Header
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class)
import Page exposing (Page)
import Route exposing (Route)
import Shared
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
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    Components.Header.view
        { title = "MDRepo - About"
        , body =
            [ Html.div [ class "card" ]
                [ Html.header [ class "card-header" ]
                    [ Html.p [ class "card-header-title" ] [ Html.text "Acknowledgements" ]
                    , Html.div [ class "card-content" ]
                        [ Html.div
                            [ class "content" ]
                            [ Html.text
                                """
                                We thank the University of Arizona Research, Innovation & Impact (RII) 
                                for supporting development of MD Repo through BIO5 and IT4IR TRIF Funds. MD Repo 
                                would not be possible without the capacity building infrastructure available through 
                                CyVerse, ACCESS, and JetStream2
                                """
                            ]
                        ]
                    ]
                ]
            ]
        , shared = shared
        }
