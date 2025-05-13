module Pages.SignIn exposing (Model, Msg, page)

import Components.Header
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class, href)
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
-- https://orcid.org/signin?client_id=APP-SRX6BHEN9JIX6K7I&redirect_uri=http:%2F%2Fmdrepo.org%2Fapi%2Faccounts%2Forcid%2Flogin%2Fcallback%2F&scope=%2Fauthenticate&response_type=code&state=NUO8LqLcOE6W


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        redirectUrl = "https://test.mdrepo.org/profile"

        orcidUrl =
            "https://sandbox.orcid.org/oauth/authorize?"
                ++ "response_type=token"
                ++ "&redirect_uri=" 
                ++ redirectUrl
                ++ "&client_id="
                ++ Maybe.withDefault "" shared.orcidClientId
                ++ "&scope=/read-limited"

        {-
           "https://sandbox.orcid.org/oauth/authorize?client_id="
                                   ++ Maybe.withDefault "" shared.orcidClientId
                                   ++ "&response_type=code&scope=/read-limited"
                                   ++ "&redirect_uri=http://localhost:1234"
        -}
    in
    Components.Header.view
        { title = "MDRepo - Sign-In"
        , body =
            [ Html.div [ class "container" ]
                [ Html.div [ class "content" ]
                    [ Html.a [ href orcidUrl ] [ Html.text "Login with ORCID" ]
                    ]
                ]
            ]
        }
