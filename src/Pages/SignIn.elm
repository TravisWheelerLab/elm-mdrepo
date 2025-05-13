module Pages.SignIn exposing (Model, Msg, page)

import Components.Header
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class, href)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Url.Parser.Query as Query
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared route
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


view : Shared.Model -> Route () -> Model -> View Msg
view shared route model =
    let
        _ =
            Debug.log "params" route.params

        _ =
            Debug.log "query" route.query

        redirectUrl =
            "https://test.mdrepo.org/profile"

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

           Exchange for token:
           https://sandbox.orcid.org/oauth/token
           code=DmT85J
           &redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground
           &client_id=APP-865IB4OLL27P3AMG
           &client_secret=3c652202-984d-4f62-93c7-e49f1d18f29e
           &scope=
           &grant_type=authorization_code
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
