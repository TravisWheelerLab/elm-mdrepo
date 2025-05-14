module Pages.Profile exposing (Model, Msg, page)

import Auth
import Components.Header
import Config
import Dict
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required)
import Maybe
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)



{-
      "name": "Ken Youens-Clark",
      "access_token": "57c02336-58ce-4907-90b7-4142dc66838d",
      "expires_in": 631138518,
      "token_type": "bearer",
      "orcid": "0009-0002-5461-8597",
      "scope": "/read-limited",
      "refresh_token": "7c7e2a33-e369-4c2e-9699-92d012eccd0f"


   type alias OrcidToken =
       { name : String
       , accessToken : String
       , expiresIn : Int
       , tokenType : String
       , orcid : String
       , scope : String
       , refreshToken : String
       }


   orcidTokenDecoder : Decoder OrcidToken
   orcidTokenDecoder =
       Decode.succeed OrcidToken
           |> required "name" string
           |> required "access_token" string
           |> required "expires_in" int
           |> required "token_type" string
           |> required "orcid" string
           |> required "scope" string
           |> required "refresh_token" string
-}


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared route
        }



-- INIT


type alias Model =
    { error : Maybe String }


initialModel : Model
initialModel =
    { error = Nothing }


init : () -> ( Model, Effect Msg )
init () =
    {-
       let
           query = route.query

           redirectUrl = "https://test.mdrepo.org/profile"

           effect = case (Dict.get "code" query) of
               Just code -> Effect.sendCmd <|
                   Http.get
                       { url = "https://sandbox.orcid.org/oauth/token?"
                           ++ "code=" ++ code
                           ++ "&redirect_uri=" ++ redirectUrl
                           ++ "&client_id=" ++ Maybe.withDefault "" shared.orcidClientId
                           ++ "&client_secret="
                           ++ Maybe.withDefault "" shared.orcidClientSecret
                           ++ "&scope="
                           ++ "&grant_type=authorization_code"
                       , expect = Http.expectJson GotToken orcidTokenDecoder
                       }

               _ -> Effect.none
       in
    -}
    ( initialModel
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp



--= GotToken (Result Http.Error OrcidToken)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



{-
   GotToken (Ok token) ->
       ( model
       , Effect.none
       )

   GotToken (Err err) ->
       ( { model | error = Just err }
       , Effect.none
       )
-}
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Route () -> Model -> View Msg
view shared route model =
    let
        _ =
            Debug.log "params" route.params

        _ =
            Debug.log "query" route.query
    in
    Components.Header.view
        { title = "MDRepo - User Profile"
        , body =
            [ Html.div
                [ class "container" ]
                [ Html.text <| "Error = " ++ Maybe.withDefault "None" model.error
                ]
            ]
        }
