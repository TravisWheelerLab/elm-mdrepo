module Pages.Profile exposing (Model, Msg, page)

--import Shared.Msg
--import Auth

import Api
import Components.Header
import Config
import Effect exposing (Effect, loadExternalUrl)
import Html
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, bool, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Maybe
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Types exposing (Profile)
import View exposing (View)



{-
   {
     "first_name": "Ken",
     "last_name": "Youens-Clark",
     "full_name": "Ken Youens-Clark",
     "email": "kyclark@arizona.edu",
     "institution": null,
     "is_superuser": true,
     "is_staff": true,
     "orcid": "0000-0001-9961-144X",
     "can_contribute": false
   }
-}


profileDecoder : Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> required "first_name" string
        |> required "last_name" string
        |> required "full_name" string
        |> required "email" string
        |> optional "institution" (nullable string) Nothing
        |> optional "is_superuser" (nullable bool) Nothing
        |> optional "is_staff" (nullable bool) Nothing
        |> optional "orcid" (nullable string) Nothing
        |> optional "can_contribute" (nullable bool) Nothing



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
-- page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
-- page user shared route =


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
    { error : Maybe String
    , profile : Maybe Profile
    }


initialModel : Model
initialModel =
    { error = Nothing
    , profile = Nothing
    }


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
    , Effect.sendCmd <|
        Http.get
            { url = Config.apiHost ++ "/getProfile"
            , expect = Http.expectJson GotProfile (Decode.list profileDecoder)
            }
    )



-- UPDATE


type Msg
    = GotProfile (Result Http.Error (List Profile))



--= GotToken (Result Http.Error OrcidToken)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotProfile (Ok profiles) ->
            let
                profile =
                    case List.length profiles of
                        1 ->
                            List.head profiles

                        _ ->
                            Nothing
            in
            ( { model | profile = profile }
            , Effect.login profile
            )

        GotProfile (Err error) ->
            ( { model
                | error = Just <| Api.toUserFriendlyMessage error
              }
            , loadExternalUrl Config.loginUrl
            )



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
        profile =
            case model.profile of
                Just p ->
                    viewProfile p

                _ ->
                    Html.div [] []
    in
    Components.Header.view
        { title = "MDRepo - User Profile"
        , body =
            [ Html.div
                [ class "container" ]
                [ Html.text <| "Error = " ++ Maybe.withDefault "None" model.error
                , profile
                ]
            ]
        , shared = shared
        }


viewProfile : Profile -> Html.Html Msg
viewProfile profile =
    Html.div [ class "box" ]
        [ Html.table [ class "table" ]
            [ Html.thead [] []
            , Html.tbody []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Name" ]
                    , Html.td [] [ Html.text profile.fullName ]
                    ]
                , Html.tr []
                    [ Html.th [] [ Html.text "Institution" ]
                    , Html.td [] [ Html.text <| Maybe.withDefault "NA" profile.institution ]
                    ]
                , Html.tr []
                    [ Html.th [] [ Html.text "Email" ]
                    , Html.td [] [ Html.text profile.email ]
                    ]
                , Html.tr []
                    [ Html.th [] [ Html.text "ORCID" ]
                    , Html.td [] [ Html.text <| Maybe.withDefault "NA" profile.orcid ]
                    ]
                , Html.tr []
                    [ Html.th [] [ Html.text "Can Contribute" ]
                    , Html.td []
                        [ Html.text <|
                            if Maybe.withDefault False profile.canContribute then
                                "Yes"

                            else
                                "No"
                        ]
                    ]
                ]
            ]
        ]
