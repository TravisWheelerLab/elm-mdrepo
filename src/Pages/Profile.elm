module Pages.Profile exposing (Model, Msg, page)

--import Shared.Msg
--import Auth

import Api
import Components.Header
import Config
import Decoders exposing (profileDecoder)
import Effect exposing (Effect, loadExternalUrl)
import Html
import Html.Attributes exposing (class)
import Http
import Json.Decode
import Maybe
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Types exposing (Profile)
import View exposing (View)



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
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = Config.apiHost ++ "/getProfile"
            , expect = Http.expectJson GotProfile (Json.Decode.list profileDecoder)
            }
    )



-- UPDATE


type Msg
    = GotProfile (Result Http.Error (List Profile))


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
