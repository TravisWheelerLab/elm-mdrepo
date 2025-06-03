module Pages.Profile exposing (Model, Msg, page)

--import Shared.Msg
--import Auth

import Api
import Auth
import Components.Header
import Config
import Decoders exposing (userDecoder)
import Effect exposing (Effect, loadExternalUrl, none)
import Html
import Html.Attributes exposing (class)
import Http
import Json.Decode
import Maybe
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Types exposing (User)
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
    , user : Maybe User
    }


initialModel : Model
initialModel =
    { error = Nothing
    , user = Nothing
    }


init : () -> ( Model, Effect Msg )
init () =
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = Config.apiHost ++ "/getProfile"
            , expect = Http.expectJson GotUser (Json.Decode.list userDecoder)
            }
    )



-- UPDATE


type Msg
    = GotUser (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotUser (Ok users) ->
            let
                user =
                    case List.length users of
                        1 ->
                            List.head users

                        _ ->
                            Nothing
            in
            ( { model | user = user }
            , Effect.login user
            )

        GotUser (Err error) ->
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
    Components.Header.view
        { title = "MDRepo - User Profile"
        , body =
            [ Html.div
                [ class "container" ]
                [ Html.text <| "Error = " ++ Maybe.withDefault "None" model.error
                , viewUser model.user
                ]
            ]
        , shared = shared
        }


viewUser : Maybe User -> Html.Html Msg
viewUser curUser =
    case curUser of
        Nothing ->
            Html.div [] []

        Just user ->
            Html.div [ class "box" ]
                [ Html.table [ class "table" ]
                    [ Html.thead [] []
                    , Html.tbody []
                        [ Html.tr []
                            [ Html.th [] [ Html.text "Name" ]
                            , Html.td [] [ Html.text user.fullName ]
                            ]
                        , Html.tr []
                            [ Html.th [] [ Html.text "Institution" ]
                            , Html.td [] [ Html.text <| Maybe.withDefault "NA" user.institution ]
                            ]
                        , Html.tr []
                            [ Html.th [] [ Html.text "Email" ]
                            , Html.td [] [ Html.text user.email ]
                            ]
                        , Html.tr []
                            [ Html.th [] [ Html.text "ORCID" ]
                            , Html.td [] [ Html.text <| Maybe.withDefault "NA" user.orcid ]
                            ]
                        , Html.tr []
                            [ Html.th [] [ Html.text "Can Contribute" ]
                            , Html.td []
                                [ Html.text <|
                                    if Maybe.withDefault False user.canContribute then
                                        "Yes"

                                    else
                                        "No"
                                ]
                            ]
                        ]
                    ]
                ]
