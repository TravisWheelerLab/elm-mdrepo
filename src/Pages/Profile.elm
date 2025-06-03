module Pages.Profile exposing (Model, Msg, page)

import Auth
import Components.Header
import Decoders exposing (userDecoder)
import Effect exposing (Effect, loadExternalUrl, none)
import Html
import Html.Attributes exposing (class)
import Maybe
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Types exposing (User)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared user route
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
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Auth.User -> Route () -> Model -> View Msg
view shared user route model =
    Components.Header.view
        { title = "MDRepo - User Profile"
        , body =
            [ Html.div
                [ class "container" ]
                [ viewUser user ]
            ]
        , shared = shared
        }


viewUser : User -> Html.Html Msg
viewUser user =
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
