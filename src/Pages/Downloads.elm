module Pages.Downloads exposing (Model, Msg, page)

import Api
import Bitwise exposing (or)
import Components.Header
import Config
import Decoders exposing (downloadsDecoder)
import Effect exposing (Effect, pushRoutePath)
import Html
import Html.Attributes exposing (class)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Types exposing (DownloadToken, Downloads)
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
    { error : Maybe String
    , downloads : Maybe Downloads
    }


initialModel : Model
initialModel =
    { error = Nothing
    , downloads = Nothing
    }


init : () -> ( Model, Effect Msg )
init () =
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = Config.apiHost ++ "/getUserDownloadTickets"
            , expect = Http.expectJson GotDownloads downloadsDecoder
            }
    )



-- UPDATE


type Msg
    = GotDownloads (Result Http.Error Downloads)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotDownloads (Ok downloads) ->
            ( { model | downloads = Just downloads }
            , Effect.none
            )

        GotDownloads (Err error) ->
            ( { model
                | downloads = Nothing
                , error = Just <| Api.toUserFriendlyMessage error
              }
            , pushRoutePath Route.Path.Profile
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        table =
            case model.downloads of
                Just downloads ->
                    downloadsTable downloads.results

                _ ->
                    Html.text "Unable to get data"
    in
    Components.Header.view
        { title = "Pages.Downloads"
        , body = [ Html.div [ class "container" ] [ table ] ]
        , shared = shared
        }


downloadsTable : List DownloadToken -> Html.Html Msg
downloadsTable downloadTokens =
    let
        mkRow token =
            Html.tr []
                [ Html.td [] [ Html.text token.token ]
                , Html.td [] [ Html.text token.createdAt ]
                , Html.td [] [ Html.text token.status ]
                ]
    in
    case List.length downloadTokens of
        0 ->
            Html.text "No tokens"

        _ ->
            Html.table
                [ class "table" ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "Token" ]
                        , Html.th [] [ Html.text "Created On" ]
                        , Html.th [] [ Html.text "Status" ]
                        ]
                    ]
                , Html.tbody [] (List.map mkRow downloadTokens)
                ]
