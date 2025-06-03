module Pages.Uploads exposing (Model, Msg, page)

import Api
import Components.Header
import Config
import Decoders exposing (uploadTicketsResultDecoder)
import Effect exposing (Effect, pushRoutePath)
import Html
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Types exposing (UploadTicketsResult)
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
    , uploadTickets : Maybe UploadTicketsResult
    }


initialModel : Model
initialModel =
    { error = Nothing
    , uploadTickets = Nothing
    }


init : () -> ( Model, Effect Msg )
init () =
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = Config.apiHost ++ "/getUserDownloadTickets"
            , expect = Http.expectJson GotUploadTickets uploadTicketsResultDecoder
            }
    )



-- UPDATE


type Msg
    = GotUploadTickets (Result Http.Error UploadTicketsResult)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotUploadTickets (Ok result) ->
            ( { model | uploadTickets = Just result }
            , Effect.none
            )

        GotUploadTickets (Err error) ->
            ( { model
                | uploadTickets = Nothing
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
        _ =
            Debug.log "model" model
    in
    Components.Header.view
        { title = "Pages.Uploads"
        , body = [ Html.text "/uploads" ]
        , shared = shared
        }
