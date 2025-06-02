module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Dict
import Effect exposing (Effect)
import Json.Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (optional)
import Regex
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { apiHost : Maybe String
    , mediaHost : Maybe String
    , csrfToken : Maybe String
    , sessionId : Maybe String
    }



--decoder : Json.Decode.Decoder Flags
--decoder =
-- Json.Decode.succeed {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> optional "API_HOST" (nullable string) Nothing
        |> optional "MEDIA_HOST" (nullable string) Nothing
        |> optional "CSRF_TOKEN" (nullable string) Nothing
        |> optional "SESSION_ID" (nullable string) Nothing



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        model =
            case flagsResult of
                Ok flags ->
                    { apiHost = flags.apiHost
                    , mediaHost = flags.mediaHost
                    , csrfToken = flags.csrfToken
                    , sessionId = flags.sessionId
                    , profile = Nothing
                    }

                _ ->
                    { apiHost = Nothing
                    , mediaHost = Nothing
                    , csrfToken = Nothing
                    , sessionId = Nothing
                    , profile = Nothing
                    }
    in
    ( model
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.Login profile ->
            ( { model | profile = profile }, Effect.none )

        Shared.Msg.Logout ->
            ( { model | profile = Nothing }, Effect.none )

        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
