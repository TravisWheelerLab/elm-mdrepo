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

import Effect exposing (Effect)
import Json.Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (optional)
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { orcidClientId : Maybe String
    , orcidClientSecret : Maybe String
    , apiHost : Maybe String
    , mediaHost : Maybe String
    }



--decoder : Json.Decode.Decoder Flags
--decoder =
-- Json.Decode.succeed {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> optional "ORCID_CLIENT_ID" (nullable string) Nothing
        |> optional "ORCID_CLIENT_SECRET" (nullable string) Nothing
        |> optional "API_HOST" (nullable string) Nothing
        |> optional "MEDIA_HOST" (nullable string) Nothing



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        _ =
            Debug.log "FLAGS" flagsResult

        model =
            case flagsResult of
                Ok flags ->
                    { token = Nothing
                    , orcidClientId = flags.orcidClientId
                    , orcidClientSecret = flags.orcidClientSecret
                    , apiHost = flags.apiHost
                    , mediaHost = flags.mediaHost
                    }

                _ ->
                    { token = Nothing
                    , orcidClientId = Nothing
                    , orcidClientSecret = Nothing
                    , apiHost = Nothing
                    , mediaHost = Nothing
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
        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
