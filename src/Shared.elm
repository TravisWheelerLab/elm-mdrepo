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
    , cookies : Maybe String
    }



--decoder : Json.Decode.Decoder Flags
--decoder =
-- Json.Decode.succeed {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> optional "API_HOST" (nullable string) Nothing
        |> optional "MEDIA_HOST" (nullable string) Nothing
        |> optional "COOKIES" (nullable string) Nothing



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        model =
            case flagsResult of
                Ok flags ->
                    let
                        _ =
                            Debug.log "flags.cookies" flags.cookies

                        semi =
                            Maybe.withDefault Regex.never <| Regex.fromString ";\\s*"

                        equal =
                            Maybe.withDefault Regex.never <| Regex.fromString "="

                        parts =
                            flags.cookies
                                |> Maybe.map (Regex.split semi)
                                |> Maybe.map (List.map (Regex.split semi))

                        --|> Maybe.map (Tuple.pair)
                        --|> Maybe.map Dict.fromList
                        _ =
                            Debug.log "parts" parts
                    in
                    { apiHost = flags.apiHost
                    , mediaHost = flags.mediaHost
                    , csrfToken = Nothing
                    }

                _ ->
                    { apiHost = Nothing
                    , mediaHost = Nothing
                    , csrfToken = Nothing
                    }

        _ =
            Debug.log "init model" model
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
