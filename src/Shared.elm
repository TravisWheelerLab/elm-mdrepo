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

import Decoders exposing (userDecoder)
import Effect exposing (Effect)
import Http
import Json.Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (optional)
import Route exposing (Route)
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { apiHost : String
    , mediaHost : String
    , csrfToken : Maybe String
    }


defaultApiHost =
    "http://localhost"


defaultMediaHost =
    "https://assets.mdrepo.org"


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> optional "API_HOST" string defaultApiHost
        |> optional "MEDIA_HOST" string defaultMediaHost
        |> optional "CSRF_TOKEN" (nullable string) Nothing



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        model =
            case flagsResult of
                Ok flags ->
                    { apiHost = flags.apiHost ++ "/api/v1"
                    , loginUrl = flags.apiHost ++ "/api/accounts/orcid/login/"
                    , logoutUrl = flags.apiHost ++ "/api/accounts/logout/"
                    , mediaHost = flags.mediaHost
                    , csrfToken = flags.csrfToken
                    , user = Nothing
                    }

                _ ->
                    { apiHost = defaultApiHost ++ "/api/v1"
                    , loginUrl = defaultApiHost ++ "/api/accounts/orcid/login/"
                    , logoutUrl = defaultApiHost ++ "/api/accounts/logout/"
                    , mediaHost = defaultMediaHost
                    , csrfToken = Nothing
                    , user = Nothing
                    }
    in
    ( model
    , Effect.sendCmd <|
        Http.get
            { url = model.apiHost ++ "/getProfile"
            , expect =
                Http.expectJson
                    Shared.Msg.GotUser
                    (Json.Decode.list userDecoder)
            }
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.Login user ->
            ( { model | user = user }, Effect.none )

        Shared.Msg.Logout ->
            ( { model | user = Nothing }, Effect.none )

        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )

        Shared.Msg.GotUser (Ok users) ->
            let
                user =
                    case List.length users of
                        1 ->
                            List.head users

                        _ ->
                            Nothing
            in
            ( { model | user = user }
            , Effect.none
            )

        Shared.Msg.GotUser (Err _) ->
            ( { model | user = Nothing }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
