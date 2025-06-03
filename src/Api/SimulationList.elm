module Api.SimulationList exposing (getSimulations)

import Http
import Json.Decode


type alias Simulation =
    { id : Int
    }


type alias Error =
    { message : String }


getSimulations :
    { onResponse : Result Http.Error (List Simulation) -> msg
    }
    -> Shared.Model
    -> Cmd msg
getSimulations options shared =
    Http.get
        { url = shared.apiHost ++ "/getSimulations"
        , expect = Http.expectJson options.onResponse decoder
        }


decoder : Json.Decode.Decoder (List Simulation)
decoder =
    Json.Decode.field "results" (Json.Decode.list simulationDecoder)


simulationDecoder : Json.Decode.Decoder Simulation
simulationDecoder =
    Json.Decode.map Simulation
        (Json.Decode.field "id" Json.Decode.int)


handleHttpResponse : Http.Response String -> Result (List Error) (List Simulation)
handleHttpResponse response =
    case response of
        Http.BadUrl_ _ ->
            Err
                [ { message = "Unexpected URL format"
                  }
                ]

        Http.Timeout_ ->
            Err
                [ { message = "Request timed out, please try again"
                  }
                ]

        Http.NetworkError_ ->
            Err
                [ { message = "Could not connect, please try again"
                  }
                ]

        Http.BadStatus_ { statusCode } body ->
            Err
                [ { message = body
                  }
                ]

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok data ->
                    Ok data

                Err _ ->
                    Err
                        [ { message = "Something unexpected happened"
                          }
                        ]
