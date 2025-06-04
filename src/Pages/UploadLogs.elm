module Pages.UploadLogs exposing (Model, Msg, page)

import Api
import Components.Header
import Decoders exposing (userDecoder)
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Types exposing (User)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { userUploadTicketsResult : Maybe UserUploadTicketsResult
    , showDialog : Bool
    , numberUploads : Int
    , uploadToken : Maybe UploadToken
    , isAdminToken : Bool
    }


initialModel : Model
initialModel =
    { userUploadTicketsResult = Nothing
    , showDialog = False
    , numberUploads = 1
    , uploadToken = Nothing
    , isAdminToken = False
    }


type alias UploadToken =
    { token : String
    , orcid : String
    }


type alias UserUploadTicketsResult =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List UserUploadTicket
    }


type alias UserUploadTicket =
    { id : Int
    , token : String
    , nSubmissions : Int
    , createdBy : Int
    , orcid : String
    , createdAt : String
    , status : String
    , simulations : List UserUploadSimulation
    , ticketType : String
    , uploadInstances : List UserUploadInstance
    , statusMessages : List UserUploadStatusMessage
    , successful : Bool
    , leadContributorOrcid : String
    , filenames : String
    }


type alias UserUploadStatusMessage =
    { timestamp : String
    , message : String
    , isError : Bool
    , isWarning : Bool
    }


type alias UserUploadInstance =
    { id : Int
    , user : User
    , ticket : Int
    , createdOn : String
    , simulation : UserUploadSimulation
    }


type alias UserUploadSimulation =
    { mdRepoId : String
    , guid : String
    , description : Maybe String
    , slug : String
    , isPlaceholder : Bool
    , isDeprecated : Bool
    , creationDate : String
    , uploadedFiles : List UploadedFile
    }


type alias UploadedFile =
    { id : Int
    , isPrimary : Bool
    , fileName : String
    , fileType : String
    , description : String
    , fileSizeBytes : String
    }


decodeRequest : Decoder UserUploadTicketsResult
decodeRequest =
    Decode.succeed UserUploadTicketsResult
        |> required "count" int
        |> required "next" (nullable string)
        |> required "previous" (nullable string)
        |> required "results" (Decode.list userUploadTicketDecoder)


userUploadTicketDecoder : Decoder UserUploadTicket
userUploadTicketDecoder =
    Decode.succeed UserUploadTicket
        |> required "id" int
        |> required "token" string
        |> required "n_submissions" int
        |> required "created_by" int
        |> required "orcid" string
        |> required "created_at" string
        |> required "status" string
        |> required "simulations" (list userUploadSimulationDecoder)
        |> required "ticket_type" string
        |> required "upload_instances" (list userUploadInstanceDecoder)
        |> required "status_messages" (list userUploadStatusMessageDecoder)
        |> required "successful" bool
        |> required "lead_contributor_orcid" string
        |> required "filenames" string


userUploadStatusMessageDecoder : Decoder UserUploadStatusMessage
userUploadStatusMessageDecoder =
    Decode.succeed UserUploadStatusMessage
        |> required "timestamp" string
        |> required "message" string
        |> required "is_error" bool
        |> required "is_warning" bool


userUploadInstanceDecoder : Decoder UserUploadInstance
userUploadInstanceDecoder =
    Decode.succeed UserUploadInstance
        |> required "id" int
        |> required "user" userDecoder
        |> required "ticket" int
        |> required "created_on" string
        |> required "simulation" userUploadSimulationDecoder


userUploadSimulationDecoder : Decoder UserUploadSimulation
userUploadSimulationDecoder =
    Decode.succeed UserUploadSimulation
        |> required "md_repo_id" string
        |> required "guid" string
        |> required "description" (nullable string)
        |> required "slug" string
        |> required "is_placeholder" bool
        |> required "is_deprecated" bool
        |> required "creation_date" string
        |> required "uploaded_files" (list uploadedFileDecoder)


uploadedFileDecoder : Decoder UploadedFile
uploadedFileDecoder =
    Decode.succeed UploadedFile
        |> required "id" int
        |> required "primary" bool
        |> required "filename" string
        |> required "file_type" string
        |> required "description" string
        |> required "file_size_bytes" string


uploadTokenDecoder : Decoder UploadToken
uploadTokenDecoder =
    Decode.succeed UploadToken
        |> required "token" string
        |> required "orcid" string


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    let
        _ =
            Debug.log "url" <| shared.apiHost ++ "/getUserUploadTickets"
    in
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = shared.apiHost ++ "/getUserUploadTickets"
            , expect = Http.expectJson GotUploadTickets decodeRequest
            }
    )



{-
   APP-RWW13NTJYYZHV0QE
   ed987171-cb2b-4203-a2f7-60e8d5dd58d7
   requestData shared =
       let
           headers =
               case shared.csrfToken of
                   Just token ->
                       [ Http.header "X-CSRFTOKEN" token ]

                   _ ->
                       []
       in
       Effect.sendCmd <|
           Http.request
               { method = "GET"
               , headers = headers
               , url = shared.apiHost ++ "/getUserUploadTickets"
               , body = Http.emptyBody
               , expect = Http.expectJson GotUploadTickets decodeRequest
               , timeout = Nothing
               , tracker = Nothing
               }

-}


requestUploadToken shared model =
    let
        headers =
            case shared.csrfToken of
                Just token ->
                    [ Http.header "X-CSRFTOKEN" token ]

                _ ->
                    []

        _ =
            Debug.log "request" <| uploadTokenRequestEncoder model
    in
    Effect.sendCmd <|
        Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ] ++ headers
            , url = shared.apiHost ++ "/uploads"
            , body = Http.jsonBody <| uploadTokenRequestEncoder model
            , expect = Http.expectJson GotUploadToken uploadTokenDecoder
            , timeout = Nothing
            , tracker = Nothing
            }


uploadTokenRequestEncoder : Model -> Value
uploadTokenRequestEncoder model =
    Encode.object
        [ ( "is_admin_token", Encode.bool model.isAdminToken )
        , ( "submissions", Encode.int model.numberUploads )
        ]



-- UPDATE


type Msg
    = DialogClose
    | DialogShow
    | GotUploadTickets (Result Http.Error UserUploadTicketsResult)
    | GotUploadToken (Result Http.Error UploadToken)
    | RequestUploadToken
    | SetNumberUploads String
    | SetIsAdminToken Bool


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        DialogClose ->
            ( { model | showDialog = False }
            , Effect.none
            )

        DialogShow ->
            ( { model | showDialog = True }
            , Effect.none
            )

        GotUploadTickets (Ok result) ->
            let
                _ =
                    Debug.log "result" result
            in
            ( { model | userUploadTicketsResult = Just result }
            , Effect.setErrorMessage Nothing
            )

        GotUploadTickets (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            ( { model | userUploadTicketsResult = Nothing }
            , Effect.setErrorMessage (Just (Api.toUserFriendlyMessage err))
            )

        GotUploadToken (Ok result) ->
            ( { model | uploadToken = Just result }
            , Effect.none
            )

        GotUploadToken (Err err) ->
            ( { model | uploadToken = Nothing, showDialog = False }
            , Effect.setErrorMessage (Just (Api.toUserFriendlyMessage err))
            )

        SetIsAdminToken newValue ->
            ( { model | isAdminToken = newValue }
            , Effect.none
            )

        SetNumberUploads val ->
            let
                newNumber =
                    Maybe.withDefault 0 (String.toInt val)
            in
            ( { model | numberUploads = newNumber }
            , Effect.none
            )

        RequestUploadToken ->
            ( model
            , requestUploadToken shared model
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    Components.Header.view
        { title = "MDRepo - Upload Logs"
        , body = viewModel model
        , shared = shared
        }


viewModel : Model -> List (Html.Html Msg)
viewModel model =
    let
        _ =
            Debug.log "model" model

        table =
            case model.userUploadTicketsResult of
                Just result ->
                    uploadsTable result.results

                _ ->
                    Html.text "Unable to get data"
    in
    [ Html.div
        [ class "container" ]
        [ Html.button
            [ class "button", onClick DialogShow ]
            [ Html.text "Get Upload Token" ]
        , viewDialog model
        , table
        ]
    ]


uploadsTable : List UserUploadTicket -> Html.Html Msg
uploadsTable tickets =
    let
        mkRow ticket =
            Html.tr []
                [ Html.td [] [ Html.text ticket.token ]
                , Html.td [] [ Html.text ticket.createdAt ]
                , Html.td [] [ Html.text ticket.status ]
                ]
    in
    case List.length tickets of
        0 ->
            Html.text "No data"

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
                , Html.tbody [] (List.map mkRow tickets)
                ]


viewDialog : Model -> Html.Html Msg
viewDialog model =
    let
        body =
            case model.uploadToken of
                Just token ->
                    [ Html.div [ class "cell" ]
                        [ Html.p [] [ Html.text <| "Token = " ++ token.token ]
                        , Html.p [] [ Html.text <| "ORCID = " ++ token.orcid ]
                        ]
                    ]

                _ ->
                    [ Html.div [ class "cell" ]
                        [ Html.text "Number of Simulations:"
                        , Html.input
                            [ class "input"
                            , value <| String.fromInt model.numberUploads
                            , onInput SetNumberUploads
                            ]
                            []
                        ]
                    , Html.div [ class "cell" ]
                        [ Html.label [ class "checkbox" ]
                            [ Html.input
                                [ type_ "checkbox"
                                , checked model.isAdminToken
                                , onCheck SetIsAdminToken
                                ]
                                []
                            , Html.text "Request Admin Token"
                            ]
                        ]
                    , Html.div [ class "cell" ]
                        [ Html.button
                            [ class "button is-success"
                            , onClick RequestUploadToken
                            ]
                            [ Html.text "Get Token" ]
                        ]
                    ]
    in
    if model.showDialog then
        Html.div [ class "modal is-active" ]
            [ Html.div [ class "modal-background" ] []
            , Html.div [ class "modal-card" ]
                [ Html.header [ class "modal-card-head" ]
                    [ Html.p [ class "modal-card-title" ]
                        [ Html.text "Get Download Token"
                        ]
                    , Html.button [ class "delete", onClick DialogClose ] []
                    ]
                , Html.section [ class "modal-card-body" ] body
                , Html.footer [ class "modal-card-foot" ]
                    [ Html.div [ class "buttons" ]
                        [ Html.button
                            [ class "button", onClick DialogClose ]
                            [ Html.text "Cancel" ]
                        ]
                    ]
                ]
            ]

    else
        Html.div [] []
