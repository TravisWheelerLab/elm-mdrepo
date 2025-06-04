module Pages.UploadLogs exposing (Model, Msg, page)

import Api
import Components.Header
import Decoders exposing (userDecoder)
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
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
    { uploadInstancesResult : Maybe UploadInstancesResult }


initialModel : Model
initialModel =
    { uploadInstancesResult = Nothing }


type alias UploadInstancesResult =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List UploadInstance
    }


uploadInstancesResultDecoder : Decoder UploadInstancesResult
uploadInstancesResultDecoder =
    Decode.succeed UploadInstancesResult
        |> required "count" int
        |> required "next" (nullable string)
        |> required "previous" (nullable string)
        |> required "results" (Decode.list uploadInstanceDecoder)


type alias UploadInstance =
    { id : Int
    , user : Maybe User
    , ticket : Int
    , createdOn : String
    , simulation : Simulation
    , statusMessages : List StatusMessage
    , successful : Maybe Bool
    , leadContributorOrcid : String
    , filenames : Maybe String
    }


uploadInstanceDecoder : Decoder UploadInstance
uploadInstanceDecoder =
    Decode.succeed UploadInstance
        |> required "id" int
        |> optional "user" (nullable userDecoder) Nothing
        |> required "ticket" int
        |> required "created_on" string
        |> required "simulation" simulationDecoder
        |> required "status_messages" (list statusMessageDecoder)
        |> optional "successful" (nullable bool) Nothing
        |> required "lead_contributor_orcid" string
        |> optional "filenames" (nullable string) Nothing


type alias StatusMessage =
    { timestamp : String
    , message : String
    , isError : Bool
    , isWarning : Bool
    }


statusMessageDecoder : Decoder StatusMessage
statusMessageDecoder =
    Decode.succeed StatusMessage
        |> required "timestamp" string
        |> required "message" string
        |> required "is_error" bool
        |> required "is_warning" bool


type alias Simulation =
    { mdRepoId : String
    , guid : String
    , description : Maybe String
    , slug : String
    , isPlaceholder : Bool
    , isDeprecated : Bool
    , creationDate : String
    , uploadedFiles : List UploadedFile
    }


simulationDecoder : Decoder Simulation
simulationDecoder =
    Decode.succeed Simulation
        |> required "md_repo_id" string
        |> required "guid" string
        |> required "description" (nullable string)
        |> required "slug" string
        |> required "is_placeholder" bool
        |> required "is_deprecated" bool
        |> required "creation_date" string
        |> required "uploaded_files" (list uploadedFileDecoder)


type alias UploadedFile =
    { id : Int
    , isPrimary : Bool
    , fileName : String
    , fileType : String
    , description : Maybe String
    , fileSizeBytes : Maybe Int
    }


uploadedFileDecoder : Decoder UploadedFile
uploadedFileDecoder =
    Decode.succeed UploadedFile
        |> required "id" int
        |> required "primary" bool
        |> required "filename" string
        |> required "file_type" string
        |> optional "description" (nullable string) Nothing
        |> optional "file_size_bytes" (nullable int) Nothing


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = shared.apiHost ++ "/getUploadInstances"
            , expect = Http.expectJson GotUploadInstances uploadInstancesResultDecoder
            }
    )



-- UPDATE


type Msg
    = GotUploadInstances (Result Http.Error UploadInstancesResult)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        GotUploadInstances (Ok result) ->
            ( { model | uploadInstancesResult = Just result }
            , Effect.none
            )

        GotUploadInstances (Err err) ->
            ( { model | uploadInstancesResult = Nothing }
            , Effect.setErrorMessage (Just (Api.toUserFriendlyMessage err))
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
        , body = viewModel shared model
        , shared = shared
        }


viewModel : Shared.Model -> Model -> List (Html.Html Msg)
viewModel shared model =
    let
        table =
            case model.uploadInstancesResult of
                Just result ->
                    uploadInstancesTable result.results

                _ ->
                    Html.text "Unable to get data"
    in
    [ Html.div
        [ class "content" ]
        [ Html.h1 [ class "title" ] [ Html.text "Upload Logs" ]
        , table
        ]
    ]


uploadInstancesTable : List UploadInstance -> Html.Html Msg
uploadInstancesTable instances =
    let
        viewSuccess value =
            case value of
                Just flag ->
                    if flag then
                        "Success"

                    else
                        "Error"

                _ ->
                    "Upload in progress"

        viewSimulation sim =
            if sim.isPlaceholder then
                Html.text ""

            else
                Html.a
                    [ Route.Path.href (Route.Path.Explore_Id_ { id = sim.mdRepoId }) ]
                    [ Html.text sim.slug ]

        mkRow instance =
            Html.tr []
                [ Html.td [] [ Html.text (viewSuccess instance.successful) ]
                , Html.td [] [ Html.text instance.createdOn ]
                , Html.td [] [ viewSimulation instance.simulation ]
                , Html.td [] [ Html.text (Maybe.withDefault "" instance.filenames) ]
                ]
    in
    case List.length instances of
        0 ->
            Html.text "No data"

        _ ->
            Html.table
                [ class "table" ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "Status" ]
                        , Html.th [] [ Html.text "Created On" ]
                        , Html.th [] [ Html.text "Simulation" ]
                        , Html.th [] [ Html.text "Filenames" ]
                        ]
                    ]
                , Html.tbody [] (List.map mkRow instances)
                ]
