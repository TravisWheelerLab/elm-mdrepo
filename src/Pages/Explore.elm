module Pages.Explore exposing (Model, Msg, page)

import Api
import Char exposing (isAlphaNum)
import Chart.Attributes exposing (onTopOrBottom)
import Components.Header
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (checked, class, disabled, name, placeholder, selected, src, type_, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Maybe
import Maybe.Extra exposing (isJust)
import Page exposing (Page)
import RemoteData exposing (RemoteData, WebData)
import Route exposing (Route)
import Route.Path
import Shared
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
    { recordCount : Maybe Int
    , simulations : WebData (List Simulation)
    , pageSize : Int
    , pageNumber : Int
    , textSearch : Maybe String
    , ligandSearch : Maybe String
    , nextUrl : Maybe String
    , previousUrl : Maybe String
    , selectedSimulationIds : List Int
    , showDownloadDialog : Bool
    , downloadFileTypes : List String
    , showDownloadFileTypes : Bool
    , downloadToken : Maybe String
    , downloadTokenError : Maybe String
    , sortColumn : String
    }


type alias Simulation =
    { id : Int
    , guid : String
    , slug : String
    , fastaSequence : Maybe String
    , shortDescription : Maybe String
    , biomolecules : List Biomolecule
    , ligands : List Ligand
    , software : Maybe Software
    }


type alias Biomolecule =
    { name : String
    , aminoLength : Int
    , sequence : String
    , uniprotId : Maybe String
    , pdbId : Maybe String
    , primaryMoleculeIdType : String
    }


type alias Ligand =
    { name : String
    , smilesString : String
    }


type alias ExploreRequest =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List Simulation
    }


type alias Software =
    { name : String
    , version : Maybe String
    }


type alias DownloadToken =
    { token : String
    }


initialModel : Model
initialModel =
    { recordCount = Nothing
    , simulations = RemoteData.NotAsked
    , pageSize = 25
    , pageNumber = 1
    , textSearch = Nothing
    , ligandSearch = Nothing
    , nextUrl = Nothing
    , previousUrl = Nothing
    , selectedSimulationIds = []
    , showDownloadDialog = False
    , downloadFileTypes = []
    , showDownloadFileTypes = False
    , downloadToken = Nothing
    , downloadTokenError = Nothing
    , sortColumn = "description"
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( initialModel
    , requestData shared initialModel
    )


requestData : Shared.Model -> Model -> Effect Msg
requestData shared model =
    let
        offset =
            if model.pageNumber > 1 then
                "&offset="
                    ++ String.fromInt
                        ((model.pageNumber
                            - 1
                         )
                            * model.pageSize
                        )

            else
                ""

        search =
            case model.textSearch of
                Just text ->
                    "&search=" ++ text

                _ ->
                    ""

        ordering =
            "&ordering=" ++ model.sortColumn

        url =
            shared.apiHost
                ++ "/getSimulations?limit="
                ++ String.fromInt model.pageSize
                ++ offset
                ++ search
                ++ ordering
    in
    Effect.sendCmd <|
        Http.get
            { url = url
            , expect =
                Http.expectJson
                    SimulationApiResponded
                    decodeRequest
            }


decodeRequest : Decoder ExploreRequest
decodeRequest =
    Decode.succeed ExploreRequest
        |> required "count" int
        |> required "next" (nullable string)
        |> required "previous" (nullable string)
        |> required "results" (Decode.list simulationDecoder)


downloadTokenDecoder : Decoder DownloadToken
downloadTokenDecoder =
    Decode.succeed DownloadToken
        |> required "token" string


simulationDecoder : Decoder Simulation
simulationDecoder =
    Decode.succeed Simulation
        |> required "id" int
        |> required "guid" string
        |> required "slug" string
        |> required "fasta_sequence" (nullable string)
        |> required "short_description" (nullable string)
        |> required "biomolecules" (Decode.list biomoleculeDecoder)
        |> required "ligands" (Decode.list ligandDecoder)
        |> required "software" (nullable softwareDecoder)


softwareDecoder : Decoder Software
softwareDecoder =
    Decode.succeed Software
        |> required "name" string
        |> required "version" (nullable string)


biomoleculeDecoder : Decoder Biomolecule
biomoleculeDecoder =
    Decode.succeed Biomolecule
        |> required "name" string
        |> required "amino_length" int
        |> required "sequence" string
        |> required "uniprot_id" (nullable string)
        |> required "pdb_id" (nullable string)
        |> required "primary_molecule_id_type" string


ligandDecoder : Decoder Ligand
ligandDecoder =
    Decode.succeed Ligand
        |> required "name" string
        |> required "smiles_string" string


downloadTokenRequestEncoder : Model -> Value
downloadTokenRequestEncoder model =
    let
        simulationIds =
            List.map String.fromInt model.selectedSimulationIds

        selectedFileTypes =
            model.downloadFileTypes
    in
    Encode.object
        [ ( "simulation_ids", Encode.list Encode.string simulationIds )
        , ( "download_all_file_types", Encode.bool (List.isEmpty selectedFileTypes) )
        , ( "selected_file_types", Encode.list Encode.string selectedFileTypes )
        ]



-- UPDATE


type Msg
    = CloseDownloadDialog
    | CopyDownloadTokenToClipboard
    | GetDownloadToken
    | GotDownloadToken (Result Http.Error DownloadToken)
    | SimulationApiResponded (Result Http.Error ExploreRequest)
    | ShowDownloadDialog
    | SelectAllDownloadFileTypes
    | SetDownloadFileType String Bool
    | SetSortColumn String
    | UpdatePageSize String
    | UpdatePageNumber String
    | UpdateTextSearch String
    | ToggleSimulationSelection Int Bool
    | ToggleAllSimulations Bool
    | ToggleShowDownloadFileTypes


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        CloseDownloadDialog ->
            ( { model | showDownloadDialog = False }, Effect.none )

        CopyDownloadTokenToClipboard ->
            let
                cmd =
                    case model.downloadToken of
                        Just token ->
                            Effect.copyToClipboard token

                        _ ->
                            Effect.none
            in
            ( model, cmd )

        GetDownloadToken ->
            let
                headers =
                    case shared.csrfToken of
                        Just token ->
                            [ Http.header "X-CSRFTOKEN" token ]

                        _ ->
                            []
            in
            ( model
            , Effect.sendCmd <|
                Http.request
                    { method = "POST"
                    , headers = headers
                    , url = shared.apiHost ++ "/downloads"
                    , body = Http.jsonBody <| downloadTokenRequestEncoder model
                    , expect = Http.expectJson GotDownloadToken downloadTokenDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }
            )

        GotDownloadToken (Ok downloadToken) ->
            ( { model
                | downloadToken = Just downloadToken.token
                , downloadTokenError = Nothing
              }
            , Effect.none
            )

        GotDownloadToken (Err err) ->
            ( { model
                | downloadToken = Nothing
                , downloadTokenError = Just (Api.toUserFriendlyMessage err)
              }
            , Effect.none
            )

        SetDownloadFileType fileType isChecked ->
            let
                newFileTypes =
                    if isChecked then
                        model.downloadFileTypes ++ [ fileType ]

                    else
                        List.filter ((/=) fileType) model.downloadFileTypes
            in
            ( { model | downloadFileTypes = newFileTypes }
            , Effect.none
            )

        ShowDownloadDialog ->
            ( { model | showDownloadDialog = True }, Effect.none )

        SetSortColumn newSortColumn ->
            let
                newModel =
                    { model | sortColumn = newSortColumn }
            in
            ( newModel, requestData shared newModel )

        SelectAllDownloadFileTypes ->
            ( { model | showDownloadFileTypes = False, downloadFileTypes = [] }, Effect.none )

        SimulationApiResponded (Ok data) ->
            ( { model
                | recordCount = Just data.count
                , simulations = RemoteData.Success data.results
                , nextUrl = data.next
                , previousUrl = data.previous
              }
            , Effect.setErrorMessage Nothing
            )

        SimulationApiResponded (Err err) ->
            ( { model
                | recordCount = Nothing
                , simulations = RemoteData.Failure err
                , nextUrl = Nothing
                , previousUrl = Nothing
              }
            , Effect.setErrorMessage (Just (Api.toUserFriendlyMessage err))
            )

        UpdatePageSize newSize ->
            case String.toInt newSize of
                Just size ->
                    let
                        newModel =
                            { model
                                | pageSize = size
                                , pageNumber = 1
                            }
                    in
                    ( newModel
                    , requestData shared newModel
                    )

                Nothing ->
                    ( model
                    , Effect.setErrorMessage (Just "Invalid page size")
                    )

        UpdatePageNumber newNumber ->
            case String.toInt newNumber of
                Just pageNumber ->
                    let
                        newModel =
                            { model | pageNumber = pageNumber }
                    in
                    ( newModel
                    , requestData shared newModel
                    )

                Nothing ->
                    ( model
                    , Effect.setErrorMessage (Just "Invalid page number")
                    )

        UpdateTextSearch newText ->
            let
                textSearch =
                    case String.length newText of
                        0 ->
                            Nothing

                        _ ->
                            Just newText

                newModel =
                    { model
                        | textSearch = textSearch
                        , selectedSimulationIds = []
                        , pageNumber = 0
                    }
            in
            ( newModel
            , requestData shared newModel
            )

        ToggleShowDownloadFileTypes ->
            ( { model | showDownloadFileTypes = not model.showDownloadFileTypes }, Effect.none )

        ToggleAllSimulations checked ->
            let
                selectedSimulationIds =
                    if checked then
                        case model.simulations of
                            RemoteData.Success sims ->
                                List.map .id sims

                            _ ->
                                []

                    else
                        []
            in
            ( { model | selectedSimulationIds = selectedSimulationIds }, Effect.none )

        ToggleSimulationSelection simulationId isChecked ->
            let
                newIds =
                    if isChecked then
                        model.selectedSimulationIds ++ [ simulationId ]

                    else
                        List.filter ((/=) simulationId) model.selectedSimulationIds
            in
            ( { model | selectedSimulationIds = newIds }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    Components.Header.view
        { title = "MDRepo - Explore"
        , body =
            [ case model.simulations of
                RemoteData.Success simulations ->
                    Html.div [ class "container" ]
                        [ Html.div [ class "box" ]
                            [ viewDownloadDialog model
                            , pagination shared model
                            , viewSimulations shared simulations model.selectedSimulationIds model.sortColumn
                            ]
                        ]

                RemoteData.Failure err ->
                    Html.div [] []

                _ ->
                    Html.div [ class "has-text-centered p-6" ]
                        [ Html.text "Loading..." ]
            ]
        , shared = shared
        }


viewDownloadDialog : Model -> Html Msg
viewDownloadDialog model =
    let
        ids =
            String.join ", " <|
                List.map String.fromInt model.selectedSimulationIds

        numSelected =
            List.length model.selectedSimulationIds

        fileTypes =
            [ "Structure"
            , "Topology"
            , "Trajectory"
            , "Minimal structure"
            , "Minimal topology"
            , "Minimal trajectory"
            , "Minimal preview image"
            , ".gif preview image"
            , "Samples minimal trajectory"
            , "Periodic boundary condition"
            , "PBC file"
            , "FASTA file"
            , "Compressed trajectory frame datas"
            ]

        fileTypesCheckboxes =
            if model.showDownloadFileTypes then
                Html.div [ class "fixed-grid has-auto-count" ] <|
                    List.map
                        (\fileType ->
                            Html.div [ class "cell" ]
                                [ Html.label [ class "checkbox" ]
                                    [ Html.input
                                        [ type_ "checkbox"
                                        , onCheck (SetDownloadFileType fileType)
                                        ]
                                        []
                                    , Html.text fileType
                                    ]
                                ]
                        )
                        fileTypes

            else
                Html.div [] []

        downloadTokenView =
            case model.downloadToken of
                Just token ->
                    Html.div []
                        [ Html.text <| "Download token = " ++ token
                        , Html.button
                            [ class "button", onClick CopyDownloadTokenToClipboard ]
                            [ Html.text "Copy to clipboard" ]
                        ]

                _ ->
                    Html.div [] []

        downloadTokenErrorView =
            case model.downloadTokenError of
                Just err ->
                    Html.div [] [ Html.text <| "Error: " ++ err ]

                _ ->
                    Html.div [] []
    in
    if model.showDownloadDialog then
        Html.div [ class "modal is-active" ]
            [ Html.div [ class "modal-background" ] []
            , Html.div [ class "modal-card" ]
                [ Html.header [ class "modal-card-head" ]
                    [ Html.p [ class "modal-card-title" ]
                        [ Html.text "Get Download Token"
                        ]
                    , Html.button [ class "delete", onClick CloseDownloadDialog ] []
                    ]
                , Html.section [ class "modal-card-body" ]
                    [ Html.text <|
                        String.fromInt numSelected
                            ++ " selected simulation"
                            ++ (if numSelected == 1 then
                                    ""

                                else
                                    "s"
                               )
                    , Html.p []
                        [ Html.text <| "Types: " ++ String.join ", " model.downloadFileTypes ]
                    , Html.div [ class "radios" ]
                        [ Html.label [ class "radio" ]
                            [ Html.input
                                [ type_ "radio"
                                , name "download_file_type"
                                , onClick SelectAllDownloadFileTypes
                                ]
                                []
                            , Html.text "Download all file types"
                            ]
                        , Html.label [ class "radio" ]
                            [ Html.input
                                [ type_ "radio"
                                , name "download_file_type"
                                , onClick ToggleShowDownloadFileTypes
                                ]
                                []
                            , Html.text " Select file types"
                            ]
                        ]
                    , fileTypesCheckboxes
                    , downloadTokenView
                    , downloadTokenErrorView
                    ]
                , Html.footer [ class "modal-card-foot" ]
                    [ Html.div [ class "buttons" ]
                        [ Html.button
                            [ class "button is-success"
                            , onClick GetDownloadToken
                            ]
                            [ Html.text "Get Token" ]
                        , Html.button
                            [ class "button", onClick CloseDownloadDialog ]
                            [ Html.text "Cancel" ]
                        ]
                    ]
                ]
            ]

    else
        Html.div [] []


pagination : Shared.Model -> Model -> Html Msg
pagination shared model =
    let
        {- prevUrl =
               case model.previousUrl of
                   Nothing ->
                       []

                   Just url ->
                       [ Html.a [ href url, class "pagination-previous" ] [ Html.text "Previous" ]
                       ]

           nextUrl =
               case model.nextUrl of
                   Nothing ->
                       []

                   Just url ->
                       [ Html.a [ href url, class "pagination-next" ] [ Html.text "Next" ]
                       ]
        -}
        downloadButton =
            let
                loggedIn =
                    isJust shared.user

                buttonText =
                    if loggedIn then
                        case List.length model.selectedSimulationIds of
                            0 ->
                                "Download"

                            num ->
                                "Download " ++ String.fromInt num

                    else
                        "Login to Download"
            in
            Html.div [ class "control" ]
                [ Html.button
                    [ class "button is-primary"
                    , disabled (List.isEmpty model.selectedSimulationIds || not loggedIn)
                    , onClick ShowDownloadDialog
                    ]
                    [ Html.text buttonText ]
                ]

        recordCount =
            Maybe.withDefault 0 model.recordCount

        numShowing =
            case model.simulations of
                RemoteData.Success sims ->
                    List.length sims

                _ ->
                    0

        numPages =
            recordCount // model.pageSize

        pageNav =
            let
                options =
                    List.map
                        (\num ->
                            Html.option [ selected (num == model.pageNumber) ]
                                [ Html.text (String.fromInt num) ]
                        )
                        (List.range
                            1
                            (numPages + 1)
                        )
            in
            Html.div [ class "control" ]
                [ Html.text "Page: "
                , Html.div [ class "select" ]
                    [ Html.select [ onInput UpdatePageNumber ] options ]
                ]

        pageSizeNav =
            let
                options =
                    List.map
                        (\num ->
                            Html.option [ selected (model.pageSize == num) ]
                                [ Html.text (String.fromInt num) ]
                        )
                        [ 10, 25, 50, 100 ]
            in
            Html.div [ class "control" ]
                [ Html.text "Show: "
                , Html.div [ class "select" ]
                    [ Html.select [ onInput UpdatePageSize ] options
                    ]
                ]
    in
    Html.div [ class "container" ]
        [ Html.div [ class "columns" ]
            [ Html.div [ class "column" ] [ downloadButton ]
            , Html.div [ class "column" ]
                [ Html.text
                    ("Count: "
                        ++ String.fromInt recordCount
                    )
                ]
            , Html.div [ class "column" ]
                [ Html.div [ class "control" ]
                    [ Html.input
                        [ type_ "text"
                        , class "input"
                        , placeholder "Search"
                        , onInput UpdateTextSearch
                        ]
                        []
                    ]
                ]
            , Html.div [ class "column" ] [ pageSizeNav ]
            , Html.div [ class "column" ] [ pageNav ]
            ]
        ]


viewSimulations : Shared.Model -> List Simulation -> List Int -> String -> Html Msg
viewSimulations shared simulations selectedSimulationIds sortColumn =
    let
        up =
            "↓"

        down =
            "↑"

        both =
            "↓↑"

        ( descSortCol, descSortArrow ) =
            case sortColumn of
                "description" ->
                    ( "-description", up )

                "-description" ->
                    ( "description", down )

                _ ->
                    ( "description", both )

        ( idSortCol, idSortArrow ) =
            case sortColumn of
                "id" ->
                    ( "-id", up )

                "-id" ->
                    ( "id", down )

                _ ->
                    ( "id", both )

        ( softwareSortCol, softwareSortArrow ) =
            case sortColumn of
                "softwareName" ->
                    ( "-softwareName", up )

                "-softwareName" ->
                    ( "softwareName", down )

                _ ->
                    ( "softwareName", both )

        header =
            Html.thead []
                [ Html.tr []
                    [ Html.th []
                        [ Html.input
                            [ type_ "checkbox"
                            , onCheck ToggleAllSimulations
                            ]
                            []
                        ]
                    , Html.th [] [ Html.text "Thumbnail" ]
                    , Html.th []
                        [ Html.a
                            [ onClick (SetSortColumn descSortCol) ]
                            [ Html.text <| "Description" ++ descSortArrow ]
                        ]
                    , Html.th []
                        [ Html.a
                            [ onClick (SetSortColumn idSortCol) ]
                            [ Html.text <| "MDRepo ID" ++ idSortArrow ]
                        ]
                    , Html.th [] [ Html.text "Biomolecules" ]
                    , Html.th [] [ Html.text "Ligands" ]
                    , Html.th [] [ Html.text "Sequence" ]
                    , Html.th []
                        [ Html.a
                            [ onClick (SetSortColumn softwareSortCol) ]
                            [ Html.text <| "Software" ++ softwareSortArrow ]
                        ]
                    ]
                ]

        rows =
            Html.tbody []
                (List.map
                    (viewSimulation shared selectedSimulationIds)
                    simulations
                )
    in
    Html.table
        [ class "table is-striped" ]
        [ header, rows ]


viewSimulation : Shared.Model -> List Int -> Simulation -> Html Msg
viewSimulation shared selectedSimulationIds simulation =
    Html.tr []
        [ Html.td []
            [ Html.input
                [ type_ "checkbox"
                , onCheck (ToggleSimulationSelection simulation.id)
                , checked (List.member simulation.id selectedSimulationIds)
                ]
                []
            ]
        , Html.td []
            [ Html.img
                [ src <| shared.mediaHost ++ "/" ++ simulation.guid ++ "/thumbnail.png"
                , width 200
                ]
                []
            ]
        , Html.td []
            [ Html.text <|
                truncate 30
                    (Maybe.withDefault "" simulation.shortDescription)
            ]
        , Html.td []
            [ Html.a
                [ Route.Path.href <|
                    Route.Path.Explore_Id_
                        { id = String.fromInt simulation.id }
                ]
                [ Html.text simulation.slug ]
            ]
        , Html.td []
            [ viewBiomolecules simulation.biomolecules
            ]
        , Html.td []
            [ viewLigands simulation.ligands
            ]
        , Html.td []
            [ Html.text <|
                truncate 30 (Maybe.withDefault "" simulation.fastaSequence)
            ]
        , Html.td []
            [ viewSoftware simulation.software
            ]
        ]


truncate : Int -> String -> String
truncate len val =
    if String.length val > len then
        String.left (len - 3) val ++ "..."

    else
        val


viewSoftware : Maybe Software -> Html Msg
viewSoftware software =
    case software of
        Just val ->
            Html.text val.name

        _ ->
            Html.text ""


viewBiomolecules : List Biomolecule -> Html Msg
viewBiomolecules biomolecules =
    case List.length biomolecules of
        0 ->
            Html.text ""

        _ ->
            Html.ul [] <|
                List.map
                    (\m -> Html.li [] [ Html.text m.name ])
                    biomolecules


viewLigands : List Ligand -> Html Msg
viewLigands ligands =
    case List.length ligands of
        0 ->
            Html.text ""

        _ ->
            Html.ul [] <|
                List.map
                    (\m -> Html.li [] [ Html.text m.name ])
                    ligands
