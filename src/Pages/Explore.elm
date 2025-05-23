module Pages.Explore exposing (Model, Msg, page)

import Api
import Char exposing (isAlphaNum)
import Components.Header
import Config
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (checked, class, disabled, name, placeholder, selected, src, type_, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Maybe
import Page exposing (Page)
import RemoteData exposing (RemoteData, WebData)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
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
    , error : Maybe String
    , showDownloadDialog : Bool
    , downloadFileTypes : List String
    , showDownloadFileTypes : Bool
    , downloadToken : Maybe String
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



{- type alias DownloadTokenRequest =
   { simulationIds : List Int
   , downloadAllFileTypes : Bool
   , selectedFileTypes : List String
   }
-}
-- TODO: Effect.sendApiRequest?


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
    , error = Nothing
    , selectedSimulationIds = []
    , showDownloadDialog = False
    , downloadFileTypes = []
    , showDownloadFileTypes = False
    , downloadToken = Nothing
    , sortColumn = "description"
    }


init : () -> ( Model, Effect Msg )
init () =
    ( initialModel
    , requestData initialModel
    )


requestData : Model -> Effect Msg
requestData model =
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
            Config.apiHost
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



{- downloadTokenRequestEncoder : DownloadTokenRequest -> Value
   downloadTokenRequestEncoder request =
       Encode.object
           [ ( "simulation_ids", Encode.list (List.map Encode.int request.simulationIds) )
           , ( "download_all_file_types", Encode.bool (List.isEmpty request.selectedFileTypes) )
           , ( "selected_file_types", Encode.list (List.map Encode.string request.selectedFileTypes) )
           ]
-}
-- UPDATE


type Msg
    = CloseDownloadDialog
      -- | GetDownloadToken
      -- | GotDownloadToken (Result Http.Error String)
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


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        CloseDownloadDialog ->
            ( { model | showDownloadDialog = False }, Effect.none )

        {- GotDownloadToken ->
              let
                  downloadToken =
                      getDownloadToken model
              in
              ( { model | downloadToken = Just downloadToken }, Effect.none )

           getDownloadToken : Model -> String
           getDownloadToken model =
               Http.post
               { url = Config.apiHost ++ "/downloads"
               , body = Http.emptyBody
               , expect = Http.expectJson GotDownloadToken string
               }
        -}
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
            ( newModel, requestData newModel )

        SelectAllDownloadFileTypes ->
            ( { model | showDownloadFileTypes = False, downloadFileTypes = [] }, Effect.none )

        SimulationApiResponded (Ok data) ->
            ( { model
                | recordCount = Just data.count
                , simulations = RemoteData.Success data.results
                , nextUrl = data.next
                , previousUrl = data.previous
              }
            , Effect.none
            )

        SimulationApiResponded (Err err) ->
            ( { model
                | recordCount = Nothing
                , simulations = RemoteData.Failure err
                , nextUrl = Nothing
                , previousUrl = Nothing
              }
            , Effect.none
            )

        UpdatePageSize newSize ->
            case String.toInt newSize of
                Just size ->
                    let
                        newModel =
                            { model
                                | pageSize = size
                                , pageNumber = 1
                                , error = Nothing
                            }
                    in
                    ( newModel
                    , requestData newModel
                    )

                Nothing ->
                    ( { model
                        | error = Just "Invalid page size"
                      }
                    , Effect.none
                    )

        UpdatePageNumber newNumber ->
            case String.toInt newNumber of
                Just pageNumber ->
                    let
                        newModel =
                            { model
                                | pageNumber = pageNumber
                                , error = Nothing
                            }
                    in
                    ( newModel
                    , requestData newModel
                    )

                Nothing ->
                    ( { model
                        | error = Just "Invalid page number"
                      }
                    , Effect.none
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
            , requestData newModel
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


view : Model -> View Msg
view model =
    Components.Header.view
        { title = "MDRepo - Explore"
        , body =
            [ case model.simulations of
                RemoteData.Success simulations ->
                    Html.div [ class "container" ]
                        [ Html.div [ class "box" ]
                            [ viewDownloadDialog model
                            , pagination model
                            , viewSimulations simulations model.selectedSimulationIds model.sortColumn
                            ]
                        ]

                RemoteData.Failure err ->
                    Html.div [ class "has-text-centered p-6" ]
                        [ Html.text <| Api.toUserFriendlyMessage err ]

                _ ->
                    Html.div [ class "has-text-centered p-6" ]
                        [ Html.text "Loading..." ]
            ]
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

                                --, checked (List.isEmpty model.downloadFileTypes)
                                --, onClick SetDownloadAllFileTypes
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
                    ]
                , Html.footer [ class "modal-card-foot" ]
                    [ Html.div [ class "buttons" ]
                        [ Html.button
                            [ class "button is-success"

                            -- , onClick GetDownloadToken
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


pagination : Model -> Html Msg
pagination model =
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
                buttonText =
                    case List.length model.selectedSimulationIds of
                        0 ->
                            "Download"

                        num ->
                            "Download " ++ String.fromInt num
            in
            Html.div [ class "control" ]
                [ Html.button
                    [ class "button is-primary"
                    , disabled (List.isEmpty model.selectedSimulationIds)
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


viewSimulations : List Simulation -> List Int -> String -> Html Msg
viewSimulations simulations selectedSimulationIds sortColumn =
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
                    (viewSimulation selectedSimulationIds)
                    simulations
                )
    in
    Html.table
        [ class "table is-striped" ]
        [ header, rows ]


viewSimulation : List Int -> Simulation -> Html Msg
viewSimulation selectedSimulationIds simulation =
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
                [ src <| Config.mediaHost ++ "/" ++ simulation.guid ++ "/thumbnail.png"
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
