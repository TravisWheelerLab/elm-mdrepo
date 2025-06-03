module Pages.Explore.Id_ exposing (Model, Msg, page)

--import Layouts

import Api
import Chart
import Chart.Attributes as CA
import Components.Header
import Decoders exposing (downloadInstanceDecoder, simulationDecoder)
import Effect exposing (Effect)
import Filesize
import Html
import Html.Attributes exposing (align, checked, class, disabled, href, readonly, rows, src, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Encode as Encode
import Maybe exposing (..)
import Page exposing (Page)
import Regex
import RemoteData exposing (WebData)
import Route exposing (Route)
import Route.Path
import Shared
import Time
import Types exposing (Biomolecule, Contribution, DownloadInstance, Ligand, ProcessedFile, Simulation, Solvent, UnvalidatedBiomolecule, UploadedFile)
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { simulation : WebData Simulation
    , selectedProcessedFileIds : List Int
    , downloadInstanceId : Maybe Int
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { simulation = RemoteData.NotAsked
    , selectedProcessedFileIds = []
    , downloadInstanceId = Nothing
    , error = Nothing
    }


init : Shared.Model -> Route { id : String } -> () -> ( Model, Effect Msg )
init shared route _ =
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = shared.apiHost ++ "/getSimulations/" ++ route.params.id
            , expect = Http.expectJson SimulationApiResponded simulationDecoder
            }
    )



--|> required "email" (nullable string) -- `null` decodes to `Nothing`
--|> optional "name" string "(fallback if name is `null` or not present)"
-- UPDATE


type Msg
    = SimulationApiResponded (Result Http.Error Simulation)
    | ToggleAllProcessedFiles Bool
    | ToggleProcessedFile Int Bool
    | CreateDownloadInstance
    | GotDownloadInstanceId (Result Http.Error DownloadInstance)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        CreateDownloadInstance ->
            let
                fileEncoder fileId =
                    Encode.object
                        [ ( "file_type", Encode.string "processed" )
                        , ( "file_id", Encode.int fileId )
                        ]

                body =
                    case model.simulation of
                        RemoteData.Success simulation ->
                            Encode.object
                                [ ( "files", Encode.list fileEncoder model.selectedProcessedFileIds )
                                , ( "simulation_id", Encode.int simulation.id )
                                ]

                        _ ->
                            Encode.object []
            in
            ( model
            , Effect.sendCmd <|
                Http.post
                    { url = shared.apiHost ++ "/create_download_instance"
                    , body = Http.jsonBody body
                    , expect = Http.expectJson GotDownloadInstanceId downloadInstanceDecoder
                    }
            )

        GotDownloadInstanceId (Ok downloadInstance) ->
            ( { model | downloadInstanceId = Just downloadInstance.downloadInstanceId }
              -- TODO: Use downloadInstanceId to initiate download
            , Effect.loadExternalUrl <|
                shared.apiHost
                    ++ "/get_download_instance/"
                    ++ String.fromInt downloadInstance.downloadInstanceId
            )

        GotDownloadInstanceId (Err err) ->
            ( { model
                | downloadInstanceId = Nothing
                , error = Just (Api.toUserFriendlyMessage err)
              }
            , Effect.none
            )

        SimulationApiResponded (Ok simulation) ->
            ( { model | simulation = RemoteData.Success simulation }
            , Effect.none
            )

        SimulationApiResponded (Err err) ->
            ( { model | simulation = RemoteData.Failure err }
            , Effect.none
            )

        ToggleAllProcessedFiles checked ->
            let
                fileIds =
                    if checked then
                        case model.simulation of
                            RemoteData.Success simulation ->
                                List.map .id simulation.processedFiles

                            _ ->
                                []

                    else
                        []
            in
            ( { model | selectedProcessedFileIds = fileIds }, Effect.none )

        ToggleProcessedFile fileId isChecked ->
            let
                newIds =
                    if isChecked then
                        model.selectedProcessedFileIds ++ [ fileId ]

                    else
                        List.filter ((/=) fileId) model.selectedProcessedFileIds
            in
            ( { model | selectedProcessedFileIds = newIds }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        body =
            case model.simulation of
                RemoteData.NotAsked ->
                    Html.text "Not asked"

                RemoteData.Loading ->
                    Html.text "Loading"

                RemoteData.Success simulation ->
                    viewSimulation shared simulation model.selectedProcessedFileIds

                RemoteData.Failure err ->
                    Html.text <| "Got error: " ++ Api.toUserFriendlyMessage err

        errView =
            case model.error of
                Just err ->
                    [ Html.section [ class "hero is-danger" ]
                        [ Html.div
                            [ class "hero-body" ]
                            [ Html.p [ class "title" ] [ Html.text "Error" ]
                            , Html.p [] [ Html.text err ]
                            ]
                        ]
                    ]

                _ ->
                    []
    in
    Components.Header.view
        { title = "MDRepo - View Simulation"
        , body = List.concat [ errView, [ body ] ]
        , shared = shared
        }


viewSimulation : Shared.Model -> Simulation -> List Int -> Html.Html Msg
viewSimulation shared simulation selectedProcessedFileIds =
    let
        makeSimulationLink simId =
            let
                splitter =
                    Maybe.withDefault Regex.never <| Regex.fromString "MDR"

                split =
                    List.filterMap String.toInt <| Regex.split splitter simId
            in
            case split of
                [ id ] ->
                    Just <|
                        Html.a
                            [ Route.Path.href <|
                                Route.Path.Explore_Id_ { id = String.fromInt id }
                            ]
                            [ Html.text simId ]

                _ ->
                    Nothing

        replicateSimulations =
            List.filter ((/=) simulation.slug) simulation.replicateGroup.simulationSet
                |> List.filterMap makeSimulationLink

        tbl1 =
            Html.table [ class "table" ]
                [ Html.thead []
                    []
                , Html.tbody
                    []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "Description" ]
                        , Html.td [] [ Html.text <| withDefault "NA" simulation.description ]
                        ]
                    , Html.tr []
                        [ Html.th [] [ Html.text "External Link" ]
                        , Html.td []
                            [ case simulation.externalLink of
                                Just link ->
                                    Html.a [ href link ] [ Html.text link ]

                                _ ->
                                    Html.text "NA"
                            ]
                        ]
                    , Html.tr []
                        [ Html.th []
                            [ Html.text "Created On" ]
                        , Html.td
                            []
                            [ Html.text <| viewCreationDate simulation.creationDate ]
                        ]
                    , Html.tr []
                        [ Html.th []
                            [ Html.text "Publications" ]
                        , Html.td
                            []
                            [ case List.length <| simulation.papers of
                                0 ->
                                    Html.text "NA"

                                _ ->
                                    Html.ul [] <|
                                        List.map
                                            (\paper -> Html.li [] (viewPaper paper))
                                            simulation.papers
                            ]
                        ]
                    , Html.tr []
                        [ Html.th []
                            [ Html.text "Replicates" ]
                        , Html.td
                            []
                            [ case List.length <| replicateSimulations of
                                0 ->
                                    Html.text "NA"

                                _ ->
                                    Html.ul [] <|
                                        List.map
                                            (\replicate -> Html.li [] [ replicate ])
                                            replicateSimulations
                            ]
                        ]
                    , Html.tr []
                        [ Html.th []
                            [ Html.text "Replicate" ]
                        , Html.td
                            []
                            [ Html.text <| viewReplicate simulation.replicate simulation.totalReplicates ]
                        ]
                    , Html.tr []
                        [ Html.th []
                            [ Html.text "Software" ]
                        , Html.td
                            []
                            [ Html.text <|
                                simulation.software.name
                                    ++ (case Maybe.withDefault "" simulation.software.version of
                                            "" ->
                                                ""

                                            version ->
                                                " (" ++ version ++ ")"
                                       )
                            ]
                        ]
                    , Html.tr []
                        [ Html.th [] [ Html.text "Sampling timestep (ns)" ]
                        , Html.td []
                            [ Html.text
                                (String.fromFloat (Maybe.withDefault 0.0 simulation.samplingFrequency))
                            ]
                        ]
                    , Html.tr []
                        [ Html.th [] [ Html.text "Duration (ns)" ]
                        , Html.td []
                            [ Html.text
                                (String.fromFloat <| Maybe.withDefault 0.0 simulation.duration)
                            ]
                        ]
                    , Html.tr []
                        [ Html.th [] [ Html.text "Integration time step (fs)" ]
                        , Html.td [] [ Html.text (String.fromFloat simulation.integrationTimestepFs) ]
                        ]
                    , Html.tr []
                        [ Html.th [] [ Html.text "Temperature" ]
                        , Html.td [] [ Html.text <| String.fromInt simulation.temperature ++ "K" ]
                        ]
                    , Html.tr []
                        [ Html.th [] [ Html.text "Protein Sequence" ]
                        , Html.td []
                            [ Html.textarea
                                [ readonly True, class "textarea", rows 7 ]
                                [ Html.text (Maybe.withDefault "" simulation.fastaSequence) ]
                            ]
                        ]
                    ]
                ]

        tbl2 =
            Html.table [ class "table" ]
                [ Html.thead []
                    []
                , Html.tbody
                    []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "Created By" ]
                        , Html.td [] [ Html.text <| Maybe.withDefault "" simulation.createdBy.fullName ]
                        ]
                    , Html.tr []
                        [ Html.th []
                            [ Html.text <|
                                "Contributions ("
                                    ++ (String.fromInt <| List.length simulation.contributions)
                                    ++ ")"
                            ]
                        , Html.td []
                            [ case List.length simulation.contributions of
                                0 ->
                                    Html.text "NA"

                                _ ->
                                    Html.ul [] <| List.map viewContribution simulation.contributions
                            ]
                        ]
                    , Html.tr []
                        [ Html.th []
                            [ Html.text <|
                                "Biomolecules ("
                                    ++ (String.fromInt <| List.length simulation.biomolecules)
                                    ++ ")"
                            ]
                        , Html.td []
                            [ case List.length simulation.biomolecules of
                                0 ->
                                    Html.text "NA"

                                _ ->
                                    Html.table [ class "table" ]
                                        [ Html.thead []
                                            [ Html.tr []
                                                [ Html.th [] [ Html.text "Name" ]
                                                , Html.th [] [ Html.text "Uniprot ID" ]
                                                ]
                                            ]
                                        , Html.tbody [] <|
                                            List.map viewBiomolecule simulation.biomolecules
                                        ]
                            ]
                        ]
                    , Html.tr
                        []
                        [ Html.th []
                            [ Html.text <|
                                "Unvalidated Biomolecules ("
                                    ++ (String.fromInt <| List.length simulation.unvalidatedBiomolecules)
                                    ++ ")"
                            ]
                        , Html.td []
                            [ case List.length simulation.unvalidatedBiomolecules of
                                0 ->
                                    Html.text "NA"

                                _ ->
                                    Html.ul [] <| List.map viewUnvalidatedBiomolecule simulation.unvalidatedBiomolecules
                            ]
                        ]
                    , Html.tr
                        []
                        [ Html.th []
                            [ Html.text <|
                                "Ligands ("
                                    ++ (String.fromInt <| List.length simulation.ligands)
                                    ++ ")"
                            ]
                        , Html.td []
                            [ case List.length simulation.ligands of
                                0 ->
                                    Html.text "NA"

                                _ ->
                                    Html.ul [] <| List.map viewLigand simulation.ligands
                            ]
                        ]
                    , Html.tr
                        []
                        [ Html.th []
                            [ Html.text <|
                                "Solvents ("
                                    ++ (String.fromInt <| List.length simulation.solvents)
                                    ++ ")"
                            ]
                        , Html.td []
                            [ case List.length simulation.solvents of
                                0 ->
                                    Html.text "NA"

                                _ ->
                                    Html.ul [] <| List.map viewSolvent simulation.solvents
                            ]
                        ]
                    , Html.tr
                        []
                        [ Html.th [] [ Html.text "RMSD Values" ]
                        , Html.td []
                            [ viewChart simulation.rmsdValues "RMSD"
                            , Html.textarea
                                [ readonly True, class "textarea", rows 7 ]
                                [ Html.text <|
                                    String.join ", " <|
                                        List.map String.fromFloat simulation.rmsdValues
                                ]
                            ]
                        ]
                    , Html.tr
                        []
                        [ Html.th [] [ Html.text "RMSF Values" ]
                        , Html.td []
                            [ viewChart simulation.rmsfValues "RMSF"
                            , Html.textarea
                                [ readonly True, class "textarea", rows 7 ]
                                [ Html.text <|
                                    String.join ", " <|
                                        List.map String.fromFloat simulation.rmsfValues
                                ]
                            ]
                        ]
                    ]
                ]

        viewChart vals xLabel =
            let
                data =
                    List.indexedMap
                        (\i val -> { x = toFloat i, y = val })
                        vals
            in
            Chart.chart
                [ CA.height 100
                , CA.width 200
                , CA.margin { top = 10, bottom = 10, left = 10, right = 10 }
                ]
                [ Chart.xLabels []
                , Chart.yLabels [ CA.withGrid ]
                , Chart.series .x [ Chart.interpolated .y [] [] ] data
                ]

        viewReplicate : Maybe Int -> Maybe Int -> String
        viewReplicate replicate totalReplicates =
            let
                rep =
                    case replicate of
                        Just r ->
                            String.fromInt r

                        _ ->
                            "NA"

                tot =
                    case totalReplicates of
                        Just t ->
                            " of " ++ String.fromInt t

                        _ ->
                            ""
            in
            rep ++ tot

        viewBiomolecule : Biomolecule -> Html.Html Msg
        viewBiomolecule val =
            let
                uniprotLink =
                    case val.uniprotId of
                        Just id ->
                            Html.a
                                [ href <| "https://www.uniprot.org/uniprotkb/" ++ id ++ "/entry" ]
                                [ Html.text id ]

                        _ ->
                            Html.text "NA"
            in
            Html.tr []
                [ Html.td [] [ Html.text val.name ]
                , Html.td [] [ uniprotLink ]
                ]

        viewUnvalidatedBiomolecule : UnvalidatedBiomolecule -> Html.Html Msg
        viewUnvalidatedBiomolecule val =
            let
                molType =
                    case val.moleculeIdType of
                        Just t ->
                            " (" ++ t ++ ")"

                        _ ->
                            ""
            in
            Html.li [] [ Html.text <| val.moleculeId ++ molType ]

        viewLigand : Ligand -> Html.Html Msg
        viewLigand ligand =
            Html.li [] [ Html.text ligand.name ]

        viewContribution : Contribution -> Html.Html Msg
        viewContribution contribution =
            Html.li [] [ Html.text contribution.name ]

        viewSolvent : Solvent -> Html.Html Msg
        viewSolvent solvent =
            Html.li []
                [ Html.text <|
                    solvent.name
                        ++ " ("
                        ++ String.fromFloat solvent.concentration
                        ++ solvent.concentrationUnits
                        ++ ")"
                ]

        viewPaper paper =
            let
                doiPrefix =
                    "https://doi.org/"

                doi =
                    if String.isEmpty paper.doi then
                        []

                    else
                        [ Html.a [ href <| doiPrefix ++ paper.doi ]
                            [ Html.text <| doiPrefix ++ paper.doi ]
                        ]

                pages =
                    case paper.pages of
                        Just p ->
                            [ Html.text <| ", " ++ p ]

                        _ ->
                            []

                number =
                    case paper.number of
                        Just n ->
                            "(" ++ n ++ ")"

                        _ ->
                            ""
            in
            List.concat <|
                [ [ Html.text <|
                        paper.authors
                            ++ " ("
                            ++ String.fromInt paper.year
                            ++ "). "
                            ++ paper.title
                            ++ ". "
                  , Html.em []
                        [ Html.text <|
                            paper.journal
                                ++ ", "
                                ++ String.fromInt paper.volume
                                ++ number
                        ]
                  ]
                , pages
                , [ Html.text ". " ]
                , doi
                ]

        processedFilesTable =
            Html.div []
                [ Html.h2 [] [ Html.text "Processed Files" ]
                , Html.button
                    [ class "button"
                    , disabled (List.isEmpty selectedProcessedFileIds)
                    , onClick CreateDownloadInstance
                    ]
                    [ Html.text "Download" ]
                , Html.table [ class "table" ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th []
                                [ Html.input
                                    [ type_ "checkbox"
                                    , onCheck ToggleAllProcessedFiles
                                    ]
                                    []
                                ]
                            , Html.th [] [ Html.text "Name" ]
                            , Html.th [] [ Html.text "File Type" ]
                            , Html.th [] [ Html.text "File Size" ]
                            , Html.th [] [ Html.text "Description" ]
                            ]
                        ]
                    , Html.tbody
                        []
                        (List.map (viewProcessedFile selectedProcessedFileIds) simulation.processedFiles)
                    ]
                ]

        formatSettings =
            { units = Filesize.Base10
            , decimalPlaces = 2
            , decimalSeparator = "."
            }

        viewCreationDate : Time.Posix -> String
        viewCreationDate creationDate =
            let
                month =
                    case Time.toMonth Time.utc creationDate of
                        Time.Jan ->
                            "01"

                        Time.Feb ->
                            "02"

                        Time.Mar ->
                            "03"

                        Time.Apr ->
                            "04"

                        Time.May ->
                            "05"

                        Time.Jun ->
                            "06"

                        Time.Jul ->
                            "07"

                        Time.Aug ->
                            "08"

                        Time.Sep ->
                            "09"

                        Time.Oct ->
                            "10"

                        Time.Nov ->
                            "11"

                        Time.Dec ->
                            "12"
            in
            String.join "-"
                [ String.fromInt (Time.toYear Time.utc creationDate)
                , month
                , String.fromInt (Time.toDay Time.utc creationDate)
                ]

        viewProcessedFile : List Int -> ProcessedFile -> Html.Html Msg
        viewProcessedFile selectedFileIds file =
            Html.tr []
                [ Html.td []
                    [ Html.input
                        [ type_ "checkbox"
                        , checked (List.member file.id selectedFileIds)
                        , onCheck (ToggleProcessedFile file.id)
                        ]
                        []
                    ]
                , Html.td [] [ Html.text file.localFileName ]
                , Html.td [] [ Html.text file.fileType ]
                , Html.td [ align "right" ]
                    [ Html.text
                        (Filesize.formatWith formatSettings
                            (Maybe.withDefault 0 file.fileSizeBytes)
                        )
                    ]
                , Html.td [] [ Html.text (Maybe.withDefault "" file.description) ]
                ]

        viewUploadedFile : UploadedFile -> Html.Html Msg
        viewUploadedFile file =
            Html.tr []
                [ Html.td [] [ Html.input [ type_ "checkbox" ] [] ]
                , Html.td [] [ Html.text file.filename ]
                , Html.td [] [ Html.text file.fileType ]
                , Html.td [ align "right" ]
                    [ Html.text
                        (Filesize.formatWith formatSettings
                            (Maybe.withDefault 0 file.fileSizeBytes)
                        )
                    ]
                , Html.td [] [ Html.text (Maybe.withDefault "" file.description) ]
                ]

        uploadedFilesTable =
            Html.div []
                [ Html.h2 [] [ Html.text "Uploaded Files" ]
                , Html.table [ class "table" ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th [] []
                            , Html.th [] [ Html.text "Name" ]
                            , Html.th [] [ Html.text "File Type" ]
                            , Html.th [] [ Html.text "File Size" ]
                            , Html.th [] [ Html.text "Description" ]
                            ]
                        ]
                    , Html.tbody
                        []
                        (List.map viewUploadedFile simulation.uploadedFiles)
                    ]
                ]
    in
    Html.div [ class "container content" ]
        [ Html.div [ class "box" ]
            [ Html.h1 [ class "title is-1" ] [ Html.text ("Simulation " ++ simulation.slug) ]
            , Html.div [ class "columns" ]
                [ Html.div [ class "column" ] [ tbl1 ]
                , Html.div [ class "column" ]
                    [ Html.img
                        [ src <| shared.mediaHost ++ "/" ++ simulation.guid ++ "/preview.gif" ]
                        []
                    ]
                ]
            , tbl2
            , processedFilesTable
            , uploadedFilesTable
            ]
        ]
