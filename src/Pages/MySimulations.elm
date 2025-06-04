module Pages.MySimulations exposing (Model, Msg, page)

import Api
import Components.Header
import Decoders exposing (paperDecoder, processedFileDecoder, replicateGroupDecoder, unvalidatedBiomoleculeDecoder, uploadedFileDecoder, userDecoder)
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
import Types exposing (Paper, ProcessedFile, ReplicateGroup, UploadedFile, User)
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
    { simulationsResult : Maybe SimulationsResult }


type alias SimulationsResult =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List Simulation
    }


simulationsResultDecoder : Decoder SimulationsResult
simulationsResultDecoder =
    Decode.succeed SimulationsResult
        |> required "count" int
        |> required "next" (nullable string)
        |> required "previous" (nullable string)
        |> required "results" (Decode.list simulationDecoder)


type alias Simulation =
    { mdRepoId : String
    , guid : String
    , slug : String
    , isPlaceholder : Bool
    , isDeprecated : Bool
    , isRestricted : Bool
    , description : Maybe String
    , shortDescription : Maybe String
    , externalLink : Maybe String
    , includesWater : Maybe Bool
    , runCommands : Maybe String
    , waterType : Maybe String
    , waterDensity : Maybe Int
    , duration : Maybe Int
    , samplingFrequence : Maybe Int
    , integrationTimestepFs : Maybe Int
    , software : Maybe Software
    , ligands : Maybe String
    , contributions : Maybe String
    , biomolecules : List Biomolecule
    , creationDate : Maybe String

    {-
       , unvalidatedBiomolecules : List UnvalidatedBiomolecule
       , solvents : String
       , papers : List Paper
       , rmsdValues : List Float
       , rmsfValues : List Float
       , createdBy : User
       , replicate : Int
       , total_replicates : Int
       , replicateGroup : ReplicateGroup
       , forceField : String
       , forceFieldComments : String
       , uploadedFiles : List UploadedFile
       , processedFiles : List ProcessedFile
       , displayTrajectoryFile : ProcessedFile
       , displayStructureFile : ProcessedFile
       , fastaSequence : String
       , temperature : Int
       , protonationMethod : String
       , displayTrajectoryFileNFrames : Int
       , canEdit : Bool
    -}
    }


simulationDecoder : Decoder Simulation
simulationDecoder =
    Decode.succeed Simulation
        |> required "md_repo_id" string
        |> required "guid" string
        |> required "slug" string
        |> required "is_placeholder" bool
        |> required "is_deprecated" bool
        |> required "is_restricted" bool
        |> optional "description" (nullable string) Nothing
        |> optional "short_description" (nullable string) Nothing
        |> optional "external_link" (nullable string) Nothing
        |> optional "includes_water" (nullable bool) Nothing
        |> optional "run_commands" (nullable string) Nothing
        |> optional "water_type" (nullable string) Nothing
        |> optional "water_density" (nullable int) Nothing
        |> optional "duration" (nullable int) Nothing
        |> optional "sampling_frequency" (nullable int) Nothing
        |> optional "integration_timestep_fs" (nullable int) Nothing
        |> optional "software" (nullable softwareDecoder) Nothing
        |> optional "ligands" (nullable string) Nothing
        |> optional "contributions" (nullable string) Nothing
        |> optional "biomolecules" (list biomoleculeDecoder) []
        |> optional "creation_date" (nullable string) Nothing



{-
   |> required "unvalidated_biomolecules" (list unvalidatedBiomoleculeDecoder)
   |> required "solvents" string
   |> required "papers" (list paperDecoder)
   |> required "rmsd_values" (list float)
   |> required "rmsf_values" (list float)
   |> required "created_by" userDecoder
   |> required "replicate" int
   |> required "total_replicates" int
   |> required "replicate_group" replicateGroupDecoder
   |> required "force_field" string
   |> required "force_field_comments" string
   |> required "uploaded_files" (list uploadedFileDecoder)
   |> required "processed_files" (list processedFileDecoder)
   |> required "display_trajectory_file" processedFileDecoder
   |> required "display_structure_file" processedFileDecoder
   |> required "fasta_sequence" string
   |> required "temperature" int
   |> required "protonation_method" string
   |> required "display_trajectory_file_n_frames" int
   |> required "can_edit" bool
-}


type alias Software =
    { name : String
    , version : String
    }


softwareDecoder : Decoder Software
softwareDecoder =
    Decode.succeed Software
        |> required "name" string
        |> required "version" string


type alias Biomolecule =
    { name : String
    , aminoLength : Int
    , sequence : String
    , uniprotId : Maybe String
    , pdbId : Maybe String
    , primaryMoleculeIdType : String
    }


biomoleculeDecoder : Decoder Biomolecule
biomoleculeDecoder =
    Decode.succeed Biomolecule
        |> required "name" string
        |> required "amino_length" int
        |> required "sequence" string
        |> required "uniprot_id" (nullable string)
        |> required "pdb_id" (nullable string)
        |> required "primary_molecule_id_type" string


type alias UnvalidatedBiomolecule =
    { moleculeId : String
    , moleculeIdType : String
    }


unlvalidatedBiomoleculeDecoder : Decoder UnvalidatedBiomolecule
unlvalidatedBiomoleculeDecoder =
    Decode.succeed UnvalidatedBiomolecule
        |> required "molecule_id" string
        |> required "molecule_id_type" string


initialModel : Model
initialModel =
    { simulationsResult = Nothing }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = shared.apiHost ++ "/getUserSimulations"
            , expect = Http.expectJson GotSimulations simulationsResultDecoder
            }
    )



-- UPDATE


type Msg
    = GotSimulations (Result Http.Error SimulationsResult)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        GotSimulations (Ok result) ->
            ( { model | simulationsResult = Just result }
            , Effect.none
            )

        GotSimulations (Err err) ->
            ( { model | simulationsResult = Nothing }
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
        { title = "Pages.MySimulations"
        , body = viewModel shared model
        , shared = shared
        }


viewModel : Shared.Model -> Model -> List (Html.Html Msg)
viewModel shared model =
    let
        table =
            case model.simulationsResult of
                Just result ->
                    simulationsTable result.results

                _ ->
                    Html.text "Unable to get data"
    in
    [ Html.div
        [ class "content" ]
        [ Html.h1 [ class "title" ] [ Html.text "My Simulations" ]
        , table
        ]
    ]


simulationsTable : List Simulation -> Html.Html Msg
simulationsTable simulations =
    let
        viewSimulation sim =
            if sim.isPlaceholder then
                Html.text ""

            else
                Html.a
                    [ Route.Path.href (Route.Path.Explore_Id_ { id = sim.mdRepoId }) ]
                    [ Html.text sim.slug ]

        mkRow simulation =
            Html.tr []
                [ Html.td [] [ Html.text simulation.guid ]

                --[ Html.td [] [ viewSimulation simulation.simulation ]
                --, Html.td [] [ Html.text simulation.creationDate ]
                ]
    in
    case List.length simulations of
        0 ->
            Html.text "No data"

        _ ->
            Html.table
                [ class "table" ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "Simulation" ]

                        --, Html.th [] [ Html.text "Created On" ]
                        ]
                    ]
                , Html.tbody [] (List.map mkRow simulations)
                ]
