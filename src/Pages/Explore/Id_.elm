module Pages.Explore.Id_ exposing (Model, Msg, page)

import Api
import Components.Header
import Config
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class, src, width)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Layouts
import Maybe exposing (..)
import Page exposing (Page)
import RemoteData exposing (RemoteData, WebData)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Simulation =
    { id : Int
    , mdRepoId : String
    , guid : String
    , slug : String
    , description : Maybe String
    }



{-
   type alias Simulation =
       { id : String
       , mdRepoId : String
       , guid : String
       , isPlaceholder : Bool
       , isRestricted : Bool
       , description : String
       , shortDescription : String
       , externalLink : Maybe String
       , includesWater : Bool
       , runCommands : Maybe String
       , waterType : String
       , waterDensity : Maybe Float
       , duration : Float
       , samplingFrequence : Float
       , integrationTimestepFs : Float
       , software : SimulationSoftware
       , ligands : List String
       , contributions : List String
       , biomolecules : List Biomolecule
       , unvalidatedBiomolecules : List Biomolecule
       , solvents : List String
       , papers : List String
       , slug : String
       , rmsdValues : List Float
       , rmsfValues : List Float
       , createdBy : CreatedBy
       , replicate : Int
       , totalReplicates : Int
       , creationDate : String
       , replicateGroup : ReplicateGroup
       , forceField : Maybe String
       , forceFieldComments : Maybe String
       , uploadedFiles : List UploadedFile
       , processedFiles : List ProcessedFile
       , displayTrajectoryFile : ProcessedFile
       , displayStructureFile : ProcessedFile
       , fastaSequence : String
       , temperature : Int
       , protonationMethod : Maybe String
       , displayTrajectoryFileNFrames : Int
       , canEdit : Bool
       }


   type alias SimulationSoftware =
       { name : String
       , version : Maybe String
       }


   type alias Biomolecule =
       { moleculeId : String
       , moleculeIdType : String
       }


   type alias CreatedBy =
       { firstName : String
       , lastName : String
       , fullName : String
       , email : String
       , institution : Maybe String
       , isSuperuser : Bool
       , isStaff : Bool
       , orcid : Maybe String
       , canContribute : Bool
       }


   type alias ReplicateGroup =
       { psfHash : String
       , simulationSet : List String
       }



   type alias UploadedFile =
       { id : String
       , primary : Bool
       , filename : String
       , fileType : String
       , description : String
       , fileSizeBytes : Int
       }


   type alias ProcessedFile =
       { id : String
       , isPrimary : Bool
       , fileType : String
       , localFilename : String
       , localFilePath : String
       , description : String
       , fileSizeBytes : Int
       , public : Bool
       }
-}


type alias Model =
    { simulationData : WebData Simulation }


init : Route { id : String } -> () -> ( Model, Effect Msg )
init route _ =
    ( { simulationData = RemoteData.NotAsked }
    , Effect.sendCmd <|
        Http.get
            { url = Config.host ++ "/api/v1/getSimulations/" ++ route.params.id
            , expect = Http.expectJson SimulationApiResponded simulationDecoder
            }
    )



{- decoder : Json.Decode.Decoder Simulation
   decoder =
       Json.Decode.map Simulation
           (Json.Decode.field "id" Json.Decode.int)
-}


simulationDecoder : Decoder Simulation
simulationDecoder =
    Decode.succeed Simulation
        |> required "id" int
        |> required "md_repo_id" string
        |> required "guid" string
        |> required "slug" string
        |> required "description" (nullable string)



--|> required "email" (nullable string) -- `null` decodes to `Nothing`
--|> optional "name" string "(fallback if name is `null` or not present)"
-- UPDATE


type Msg
    = SimulationApiResponded (Result Http.Error Simulation)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SimulationApiResponded (Ok simulation) ->
            ( { model | simulationData = RemoteData.Success simulation }
            , Effect.none
            )

        SimulationApiResponded (Err err) ->
            ( { model | simulationData = RemoteData.Failure err }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        --_ = Debug.log "model" model
        body =
            case model.simulationData of
                RemoteData.NotAsked ->
                    Html.text "Not asked"

                RemoteData.Loading ->
                    Html.text "Loading"

                RemoteData.Success simulation ->
                    viewSimulation simulation

                RemoteData.Failure err ->
                    Html.text <| "Got error: " ++ Api.toUserFriendlyMessage err
    in
    Components.Header.view
        { title = "View Simulation"
        , body = [ body ]
        }


viewSimulation : Simulation -> Html.Html msg
viewSimulation simulation =
    Html.div []
        [ Html.h1 [ class "title is-1" ] [ Html.text ("View Simulation " ++ simulation.slug) ]
        , Html.table []
            [ Html.thead []
                []
            , Html.tbody
                []
                [ Html.tr []
                    [ Html.th []
                        [ Html.text "ID" ]
                    , Html.td
                        []
                        [ Html.text (String.fromInt simulation.id) ]
                    ]
                , Html.tr []
                    [ Html.th []
                        [ Html.text "MD Repo ID" ]
                    , Html.td
                        []
                        [ Html.text simulation.mdRepoId ]
                    ]
                , Html.tr []
                    [ Html.th []
                        [ Html.text "Desc" ]
                    , Html.td
                        []
                        [ Html.text <| withDefault "NA" simulation.description ]
                    ]
                , Html.tr []
                    [ Html.th []
                        [ Html.text "Thumbnail" ]
                    , Html.td
                        []
                        [ Html.img
                            [ src <| Config.mediaHost ++ "/" ++ simulation.guid ++ "/thumbnail.png"
                            , width 200
                            ]
                            []
                        ]
                    ]
                , Html.tr []
                    [ Html.th []
                        [ Html.text "Preview" ]
                    , Html.td
                        []
                        [ Html.img
                            [ src <| Config.mediaHost ++ "/" ++ simulation.guid ++ "/preview.gif"
                            , width 200
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]
