module Pages.Explore exposing (Model, Msg, page)

import Api
import Api.SimulationList
import Components.Header
import Config
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, src, width)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
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


type alias Simulation =
    { id : Int
    , guid : String
    , slug : String
    , shortDescription : Maybe String
    , biomolecules : List Biomolecule
    }


type alias Biomolecule =
    { name : String
    , aminoLength : Int
    , sequence : String
    , uniprotId : Maybe String
    , pdbId : Maybe String
    , primaryMoleculeIdType : String
    }


type alias Model =
    { simulationData : WebData (List Simulation)
    }



-- TODO: Effect.sendApiRequest?


init : () -> ( Model, Effect Msg )
init () =
    ( { simulationData = RemoteData.NotAsked }
    , Effect.sendCmd <|
        Http.get
            { url = Config.host ++ "/api/v1/getSimulations"
            , expect =
                Http.expectJson
                    SimulationApiResponded
                    decoder
            }
    )


decoder : Decoder (List Simulation)
decoder =
    Decode.field "results" (Decode.list simulationDecoder)


simulationDecoder : Decoder Simulation
simulationDecoder =
    Decode.succeed Simulation
        |> required "id" int
        |> required "guid" string
        |> required "slug" string
        |> required "short_description" (nullable string)
        |> required "biomolecules" (Decode.list biomoleculeDecoder)


biomoleculeDecoder : Decoder Biomolecule
biomoleculeDecoder =
    Decode.succeed Biomolecule
        |> required "name" string
        |> required "amino_length" int
        |> required "sequence" string
        |> required "uniprot_id" (nullable string)
        |> required "pdb_id" (nullable string)
        |> required "primary_molecule_id_type" string



-- UPDATE


type Msg
    = SimulationApiResponded (Result Http.Error (List Simulation))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        _ =
            Debug.log "resp" msg
    in
    case msg of
        SimulationApiResponded (Ok simulations) ->
            ( { model | simulationData = RemoteData.Success simulations }
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
    Components.Header.view
        { title = "Simulations"
        , body =
            [ case model.simulationData of
                RemoteData.Success simulations ->
                    viewSimulationList simulations

                RemoteData.Failure err ->
                    Html.div [ class "has-text-centered p-6" ]
                        [ Html.text <| Api.toUserFriendlyMessage err ]

                _ ->
                    Html.div [ class "has-text-centered p-6" ]
                        [ Html.text "Loading..." ]
            ]
        }


viewSimulationList : List Simulation -> Html Msg
viewSimulationList simulations =
    let
        header =
            Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Thumbnail" ]
                    , Html.th [] [ Html.text "Description" ]
                    , Html.th [] [ Html.text "MDRepo ID" ]
                    , Html.th [] [ Html.text "Biomolecules" ]
                    ]
                ]

        rows =
            Html.tbody []
                (List.map
                    viewSimulation
                    simulations
                )
    in
    Html.div [ class "container py-6 p-5" ]
        [ Html.div [ class "columns is-multiline" ]
            [ Html.table
                []
                [ header, rows ]
            ]
        ]


viewSimulation : Simulation -> Html Msg
viewSimulation simulation =
    Html.tr []
        [ Html.td []
            [ Html.img
                [ src <| Config.mediaHost ++ "/" ++ simulation.guid ++ "/thumbnail.png"
                , width 200
                ]
                []
            ]
        , Html.td []
            [ Html.text <| Maybe.withDefault "NA" simulation.shortDescription
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
        ]


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
