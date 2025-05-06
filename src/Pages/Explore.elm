module Pages.Explore exposing (Model, Msg, page)

import Api
import Api.SimulationList
import Components.Header
import Config
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, href, selected, src, type_, width)
import Html.Events exposing (onClick, onInput)
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
    }


type alias Simulation =
    { id : Int
    , guid : String
    , slug : String
    , fastaSequence : String
    , shortDescription : Maybe String
    , biomolecules : List Biomolecule
    , ligands : List Ligand
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



-- TODO: Effect.sendApiRequest?


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

        url =
            Config.apiHost
                ++ "/getSimulations?limit="
                ++ String.fromInt model.pageSize
                ++ offset
                ++ search

        -- _ = Debug.log "url" url
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
        |> required "fasta_sequence" string
        |> required "short_description" (nullable string)
        |> required "biomolecules" (Decode.list biomoleculeDecoder)
        |> required "ligands" (Decode.list ligandDecoder)


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



-- UPDATE


type Msg
    = SimulationApiResponded (Result Http.Error ExploreRequest)
    | UpdatePageSize String
    | UpdatePageNumber String
    | UpdateTextSearch String
    | ToggleSimulationSelection Int Bool


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
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
                        , pageNumber = 0
                    }
            in
            ( newModel
            , requestData newModel
            )

        ToggleSimulationSelection simulationId selected ->
            let
                newIds =
                    if selected then
                        List.filter (\id -> id /= simulationId) model.selectedSimulationIds

                    else
                        model.selectedSimulationIds ++ [ simulationId ]
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
        { title = "Simulations"
        , body =
            [ case model.simulations of
                RemoteData.Success simulations ->
                    Html.div []
                        [ pagination model
                        , viewSimulations simulations
                        ]

                RemoteData.Failure err ->
                    Html.div [ class "has-text-centered p-6" ]
                        [ Html.text <| Api.toUserFriendlyMessage err ]

                _ ->
                    Html.div [ class "has-text-centered p-6" ]
                        [ Html.text "Loading..." ]
            ]
        }


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
            Html.select [ onInput UpdatePageNumber ] options

        pageSizeNav =
            let
                options =
                    List.map
                        (\num ->
                            Html.option [ selected (model.pageSize == num) ]
                                [ Html.text (String.fromInt num) ]
                        )
                        [ 10, 25, 50 ]
            in
            Html.select [ onInput UpdatePageSize ] options
    in
    Html.div [ class "container" ]
        [ Html.div [ class "columns" ]
            [ Html.div [ class "column" ]
                [ Html.text
                    ("Count: "
                        ++ String.fromInt recordCount
                    )
                ]
            , Html.div [ class "column" ]
                [ Html.text "Search: "
                , Html.input [ onInput UpdateTextSearch ] []
                ]
            , Html.div [ class "column" ] [ Html.text "Show: ", pageSizeNav ]
            , Html.div [ class "column" ]
                [ Html.text "Go To Page: "
                , pageNav
                ]
            ]
        ]


viewSimulations : List Simulation -> Html Msg
viewSimulations simulations =
    let
        header =
            Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Select" ]
                    , Html.th [] [ Html.text "Index" ]
                    , Html.th [] [ Html.text "Thumbnail" ]
                    , Html.th [] [ Html.text "Description" ]
                    , Html.th [] [ Html.text "MDRepo ID" ]
                    , Html.th [] [ Html.text "Biomolecules" ]
                    , Html.th [] [ Html.text "Ligands" ]
                    , Html.th [] [ Html.text "Sequence" ]
                    ]
                ]

        rows =
            Html.tbody []
                (List.indexedMap
                    viewSimulation
                    simulations
                )
    in
    Html.div [ class "container py-6 p-5" ]
        [ Html.div [ class "columns is-multiline" ]
            [ Html.table
                [ class "table is-striped" ]
                [ header, rows ]
            ]
        ]


viewSimulation : Int -> Simulation -> Html Msg
viewSimulation index simulation =
    Html.tr []
        [ Html.td []
            [ Html.input
                [ type_ "checkbox"
                , onClick ToggleSimulationSelection simulation.id
                ]
                []
            ]
        , Html.td []
            [ Html.text <| String.fromInt (index + 1)
            ]
        , Html.td []
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
        , Html.td []
            [ viewLigands simulation.ligands
            ]
        , Html.td []
            [ Html.text <| truncate 30 simulation.fastaSequence
            ]
        ]


truncate : Int -> String -> String
truncate len val =
    if String.length val > len then
        String.left (len - 3) val ++ "..."

    else
        val


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
