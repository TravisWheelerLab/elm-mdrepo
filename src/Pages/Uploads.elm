module Pages.Uploads exposing (Model, Msg, page)

-- Upload logs: getUploadInstances
-- getUserUploadTickets

import Api
import Auth
import Components.Header
import Decoders exposing (uploadTicketsResultDecoder)
import Effect exposing (Effect, pushRoutePath)
import Html
import Html.Attributes exposing (class)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Types exposing (UploadTicket, UploadTicketSimulation, UploadTicketsResult)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { uploadTickets : Maybe UploadTicketsResult
    }


initialModel : Model
initialModel =
    { uploadTickets = Nothing
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( initialModel
    , Effect.sendCmd <|
        Http.get
            { url = shared.apiHost ++ "/getUserUploadTickets"
            , expect = Http.expectJson GotUploadTickets uploadTicketsResultDecoder
            }
    )



-- UPDATE


type Msg
    = GotUploadTickets (Result Http.Error UploadTicketsResult)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotUploadTickets (Ok result) ->
            ( { model | uploadTickets = Just result }
            , Effect.none
            )

        GotUploadTickets (Err error) ->
            ( { model | uploadTickets = Nothing }
            , Effect.setErrorMessage (Just <| Api.toUserFriendlyMessage error)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        table =
            case model.uploadTickets of
                Just uploadTickets ->
                    uploadTicketsTable uploadTickets.results

                _ ->
                    Html.text "Unable to get data"
    in
    Components.Header.view
        { title = "MDRepo - Upload Logs"
        , body = [ Html.div [ class "container" ] [ table ] ]
        , shared = shared
        }


simLink : UploadTicketSimulation -> Html.Html Msg
simLink sim =
    Html.a
        [ Route.Path.href <| Route.Path.Explore_Id_ { id = sim.mdRepoId } ]
        [ Html.text sim.slug ]


uploadTicketsTable : List UploadTicket -> Html.Html Msg
uploadTicketsTable uploadTickets =
    let
        mkRow ticket =
            Html.tr []
                [ Html.td [] [ Html.text ticket.status ]
                , Html.td [] [ Html.text ticket.createdAt ]
                , Html.td [] (List.map simLink ticket.simulations)
                , Html.td [] [ Html.text "files" ]
                ]
    in
    case List.length uploadTickets of
        0 ->
            Html.text "No tickets"

        _ ->
            Html.table
                [ class "table" ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "Status" ]
                        , Html.th [] [ Html.text "Created On" ]
                        , Html.th [] [ Html.text "Simulation(s)" ]
                        , Html.th [] [ Html.text "Files" ]
                        ]
                    ]
                , Html.tbody [] (List.map mkRow uploadTickets)
                ]
