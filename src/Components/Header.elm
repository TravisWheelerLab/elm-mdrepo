module Components.Header exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (attribute, class, href, src, width)
import Route.Path
import Shared
import View exposing (View)


view :
    { title : String
    , body : List (Html msg)
    , shared : Shared.Model
    }
    -> View msg
view props =
    let
        errorMessage =
            case props.shared.errorMessage of
                Just error ->
                    Html.section
                        [ class "hero is-warning is-small" ]
                        [ Html.div [ class "hero-body" ]
                            [ Html.p [ class "title" ] [ Html.text "Error" ]
                            , Html.p [ class "subtitle" ] [ Html.text error ]
                            ]
                        ]

                _ ->
                    Html.div [] []
    in
    { title = props.title
    , body =
        let
            userMenu =
                case props.shared.user of
                    Just user ->
                        Html.div
                            [ class "navbar-item has-dropdown is-hoverable" ]
                            [ Html.a [ class "navbar-link" ] [ Html.text user.fullName ]
                            , Html.div
                                [ class "navbar-dropdown" ]
                                [ Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.Profile ]
                                        [ Html.text "Profile" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.Uploads ]
                                        [ Html.text "Uploads" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.UploadLogs ]
                                        [ Html.text "Upload Logs" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.UploadTokens ]
                                        [ Html.text "Upload Tokens" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.Downloads ]
                                        [ Html.text "Download Tokens" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.MySimulations ]
                                        [ Html.text "My Simulations" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.Metadata ]
                                        [ Html.text "Create Metadata" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.Logout ]
                                        [ Html.text "Logout" ]
                                    ]
                                ]
                            ]

                    _ ->
                        Html.div
                            [ class "navbar-item" ]
                            [ Html.a
                                [ class "button is-primary"
                                , Route.Path.href Route.Path.Profile
                                ]
                                [ Html.text "Login" ]
                            ]
        in
        [ Html.div [ class "container" ]
            [ Html.nav
                [ class "navbar"
                , attribute "role" "navigation"
                , attribute "aria-label" "main navigation"
                ]
                [ Html.div
                    [ class "navbar-brand" ]
                    [ Html.a
                        --[ Route.Path.href Route.Path.Home_ ]
                        [ href "/" ]
                        [ Html.img [ src "/mdrepo.png", width 200 ] [] ]
                    ]
                , Html.div [ class "navbar-menu" ]
                    [ Html.div [ class "navbar-end" ]
                        [ Html.div
                            [ class "navbar-item" ]
                            [ Html.a
                                [ class "button"
                                , Route.Path.href Route.Path.About
                                ]
                                [ Html.text "About" ]
                            ]
                        , Html.div
                            [ class "navbar-item" ]
                            [ Html.a
                                [ class "button", Route.Path.href Route.Path.Explore ]
                                [ Html.text "Explore" ]
                            ]
                        , userMenu
                        ]
                    ]
                ]
            ]
        , errorMessage
        , Html.div
            [ class "page" ]
            props.body
        ]
    }
