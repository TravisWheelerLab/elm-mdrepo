module Components.Header exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (attribute, class, href, src, width)
import Route.Path
import View exposing (View)


view :
    { title : String
    , body : List (Html msg)
    }
    -> View msg
view props =
    let
        loginUri =
            "/api/accounts/orcid/login/"

        logoutUri =
            "/api/accounts/logout/"
    in
    { title = props.title
    , body =
        [ Html.div [ class "container" ]
            [ Html.nav
                [ class "navbar"
                , attribute "role" "navigation"
                , attribute "aria-label" "main navigation"
                ]
                [ Html.div
                    [ class "navbar-brand" ]
                    [ Html.a
                        [ Route.Path.href Route.Path.Home_ ]
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
                        , Html.div
                            [ class "navbar-item" ]
                            [ Html.a
                                [ class "button"
                                , Route.Path.href Route.Path.Profile
                                ]
                                [ Html.text "Profile" ]
                            ]
                        , Html.div
                            [ class "navbar-item" ]
                            [ Html.a
                                [ class "button", href logoutUri ]
                                [ Html.text "Logout" ]
                            ]
                        , Html.div
                            [ class "navbar-item has-dropdown is-hoverable" ]
                            [ Html.a [ class "navbar-link" ] [ Html.text "Contribute" ]
                            , Html.div
                                [ class "navbar-dropdown" ]
                                [ Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.Uploads ]
                                        [ Html.text "Upload Tokens" ]
                                    ]
                                , Html.div
                                    [ class "navbar-item" ]
                                    [ Html.a
                                        [ Route.Path.href Route.Path.Metadata ]
                                        [ Html.text "Create Metadata" ]
                                    ]
                                ]
                            ]
                        , Html.div
                            [ class "navbar-item" ]
                            [ Html.a
                                [ class "button is-primary"
                                , href loginUri

                                --, Route.Path.href Route.Path.SignIn
                                ]
                                [ Html.text "Login" ]
                            ]
                        ]
                    ]
                ]
            ]
        , Html.div [ class "page" ] props.body
        ]
    }
