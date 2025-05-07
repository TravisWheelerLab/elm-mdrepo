module Components.Header exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (attribute, class, src, width)
import Route.Path
import View exposing (View)



{- Html.a [ Route.Path.href Route.Path.Explore ]
   [ Html.text "Explore" ]
-}


view :
    { title : String
    , body : List (Html msg)
    }
    -> View msg
view props =
    { title = props.title
    , body =
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
                    [ Html.a
                        [ class "navbar-item", Route.Path.href Route.Path.About ]
                        [ Html.text "About" ]
                    , Html.a
                        [ class "navbar-item", Route.Path.href Route.Path.Explore ]
                        [ Html.text "Explore" ]
                    , Html.a
                        [ class "navbar-item", Route.Path.href Route.Path.Profile ]
                        [ Html.text "Profile" ]
                    , Html.div
                        [ class "navbar-item" ]
                        [ Html.button [ class "button is-primary" ] [ Html.text "Login" ] ]
                    ]
                ]
            ]
        , Html.div [ class "page" ] props.body
        ]
    }
