module Components.Header exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, src, width)
import Route.Path
import View exposing (View)


view :
    { title : String
    , body : List (Html msg)
    }
    -> View msg
view props =
    { title = props.title
    , body =
        [ Html.div [ class "hero py-6 has-text-centered" ]
            [ Html.img [ src "/mdrepo.png", width 200 ] []
            , Html.a [ Route.Path.href Route.Path.Explore ]
                [ Html.text "Explore" ]
            ]
        , div [ class "page" ] props.body
        ]
    }
