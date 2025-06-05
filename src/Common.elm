module Common exposing (..)

import Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


copyIcon : msg -> Html.Html msg
copyIcon msg =
    Html.button [ class "button is-small", onClick msg ]
        [ Html.span
            [ class "icon is-small" ]
            [ Html.i [ class "fa-regular fa-copy" ] []
            ]
        ]
