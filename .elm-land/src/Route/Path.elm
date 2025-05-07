module Route.Path exposing (Path(..), fromString, fromUrl, href, toString)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser exposing ((</>))


type Path
    = Home_
    | About
    | Explore
    | Explore_Id_ { id : String }
    | Profile
    | SignIn
    | NotFound_


fromUrl : Url -> Path
fromUrl url =
    fromString url.path
        |> Maybe.withDefault NotFound_


fromString : String -> Maybe Path
fromString urlPath =
    let
        urlPathSegments : List String
        urlPathSegments =
            urlPath
                |> String.split "/"
                |> List.filter (String.trim >> String.isEmpty >> Basics.not)
    in
    case urlPathSegments of
        [] ->
            Just Home_

        "about" :: [] ->
            Just About

        "explore" :: [] ->
            Just Explore

        "explore" :: id_ :: [] ->
            Explore_Id_
                { id = id_
                }
                |> Just

        "profile" :: [] ->
            Just Profile

        "sign-in" :: [] ->
            Just SignIn

        _ ->
            Nothing


href : Path -> Html.Attribute msg
href path =
    Html.Attributes.href (toString path)


toString : Path -> String
toString path =
    let
        pieces : List String
        pieces =
            case path of
                Home_ ->
                    []

                About ->
                    [ "about" ]

                Explore ->
                    [ "explore" ]

                Explore_Id_ params ->
                    [ "explore", params.id ]

                Profile ->
                    [ "profile" ]

                SignIn ->
                    [ "sign-in" ]

                NotFound_ ->
                    [ "404" ]
    in
    pieces
        |> String.join "/"
        |> String.append "/"
