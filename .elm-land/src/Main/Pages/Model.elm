module Main.Pages.Model exposing (Model(..))

import Pages.Home_
import Pages.About
import Pages.Explore
import Pages.Explore.Id_
import Pages.NotFound_
import View exposing (View)


type Model
    = Home_ Pages.Home_.Model
    | About Pages.About.Model
    | Explore Pages.Explore.Model
    | Explore_Id_ { id : String } Pages.Explore.Id_.Model
    | NotFound_ Pages.NotFound_.Model
    | Redirecting_
    | Loading_
