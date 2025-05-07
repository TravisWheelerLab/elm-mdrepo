module Main.Pages.Msg exposing (Msg(..))

import Pages.Home_
import Pages.About
import Pages.Explore
import Pages.Explore.Id_
import Pages.Profile
import Pages.SignIn
import Pages.NotFound_


type Msg
    = Home_ Pages.Home_.Msg
    | About Pages.About.Msg
    | Explore Pages.Explore.Msg
    | Explore_Id_ Pages.Explore.Id_.Msg
    | Profile Pages.Profile.Msg
    | SignIn Pages.SignIn.Msg
    | NotFound_ Pages.NotFound_.Msg
