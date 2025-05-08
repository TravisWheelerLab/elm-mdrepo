module Main.Pages.Msg exposing (Msg(..))

import Pages.Home_
import Pages.About
import Pages.Explore
import Pages.Explore.Id_
import Pages.Metadata
import Pages.Profile
import Pages.SignIn
import Pages.Uploads
import Pages.NotFound_


type Msg
    = Home_ Pages.Home_.Msg
    | About Pages.About.Msg
    | Explore Pages.Explore.Msg
    | Explore_Id_ Pages.Explore.Id_.Msg
    | Metadata Pages.Metadata.Msg
    | Profile Pages.Profile.Msg
    | SignIn Pages.SignIn.Msg
    | Uploads Pages.Uploads.Msg
    | NotFound_ Pages.NotFound_.Msg
