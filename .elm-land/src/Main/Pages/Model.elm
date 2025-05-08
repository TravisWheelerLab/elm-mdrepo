module Main.Pages.Model exposing (Model(..))

import Pages.Home_
import Pages.About
import Pages.Explore
import Pages.Explore.Id_
import Pages.Metadata
import Pages.Profile
import Pages.SignIn
import Pages.Uploads
import Pages.NotFound_
import View exposing (View)


type Model
    = Home_ Pages.Home_.Model
    | About Pages.About.Model
    | Explore Pages.Explore.Model
    | Explore_Id_ { id : String } Pages.Explore.Id_.Model
    | Metadata Pages.Metadata.Model
    | Profile Pages.Profile.Model
    | SignIn Pages.SignIn.Model
    | Uploads Pages.Uploads.Model
    | NotFound_ Pages.NotFound_.Model
    | Redirecting_
    | Loading_
