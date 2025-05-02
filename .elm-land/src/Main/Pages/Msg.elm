module Main.Pages.Msg exposing (Msg(..))

import Pages.Home_
import Pages.Explore
import Pages.Explore.Id_
import Pages.NotFound_


type Msg
    = Home_ Pages.Home_.Msg
    | Explore Pages.Explore.Msg
    | Explore_Id_ Pages.Explore.Id_.Msg
    | NotFound_ Pages.NotFound_.Msg
