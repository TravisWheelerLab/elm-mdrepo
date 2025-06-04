module Shared.Msg exposing (Msg(..))

{-| -}

import Http
import Types exposing (User)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = NoOp
    | Login (Maybe User)
    | Logout
    | GotUser (Result Http.Error (List User))
    | SetErrorMessage (Maybe String)
