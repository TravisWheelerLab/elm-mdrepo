module Types exposing (Profile)


type alias Profile =
    { firstName : String
    , lastName : String
    , fullName : String
    , email : String
    , institution : Maybe String
    , isSuperuser : Maybe Bool
    , isStaff : Maybe Bool
    , orcid : Maybe String
    , canContribute : Maybe Bool
    }
