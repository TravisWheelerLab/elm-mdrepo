module Config exposing (..)

--host =
--    "https://staging.mdrepo.org"


apiHostName =
    "http://localhost"


apiHost =
    apiHostName ++ "/api/v1"


loginUrl =
    apiHostName ++ "/api/accounts/orcid/login/"


logoutUrl =
    apiHostName ++ "/api/accounts/logout/"



--"https://test.mdrepo.org/api/v1"


mediaHost =
    "https://assets.mdrepo.org"
