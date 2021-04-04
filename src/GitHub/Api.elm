module GitHub.Api exposing (..)

{-| Talk to the GitHub API
-}

import GitHub.Model
import Model.Product
import RemoteData exposing (WebData)
import RemoteData.Http



-- getIconList


prod =
    "https://editor.funklang.com"


getRepo : Model.Product.Mode -> GitHub.Model.GitHubRepoId -> Cmd (WebData GitHub.Model.Repo)
getRepo mode githubRepoId =
    RemoteData.Http.get
        ((case mode of
            Model.Product.Core ->
                prod

            Model.Product.Enterprise ->
                ""
         )
            ++ "/api_server/iconpack/"
            ++ GitHub.Model.githubIdToPath githubRepoId
        )
        identity
        (GitHub.Model.repoReader githubRepoId)
