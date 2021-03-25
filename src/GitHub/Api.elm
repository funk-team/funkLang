module GitHub.Api exposing (..)

{-| Talk to the GitHub API
-}

import Element as El exposing (..)
import GitHub.Model
import Http exposing (Error(..))
import Json.Decode.Field as Field exposing (..)
import Model.Product
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Url.Builder



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
