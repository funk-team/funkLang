module GitHub.Api exposing (..)

{-| Talk to the GitHub API
-}

import Element as El exposing (..)
import GitHub.Model
import Http exposing (Error(..))
import Json.Decode.Field as Field exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Url
import Url.Builder



-- getIconList


getRepo : GitHub.Model.GitHubRepoId -> Cmd (WebData GitHub.Model.Repo)
getRepo githubRepoId =
    RemoteData.Http.get
        ("/api_server/iconpack/" ++ GitHub.Model.githubIdToPath githubRepoId)
        identity
        (GitHub.Model.repoReader githubRepoId)


makeUrl : String -> String -> String -> String
makeUrl content_type repo sha =
    Url.Builder.absolute
        [ "api_server", "iconpack", "repo" ]
        []


treeUrl : String -> String -> String
treeUrl repo sha =
    makeUrl "git/trees" repo sha


blobUrl : String -> String -> String
blobUrl repo sha =
    makeUrl "git/blob" repo sha
