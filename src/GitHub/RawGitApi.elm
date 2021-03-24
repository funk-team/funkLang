module GitHub.RawGitApi exposing (..)

{-| Talk to GitHub using RawGit to request raw blobs
-}

import DesignSystem.IconBrowser.Model
import GitHub.Model
import Http
import Url.Builder


{-| Can not use RemoteData.get here because return value is not a JSON
-}
getIconSvg : GitHub.Model.Repo -> DesignSystem.IconBrowser.Model.Icon -> Cmd (Result Http.Error String)
getIconSvg repo (DesignSystem.IconBrowser.Model.Icon icon) =
    Http.get
        { url = makeUrl repo icon, expect = Http.expectString identity }


rawGit_baseUrl =
    "https://rawcdn.githack.com"


makeUrl : GitHub.Model.Repo -> GitHub.Model.RepoObject -> String
makeUrl repo repoObject =
    let
        (GitHub.Model.Path path) =
            GitHub.Model.folderAndNameToPath ( repoObject.folder, repoObject.name )
    in
    Url.Builder.crossOrigin
        rawGit_baseUrl
        [ GitHub.Model.githubIdToPath repo.meta.githubId
        , repo.meta.tag
        , path
        ]
        []
