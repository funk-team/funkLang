module DesignSystem.IconBrowser.Msg exposing (..)

import DesignSystem.IconBrowser.Model
import GitHub.Model
import Http


type Msg
    = SelectRepo GitHub.Model.GitHubRepoId
    | SelectUserPickedIconSet Int
    | GotRepoData GitHub.Model.GitHubRepoId DesignSystem.IconBrowser.Model.RemoteRepoData
    | IconFromRepoSelected Int GitHub.Model.Repo DesignSystem.IconBrowser.Model.Icon
    | UpdateSearchInput String
    | RemoveIconFromUserPickedSet Int Int
    | UpdateAddUserPickedIconSetInput String
    | OpenAddUserPickedIconSetField
    | RequestNewUserPickedIconSet
    | CancelNewUserPickedIconSet
      -- add repo
    | UpdateAddRepoInput String
    | OpenAddRepoField
    | CloseAddRepoField
    | RequestNewRepo
    | RemoveUserPickedIconSet Int
    | RemoveRepo GitHub.Model.GitHubRepoId
    | UpdateUserPickedIconSetName Int String
    | GotIconSvg Int GitHub.Model.Repo DesignSystem.IconBrowser.Model.Icon (Result Http.Error String)
