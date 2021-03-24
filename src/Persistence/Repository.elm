port module Persistence.Repository exposing (..)

import Canvas.Msg
import RemoteData


newProjectSub model =
    case model.project of
        RemoteData.Success _ ->
            Sub.none

        RemoteData.Failure _ ->
            Sub.none

        _ ->
            repoInitialized
                Canvas.Msg.RepoInitialized


projectSavedSub model =
    repoPushed
        Canvas.Msg.RepoPushed


port repoInitialized : (Maybe String -> msg) -> Sub msg


port repoPushed : (String -> msg) -> Sub msg
