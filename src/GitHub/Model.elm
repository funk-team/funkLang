module GitHub.Model exposing (..)

import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode


objectListReader : Decode.Decoder (List RepoObject)
objectListReader =
    Decode.field
        "tree"
        (Decode.list readRepoObject)


type alias Repo =
    { meta : RepoMeta
    , tree : List RepoObject
    }


type alias RepoObject =
    { name : Name
    , folder : Folder
    , mode : String
    , typ : String
    , sha : Sha
    , size : Maybe Int
    , url : String
    }


readRepoObject : Decode.Decoder RepoObject
readRepoObject =
    Decode.map6
        (\path mode typ sha size url ->
            case toFolderAndName path of
                Nothing ->
                    Decode.fail "could not parse path"

                Just { name, folder } ->
                    Decode.succeed
                        { name = name
                        , folder = folder
                        , mode = mode
                        , typ = typ
                        , sha = sha
                        , size = size
                        , url = url
                        }
        )
        (Decode.field "path" Decode.string |> Decode.map Path)
        (Decode.field "mode" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "sha" Decode.string |> Decode.map Sha)
        (Decode.field "size" (Decode.maybe Decode.int))
        (Decode.field "url" Decode.string)
        |> Decode.andThen identity



-- GITHUB STUFF
-- [generator-start]


{-| A github repo has some meta information plus a list of files
-}
type Sha
    = Sha String


type alias RepoMeta =
    { githubId : GitHubRepoId
    , sha : Sha
    , tag : String -- release name required to build rawgit API URL

    -- todo: be more flexible
    }


{-| Path to file or folder
-}
type Path
    = Path String


{-| Name of a file with extension
-}
type Name
    = Name String


{-| Path to a folder
-}
type Folder
    = Folder (List String)


{-| Username / repoId combo
-}
type GitHubRepoId
    = GitHubRepoId String String



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeFolder =
    Decode.map Folder (Decode.list Decode.string)


decodeGitHubRepoId =
    Decode.map2
        GitHubRepoId
        (Decode.field "A1" Decode.string)
        (Decode.field "A2" Decode.string)


decodeName =
    Decode.map Name Decode.string


decodePath =
    Decode.map Path Decode.string


decodeRepoMeta =
    Decode.map3
        RepoMeta
        (Decode.field "githubId" decodeGitHubRepoId)
        (Decode.field "sha" decodeSha)
        (Decode.field "tag" Decode.string)


decodeSha =
    Decode.map Sha Decode.string


encodeFolder (Folder a1) =
    Encode.list Encode.string a1


encodeGitHubRepoId (GitHubRepoId a1 a2) =
    Encode.object
        [ ( "A1", Encode.string a1 )
        , ( "A2", Encode.string a2 )
        ]


encodeName (Name a1) =
    Encode.string a1


encodePath (Path a1) =
    Encode.string a1


encodeRepoMeta a =
    Encode.object
        [ ( "githubId", encodeGitHubRepoId a.githubId )
        , ( "sha", encodeSha a.sha )
        , ( "tag", Encode.string a.tag )
        ]


encodeSha (Sha a1) =
    Encode.string a1



-- [generator-end]


type alias ParsedPath =
    { name : Name
    , folder : Folder
    , extension : Maybe String
    }


{-| Derive folder and file name from a path

    toFolderAndName <| Path "docs/ionicons/svg/ios-apps.svg"
    --> Just { name = Name "ios-apps.svg"
    --> , extension = Just "svg"
    --> , folder = Folder ["docs", "ionicons", "svg"]
    --> }
    toFolderAndName <| Path "123"
    --> Just {name = Name "123"
    --> , folder = Folder []
    --> , extension = Nothing}

    toFolderAndName <| Path "hello/world.html"
    --> Just {name = Name "world.html"
    --> , folder = Folder ["hello"]
    --> , extension = Just "html"}

-}
toFolderAndName : Path -> Maybe ParsedPath
toFolderAndName (Path path) =
    case path |> String.split "/" |> List.reverse of
        fileNameWithExtension :: reverseFolderPath ->
            let
                folder =
                    Folder (List.reverse reverseFolderPath)
            in
            case String.split "." fileNameWithExtension |> List.reverse of
                [ nameParts ] ->
                    Just
                        { name = Name nameParts -- (String.join "." nameParts)
                        , folder = folder
                        , extension = Nothing
                        }

                extension :: nameParts ->
                    Just
                        { name = Name fileNameWithExtension -- (String.join "." nameParts)
                        , folder = folder
                        , extension = Just extension
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


{-|

    folderAndNameToPath ( Folder [ "some", "folder" ], Name "1.svg" ) --> Path "some/folder/1.svg"

-}
folderAndNameToPath : ( Folder, Name ) -> Path
folderAndNameToPath ( Folder folder, Name name ) =
    Path (folder ++ [ name ] |> String.join "/")


getPathFromRepoObject : RepoObject -> Path
getPathFromRepoObject { folder, name } =
    folderAndNameToPath ( folder, name )


repoReader : GitHubRepoId -> Decode.Decoder Repo
repoReader githubId =
    Decode.map2 Repo
        (Decode.field "meta" (repoMetaReader githubId))
        (Decode.field "tree" (resilientListDecoder readRepoObject))


resilientListDecoder : Decode.Decoder a -> Decode.Decoder (List a)
resilientListDecoder dec =
    Decode.list Decode.value
        |> Decode.map (List.filterMap (\vl -> Decode.decodeValue dec vl |> Result.toMaybe))


repoMetaReader : GitHubRepoId -> Decode.Decoder RepoMeta
repoMetaReader githubId =
    Field.attempt "0" (Decode.succeed True) <|
        \maybeNoTag ->
            Field.requireAt [ "0", "name" ] Decode.string <|
                \tag ->
                    Field.requireAt [ "0", "commit", "sha" ] Decode.string <|
                        \sha ->
                            maybeNoTag
                                |> Maybe.map
                                    (\_ ->
                                        Decode.succeed
                                            { githubId = githubId
                                            , sha = Sha sha
                                            , tag = tag
                                            }
                                    )
                                |> Maybe.withDefault
                                    (Decode.fail "The Git repo you enter need to have at least one tags.")


githubIdToPath : GitHubRepoId -> String
githubIdToPath (GitHubRepoId userName repoId) =
    userName ++ "/" ++ repoId
