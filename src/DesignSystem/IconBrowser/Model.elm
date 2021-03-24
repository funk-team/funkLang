module DesignSystem.IconBrowser.Model exposing (..)

import Dict.Any
import Element as El exposing (..)
import GitHub.Model
import Http exposing (Error(..))
import IntDict
import Json.Decode as Decode
import Json.Decode.Field as Field exposing (..)
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..))
import SvgParser


type alias ParsedSvg =
    SvgParser.SvgNode


encodeParsedSvg _ =
    Encode.null


decodeParsedSvg : Decode.Decoder ParsedSvg
decodeParsedSvg =
    Decode.string
        |> Decode.andThen
            (\rawSvg ->
                case SvgParser.parseToElementNode rawSvg of
                    Ok svgNode ->
                        Decode.succeed svgNode

                    Err deadEnd ->
                        Decode.fail deadEnd
            )


{-| An icon that was downloaded from a github source
-}
type alias LocallyCachedIcon =
    { name : GitHub.Model.Name
    , folder : GitHub.Model.Folder
    , sha : GitHub.Model.Sha
    , repoId : GitHub.Model.GitHubRepoId
    , svg : ParsedSvg
    , raw : String
    }


encodeLocallyCachedIcon a =
    Encode.object
        [ ( "name", GitHub.Model.encodeName a.name )
        , ( "folder", GitHub.Model.encodeFolder a.folder )
        , ( "sha", GitHub.Model.encodeSha a.sha )
        , ( "repoId", GitHub.Model.encodeGitHubRepoId a.repoId )
        , ( "raw", Encode.string a.raw )
        ]


decodeLocallyCachedIcon =
    Decode.map6
        LocallyCachedIcon
        (Decode.field "name" GitHub.Model.decodeName)
        (Decode.field "folder" GitHub.Model.decodeFolder)
        (Decode.field "sha" GitHub.Model.decodeSha)
        (Decode.field "repoId" GitHub.Model.decodeGitHubRepoId)
        (Decode.field "raw" decodeParsedSvg)
        (Decode.field "raw" Decode.string)



-- obj


getIcons : GitHub.Model.Repo -> List Icon
getIcons { tree, meta } =
    tree
        |> List.filterMap toIcon


{-| An icon is any repo object that has an SVG file ending
-}
toIcon : GitHub.Model.RepoObject -> Maybe Icon
toIcon repoObject =
    let
        (GitHub.Model.Name name) =
            repoObject.name
    in
    case String.endsWith "svg" name of
        True ->
            Just (Icon repoObject)

        False ->
            Nothing


type Icon
    = Icon GitHub.Model.RepoObject


type alias RemoteRepoData =
    RemoteData.WebData GitHub.Model.Repo


encodeRemoteRepoData _ =
    Encode.null


decodeRemoteRepoData =
    Decode.succeed RemoteData.NotAsked


type alias Repos =
    Dict.Any.AnyDict String GitHub.Model.GitHubRepoId RemoteRepoData


encodeRepos =
    Dict.Any.toList
        >> List.map Tuple.first
        >> Encode.list GitHub.Model.encodeGitHubRepoId


decodeRepos : Decode.Decoder Repos
decodeRepos =
    Decode.list GitHub.Model.decodeGitHubRepoId
        |> Decode.map
            (List.map (\id -> ( id, RemoteData.NotAsked ))
                >> Dict.Any.fromList GitHub.Model.githubIdToPath
            )



-- LOCAL LOGIC
-- [generator-start]


type alias RemoteRepo =
    ( GitHub.Model.GitHubRepoId, RemoteRepoData )


type alias UserPickedIconSet =
    { name : String
    , icons : IntDict.Dict LocallyCachedIcon
    }


type alias Model =
    { selectedRepo : Maybe GitHub.Model.GitHubRepoId
    , githubRepos : Repos
    , userPickedIconSets : IntDict.Dict UserPickedIconSet
    , selectedUserPickedIconSet : Int
    , searchInput : String
    , addUserPickedIconSetInput : Maybe String
    , addRepoInput : Maybe String
    }


type ReferenceToIconInUserPickedIconSet
    = ReferenceToIconInUserPickedIconSet Int Int



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.map7
        Model
        (Decode.field "selectedRepo" (Decode.maybe GitHub.Model.decodeGitHubRepoId))
        (Decode.field "githubRepos" decodeRepos)
        (Decode.field "userPickedIconSets" (IntDict.decodeDict decodeUserPickedIconSet))
        (Decode.field "selectedUserPickedIconSet" Decode.int)
        (Decode.field "searchInput" Decode.string)
        (Decode.field "addUserPickedIconSetInput" (Decode.maybe Decode.string))
        (Decode.field "addRepoInput" (Decode.maybe Decode.string))


decodeReferenceToIconInUserPickedIconSet =
    Decode.map2
        ReferenceToIconInUserPickedIconSet
        (Decode.field "A1" Decode.int)
        (Decode.field "A2" Decode.int)


decodeRemoteRepo =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" GitHub.Model.decodeGitHubRepoId)
        (Decode.field "A2" decodeRemoteRepoData)


decodeUserPickedIconSet =
    Decode.map2
        UserPickedIconSet
        (Decode.field "name" Decode.string)
        (Decode.field "icons" (IntDict.decodeDict decodeLocallyCachedIcon))


encodeMaybeGitHub_Model_GitHubRepoId a =
    case a of
        Just b ->
            GitHub.Model.encodeGitHubRepoId b

        Nothing ->
            Encode.null


encodeMaybeString a =
    case a of
        Just b ->
            Encode.string b

        Nothing ->
            Encode.null


encodeModel a =
    Encode.object
        [ ( "selectedRepo", encodeMaybeGitHub_Model_GitHubRepoId a.selectedRepo )
        , ( "githubRepos", encodeRepos a.githubRepos )
        , ( "userPickedIconSets", IntDict.encodeDict encodeUserPickedIconSet a.userPickedIconSets )
        , ( "selectedUserPickedIconSet", Encode.int a.selectedUserPickedIconSet )
        , ( "searchInput", Encode.string a.searchInput )
        , ( "addUserPickedIconSetInput", encodeMaybeString a.addUserPickedIconSetInput )
        , ( "addRepoInput", encodeMaybeString a.addRepoInput )
        ]


encodeReferenceToIconInUserPickedIconSet (ReferenceToIconInUserPickedIconSet a1 a2) =
    Encode.object
        [ ( "A1", Encode.int a1 )
        , ( "A2", Encode.int a2 )
        ]


encodeRemoteRepo ( a1, a2 ) =
    Encode.object
        [ ( "A1", GitHub.Model.encodeGitHubRepoId a1 )
        , ( "A2", encodeRemoteRepoData a2 )
        ]


encodeUserPickedIconSet a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "icons", IntDict.encodeDict encodeLocallyCachedIcon a.icons )
        ]



-- [generator-end]
