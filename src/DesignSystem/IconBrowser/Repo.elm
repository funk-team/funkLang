module DesignSystem.IconBrowser.Repo exposing (..)

import DesignSystem.IconBrowser.Model
import DesignSystem.IconBrowser.Msg exposing (..)
import Dict
import Dict.Any
import Element exposing (Element, px, rgb255)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import GitHub.Model
import GitHub.RawGitApi
import Html
import Html.Attributes
import Http
import List.Extra
import Random
import RemoteData
import Simple.Fuzzy as Fuzzy
import Ui.Boxicons
import Ui.Component
import Ui.Help
import Ui.Style



-- CARDS / TOP SECTION


viewSimpleButton : List (Element.Attribute Msg) -> String -> Msg -> Element.Element Msg
viewSimpleButton attr symbol msg =
    Element.Input.button
        Ui.Component.buttonStyle
        { onPress = Just msg
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Font.size 40
                ]
                (Element.text symbol)
        }


viewCards : DesignSystem.IconBrowser.Model.Model -> Element DesignSystem.IconBrowser.Msg.Msg
viewCards model =
    let
        viewAddedSet ( id, repo ) =
            let
                isSelected =
                    model.selectedRepo == Just id

                isDisplayed =
                    String.trim model.searchInput
                        == ""
                        && isSelected
            in
            viewCard
                { isSelected = isDisplayed
                , repo = ( id, repo )
                }

        importedRepos =
            model.githubRepos
                |> Dict.Any.toList
                |> List.map
                    viewAddedSet
    in
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spaceEvenly
        , Element.spacing 32
        ]
        importedRepos


viewCard :
    { isSelected : Bool
    , repo :
        ( GitHub.Model.GitHubRepoId, RemoteData.RemoteData Http.Error GitHub.Model.Repo )
    }
    -> Element DesignSystem.IconBrowser.Msg.Msg
viewCard { isSelected, repo } =
    let
        ( id, request ) =
            repo

        cardStyles =
            [ Element.padding 15
            , Element.Background.color <|
                if isSelected then
                    Ui.Style.highlightColor

                else
                    Ui.Style.transparent
            , Element.Border.width 1
            , Element.Border.color Ui.Style.highlightColor
            , Element.Border.rounded 3
            , Element.height (Element.px 110)
            , Element.width (Element.px 200)
            ]

        elAttr =
            [ Element.height (px 24)
            , Element.width (px 24)
            , Ui.Help.noPointerEvents
            ]

        content =
            case request of
                RemoteData.Failure _ ->
                    Element.el
                        [ Element.Font.color Ui.Style.highlightColorSolidImportant ]
                    <|
                        Element.text "Failed to load repo"

                RemoteData.Loading ->
                    Element.text "Loading"

                RemoteData.NotAsked ->
                    Element.text "Not asked"

                RemoteData.Success data ->
                    let
                        iconList =
                            DesignSystem.IconBrowser.Model.getIcons data

                        icons =
                            iconList
                                |> List.filter (\(DesignSystem.IconBrowser.Model.Icon icon) -> isNotDocAsset icon)
                                |> List.take 100
                                |> shuffleList
                                |> List.take 12
                                |> List.map (viewIcon data)
                                |> List.map (Element.el elAttr)

                        iconsPreview =
                            Element.wrappedRow
                                [ Element.spacing 5
                                , Element.width (Element.px 170)
                                ]
                                icons
                    in
                    if List.isEmpty iconList then
                        Element.text "no icons"
                        -- @TODO empty symbol

                    else
                        iconsPreview

        title =
            Element.text (GitHub.Model.githubIdToPath id)
                |> Element.el [ Element.width Element.fill, Element.clip, Ui.Style.class "text-overflow-ellipsis-on-child" ]
    in
    Element.Input.button
        cardStyles
        { onPress = Just (SelectRepo id)
        , label =
            Element.column
                [ Element.spacing 10
                , Element.Font.color Ui.Style.grey
                , Element.height Element.fill
                , Element.width Element.fill
                ]
                [ title
                , content
                ]
        }



-- VIEW GITHUB ICON REPOS


{-| View a folder from a github repo containing a collection of icons
-}
viewFolder : DesignSystem.IconBrowser.Model.Model -> GitHub.Model.Repo -> ( DesignSystem.IconBrowser.Model.Icon, List DesignSystem.IconBrowser.Model.Icon ) -> Element DesignSystem.IconBrowser.Msg.Msg
viewFolder model ({ meta } as repo) ( firstIcon, iconGroup ) =
    let
        folderPath =
            folder |> String.join "/"

        (DesignSystem.IconBrowser.Model.Icon firstIcon_) =
            firstIcon

        (GitHub.Model.Folder folder) =
            firstIcon_.folder

        urlToIconFolder =
            "https://github.com/"
                ++ GitHub.Model.githubIdToPath meta.githubId
                ++ "/tree/master/"
                ++ folderPath

        linkedFolderName =
            Element.newTabLink
                [ Element.padding 6
                , Element.Background.color Ui.Style.lightGrey
                , Element.Font.color (rgb255 0 0 200)
                ]
                { url = urlToIconFolder
                , label = Element.text folderPath
                }

        allIconsInFolder =
            firstIcon :: iconGroup
    in
    Element.column [ Element.spacing 10 ]
        [ linkedFolderName
        , allIconsInFolder
            |> List.map (viewIconInFolder model repo)
            |> Element.wrappedRow [ Element.spacing 5 ]
        ]


viewIconInFolder : DesignSystem.IconBrowser.Model.Model -> GitHub.Model.Repo -> DesignSystem.IconBrowser.Model.Icon -> Element.Element DesignSystem.IconBrowser.Msg.Msg
viewIconInFolder model repo icon =
    let
        attribs =
            [ Element.mouseOver [ Element.Background.color Ui.Style.slightAccent ]
            , Element.Border.rounded 4
            , Element.padding 3
            , Element.Border.rounded 3
            ]
                ++ highlightIfSelected

        findContainingIconSet : DesignSystem.IconBrowser.Model.UserPickedIconSet -> Bool
        findContainingIconSet { icons } =
            icons
                |> Dict.toList
                |> List.any (\( _, i ) -> ( i.repoId, i.folder, i.name ) == ( repo.meta.githubId, repoObject.folder, repoObject.name ))

        (DesignSystem.IconBrowser.Model.Icon repoObject) =
            icon

        foundInUserPickerIconSet =
            model.userPickedIconSets
                |> Dict.toList
                |> List.map Tuple.second
                |> List.Extra.find findContainingIconSet

        {- Find if an icon was used in a set and set the given color from an icon set -}
        highlightIfSelected : List (Element.Attribute msg)
        highlightIfSelected =
            case foundInUserPickerIconSet of
                Just _ ->
                    [ Element.Background.color Ui.Style.highlightColor
                    , Element.Border.rounded 3
                    ]

                Nothing ->
                    [ Element.Border.rounded 3
                    ]
    in
    -- Element.none
    Element.Input.button attribs
        { label =
            Element.el
                [ Element.width (Element.px 24)
                , Element.height (Element.px 24)
                ]
            <|
                viewIcon repo icon
        , onPress =
            Just (IconFromRepoSelected model.selectedUserPickedIconSet repo icon)
        }


{-| View icon from a GitHub Repo Object
-}
viewIcon :
    GitHub.Model.Repo
    -> DesignSystem.IconBrowser.Model.Icon
    -> Element.Element msg
viewIcon repo (DesignSystem.IconBrowser.Model.Icon repoObject) =
    let
        url =
            GitHub.RawGitApi.makeUrl
                repo
                repoObject
    in
    Html.img [ Html.Attributes.src url ] []
        |> Element.html


view : DesignSystem.IconBrowser.Model.Model -> ( GitHub.Model.GitHubRepoId, DesignSystem.IconBrowser.Model.RemoteRepoData ) -> Element DesignSystem.IconBrowser.Msg.Msg
view model ( repoId, repoData ) =
    let
        iconSetTitle =
            Element.text (GitHub.Model.githubIdToPath repoId)

        githubLink =
            Element.newTabLink
                [ Element.Font.size 30
                , Element.Font.bold
                ]
                { url = "https://github.com/" ++ GitHub.Model.githubIdToPath repoId
                , label = Ui.Component.icon Ui.Boxicons.bxlGithub
                }

        removeButton =
            Element.Input.button
                Ui.Component.buttonStyle
                { label = Element.text "Remove icon set"
                , onPress = Just (RemoveRepo repoId)
                }

        header =
            Element.column
                [ Element.spacing 10 ]
                [ Element.row [ Element.spacing 10 ] [ iconSetTitle, githubLink ], removeButton ]
    in
    case repoData of
        RemoteData.Loading ->
            Element.column
                [ Element.spacing 30 ]
                [ header
                , Element.text "Loading icons, please stand by..."
                ]

        RemoteData.Failure error ->
            Element.column
                [ Element.spacing 30 ]
                [ header
                , Element.text "Could not load this iconset from GitHub. Try removing it and adding it again."
                ]

        RemoteData.NotAsked ->
            Element.column
                [ Element.spacing 30 ]
                [ header
                , Element.text "No request sent. Try reloading the page."
                ]

        RemoteData.Success repo ->
            let
                iconList =
                    DesignSystem.IconBrowser.Model.getIcons repo

                folders : List (Element.Element DesignSystem.IconBrowser.Msg.Msg)
                folders =
                    iconList
                        |> groupIconsByFolder
                        |> removeNoisyFolders
                        |> dedupe
                        |> List.map applyFuzzyFilter
                        |> List.filterMap
                            (\iconsInFolder ->
                                case iconsInFolder of
                                    atLeastOne :: more ->
                                        Just ( atLeastOne, more )

                                    [] ->
                                        Nothing
                            )
                        |> List.map (viewFolder model repo)

                applyFuzzyFilter : List DesignSystem.IconBrowser.Model.Icon -> List DesignSystem.IconBrowser.Model.Icon
                applyFuzzyFilter =
                    case String.length model.searchInput > 2 of
                        False ->
                            identity

                        True ->
                            Fuzzy.filter
                                (\(DesignSystem.IconBrowser.Model.Icon icon) ->
                                    let
                                        (GitHub.Model.Path iconPath) =
                                            GitHub.Model.getPathFromRepoObject icon
                                    in
                                    iconPath
                                )
                                model.searchInput
            in
            Element.column [ Element.spacing 30 ]
                (header :: folders)



-- CLEAN REPO HELPERS


{-| Remove smaller folders if one of their icons are already in bigger folders

  - using a minimum size
  - finding duplicate entries

-}
dedupe : List (List DesignSystem.IconBrowser.Model.Icon) -> List (List DesignSystem.IconBrowser.Model.Icon)
dedupe folders =
    let
        onlyAddIfNoSharedIcons :
            List DesignSystem.IconBrowser.Model.Icon
            -> List (List DesignSystem.IconBrowser.Model.Icon)
            -> List (List DesignSystem.IconBrowser.Model.Icon)
        onlyAddIfNoSharedIcons thisFolder previousFolders =
            let
                someIconIsInPreviousFolders =
                    -- for any icon in this folder
                    List.any
                        (\(DesignSystem.IconBrowser.Model.Icon someIcon) ->
                            -- in any previous folder
                            List.any
                                (\previousFolder ->
                                    List.any
                                        (\(DesignSystem.IconBrowser.Model.Icon previousIcon) -> someIcon.name == previousIcon.name)
                                        previousFolder
                                )
                                previousFolders
                        )
                        thisFolder
            in
            if someIconIsInPreviousFolders then
                previousFolders

            else
                previousFolders ++ [ thisFolder ]
    in
    folders
        -- sort by descending size
        |> List.sortBy (List.length >> (*) -1)
        |> List.foldl onlyAddIfNoSharedIcons []


{-| Folders with just a few icons in them are likely docs, tests etc. so not the actual SVG source the use cares about
-}
removeNoisyFolders : List (List DesignSystem.IconBrowser.Model.Icon) -> List (List DesignSystem.IconBrowser.Model.Icon)
removeNoisyFolders folders =
    let
        minimumFolderSize =
            10

        noSmallFolders =
            List.filter
                (\folderWithIcons ->
                    let
                        length =
                            List.length folderWithIcons
                    in
                    length > minimumFolderSize
                )
    in
    folders
        |> noSmallFolders
        |> noDocsFolders


noDocsFolders =
    List.filter
        (\folderWithIcons ->
            case folderWithIcons of
                (DesignSystem.IconBrowser.Model.Icon icon) :: _ ->
                    isNotDocAsset icon

                _ ->
                    False
        )


isNotDocAsset : GitHub.Model.RepoObject -> Bool
isNotDocAsset icon =
    let
        (GitHub.Model.Folder f) =
            icon.folder

        blacklist =
            [ "test", "docs" ]
    in
    case List.any (\blacklisted -> List.member blacklisted f) blacklist of
        True ->
            False

        False ->
            True


groupIconsByFolder : List DesignSystem.IconBrowser.Model.Icon -> List (List DesignSystem.IconBrowser.Model.Icon)
groupIconsByFolder iconList =
    iconList
        |> List.Extra.groupWhile
            (\(DesignSystem.IconBrowser.Model.Icon a) (DesignSystem.IconBrowser.Model.Icon b) -> a.folder == b.folder)
        |> List.map (\( head, queue ) -> head :: queue)



-- https://stackoverflow.com/questions/42207900/how-to-shuffle-a-list-in-elm


shuffleList : List a -> List a
shuffleList list =
    let
        seed =
            Random.initialSeed 1234
    in
    shuffleListHelper seed list []


shuffleListHelper : Random.Seed -> List a -> List a -> List a
shuffleListHelper seed source result =
    if List.isEmpty source then
        result

    else
        let
            indexGenerator =
                Random.int 0 (List.length source - 1)

            ( index, nextSeed ) =
                Random.step indexGenerator seed

            valAtIndex =
                List.Extra.getAt index source

            sourceWithoutIndex =
                List.Extra.removeAt index source
        in
        case valAtIndex of
            Just val ->
                shuffleListHelper nextSeed sourceWithoutIndex (val :: result)

            Nothing ->
                -- this should never happen
                -- Debug.crash "generated an index outside list"
                result
