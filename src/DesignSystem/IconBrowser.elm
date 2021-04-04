module DesignSystem.IconBrowser exposing (..)

{-| Use this module as well as DesignSystem.IconBrowser.Model to render the icon browser
menu and to work with icons in general.
-}

import Cmd.Extra exposing (..)
import Color.OneDark
import DesignSystem.IconBrowser.Model
import DesignSystem.IconBrowser.Msg exposing (..)
import DesignSystem.IconBrowser.Repo
import Dict
import Dict.Any
import Element exposing (Element, px)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import EventsExtra
import GitHub.Api
import GitHub.Model
import GitHub.RawGitApi
import Http exposing (Error(..))
import IntDict
import Model.Product
import RemoteData
import Slug
import SvgParser
import Ui.Boxicons
import Ui.Component
import Ui.DesignSystem
import Ui.Help
import Ui.Style exposing (edges)
import Url
import ZipList



---- DATA STRUCTURES ----


{-| Get the icon defined in the reference (for example in a data connection).
This returns a locally cached icon with parsed SVG for inlining and coloring
-}
getCorrespondingIcon : DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet -> DesignSystem.IconBrowser.Model.Model -> Maybe DesignSystem.IconBrowser.Model.LocallyCachedIcon
getCorrespondingIcon (DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet userPickedIconSetId iconId) model =
    model.userPickedIconSets
        |> Dict.get userPickedIconSetId
        |> Maybe.map .icons
        |> Maybe.andThen (Dict.get iconId)


{-| Init
-}
requestRepos : Model.Product.Mode -> DesignSystem.IconBrowser.Model.Repos -> List (Cmd Msg)
requestRepos mode githubRepos =
    githubRepos
        |> Dict.Any.toList
        |> List.map
            (\( id, status ) ->
                case status of
                    RemoteData.Success _ ->
                        Cmd.none

                    _ ->
                        GitHub.Api.getRepo mode id
                            |> Cmd.map (GotRepoData id)
            )


init : Model.Product.Mode -> ( DesignSystem.IconBrowser.Model.Model, Cmd Msg )
init mode =
    let
        githubRepos : Dict.Any.AnyDict String GitHub.Model.GitHubRepoId DesignSystem.IconBrowser.Model.RemoteRepoData
        githubRepos =
            [ ( GitHub.Model.GitHubRepoId "michaelampr" "jam", RemoteData.Loading )
            , ( GitHub.Model.GitHubRepoId "ionic-team" "ionicons", RemoteData.Loading )
            , ( GitHub.Model.GitHubRepoId "feathericons" "feather", RemoteData.Loading )
            ]
                |> Dict.Any.fromList GitHub.Model.githubIdToPath

        model : DesignSystem.IconBrowser.Model.Model
        model =
            { selectedRepo = Nothing
            , githubRepos = githubRepos
            , userPickedIconSets =
                IntDict.insertNew
                    { name = "My Custom Iconset"
                    , icons = IntDict.empty
                    }
                    IntDict.empty
            , selectedUserPickedIconSet = 0
            , searchInput = ""
            , addUserPickedIconSetInput = Nothing
            , addRepoInput = Nothing
            }
    in
    withCmds (requestRepos mode githubRepos) model


initColorPalette =
    [ Color.OneDark.lightRed
    , Color.OneDark.blue
    , Color.OneDark.cyan
    , Color.OneDark.lightYellow
    , Color.OneDark.darkRed
    , Color.OneDark.magenta
    , Color.OneDark.darkYellow
    ]
        |> ZipList.new Color.OneDark.green



---- VIEW ----


view : DesignSystem.IconBrowser.Model.Model -> Element Msg
view model =
    Ui.DesignSystem.view
        { sidebar = viewSidebar model
        , mainBody = mainContent model
        }


viewSidebar : DesignSystem.IconBrowser.Model.Model -> Element.Element Msg
viewSidebar model =
    let
        viewSidebarRow ( id, userPickedIconSet ) =
            let
                isSelected =
                    model.selectedUserPickedIconSet == id

                label =
                    Element.column
                        [ Element.width Element.fill, Element.spacing 10 ]
                        [ title id isSelected userPickedIconSet
                        , preview
                        , case ( isSelected, icons ) of
                            ( True, [] ) ->
                                Element.el [ Element.alpha 0.5 ] (Element.text "Pick icon from right section")

                            ( True, _ ) ->
                                Element.el [ Element.alpha 0.5 ] (Element.text "Click icon to remove it from set")

                            _ ->
                                Element.none
                        ]

                icons =
                    userPickedIconSet.icons
                        |> Dict.toList

                preview =
                    icons
                        |> List.map
                            (\( iconId, icon ) ->
                                viewIconForUserPickedIconSet isSelected icon
                                    |> Element.map (\_ -> RemoveIconFromUserPickedSet id iconId)
                            )
                        |> Element.wrappedRow
                            [ Element.width Element.fill
                            ]
            in
            Ui.DesignSystem.viewSidebarRowSimple
                { isSelected = isSelected
                , label = label
                , msg = SelectUserPickedIconSet id
                , onRemove = Just (RemoveUserPickedIconSet id)
                , attribs = []
                }

        title =
            \id isSelected userPickedIconSet ->
                let
                    editableName =
                        Ui.Component.contenteditable
                            { text = userPickedIconSet.name
                            , placeholder = "Name your icon pack"
                            , enabled = isSelected
                            }
                            |> Element.map (UpdateUserPickedIconSetName id)
                in
                Element.row
                    [ Element.spacing 5 ]
                    [ editableName |> Element.el []
                    ]
    in
    Ui.DesignSystem.viewSidebar
        { rows =
            model.userPickedIconSets
                |> Dict.toList
                |> List.map viewSidebarRow
        , addButton =
            Ui.DesignSystem.viewAddButton
                { openFieldMsg = OpenAddUserPickedIconSetField
                , updateFieldMsg = UpdateAddUserPickedIconSetInput
                , submitMsg = RequestNewUserPickedIconSet
                , cancelMsg = CancelNewUserPickedIconSet
                , maybeInput = model.addUserPickedIconSetInput
                , thingToAdd = "Icon Set"
                }
                |> Just
        }


currentRepoAndId : DesignSystem.IconBrowser.Model.Model -> Maybe ( GitHub.Model.GitHubRepoId, DesignSystem.IconBrowser.Model.RemoteRepoData )
currentRepoAndId model =
    case model.selectedRepo of
        Nothing ->
            Nothing

        Just id ->
            case Dict.Any.get id model.githubRepos of
                Nothing ->
                    Nothing

                Just set ->
                    Just ( id, set )


mainContent : DesignSystem.IconBrowser.Model.Model -> Element Msg
mainContent model =
    let
        hasRemoteIconSets =
            not (Dict.Any.isEmpty model.githubRepos)

        viewSelectedRepo =
            case ( hasRemoteIconSets, currentRepoAndId model, String.trim model.searchInput ) of
                ( False, Nothing, _ ) ->
                    Element.text "Click the '+' button to add an icon set from GitHub"

                ( True, Nothing, "" ) ->
                    Element.text "Select a remote icon set from above to add icons to your project icon sets"

                ( _, Just info, "" ) ->
                    DesignSystem.IconBrowser.Repo.view model info

                -- user has given search input
                _ ->
                    model.githubRepos
                        |> Dict.Any.toList
                        |> List.map
                            (\( key, iconRepo ) ->
                                DesignSystem.IconBrowser.Repo.view model ( key, iconRepo )
                            )
                        |> Element.column [ Element.width Element.fill, Element.spacing 50 ]

        addButton =
            viewImportRepoButtonOrForm
                model.githubRepos
                model.addRepoInput
    in
    Element.column
        [ Element.width Element.fill
        , Element.alignTop
        , Element.spacing 40
        , Element.padding 20
        ]
        [ DesignSystem.IconBrowser.Repo.viewCards model
        , addButton
        , viewSearchTextInput UpdateSearchInput model.searchInput
        , viewSelectedRepo
        ]


viewImportRepoButtonOrForm : DesignSystem.IconBrowser.Model.Repos -> Maybe String -> Element Msg
viewImportRepoButtonOrForm repos maybeInput =
    case maybeInput of
        Nothing ->
            Element.Input.button
                Ui.Component.buttonStyle
                { onPress = Just OpenAddRepoField
                , label =
                    Element.text "Add GitHub repository"
                }

        Just repo ->
            let
                placeholder =
                    "github: owner/repo"

                input =
                    Element.Input.text
                        [ Element.width (px 240)
                        , EventsExtra.onEnter
                            RequestNewRepo
                        , Element.padding 10
                        ]
                        { onChange =
                            UpdateAddRepoInput
                        , text = repo
                        , placeholder =
                            Element.Input.placeholder []
                                (Element.text placeholder)
                                |> Just
                        , label =
                            Element.text "Add repository:"
                                |> Element.Input.labelAbove [ Element.paddingEach { edges | bottom = 10 }, Element.Font.bold ]
                        }

                submitButton =
                    let
                        attribs =
                            case canSubmit of
                                False ->
                                    [ Element.alpha 0.5, Ui.Style.transition, Ui.Help.noPointerEvents ]

                                True ->
                                    [ Ui.Style.transition ]
                    in
                    Element.Input.button
                        (Ui.Component.buttonStyle ++ attribs)
                        { onPress = Just RequestNewRepo
                        , label = Element.text "Submit"
                        }

                icon =
                    Ui.Component.icon Ui.Boxicons.bxInfoCircle
                        |> Element.el []

                hintHeight =
                    Element.px 60

                hintStyles =
                    [ Element.height hintHeight
                    , Element.Border.widthEach { edges | left = 1 }
                    , Element.paddingEach { edges | left = 10 }
                    , Element.Font.color Ui.Style.grey
                    , Element.spacing 10
                    ]

                usageHint =
                    let
                        text =
                            Element.paragraph
                                []
                                [ Element.text "To add an icon pack, use a GitHub URL or just the "
                                , Element.el [ Element.Font.italic, Element.Font.bold ] (Element.text "username/repository-name")
                                , Element.text " part. The repository must have at least one release tag. Try using "
                                , Element.el [ Ui.Style.monospace, Element.Font.bold, Ui.Style.style "user-select" "all" ] (Element.text "amitjakhu/dripicons")
                                , Element.text " (click to select)."
                                ]
                    in
                    Element.row
                        hintStyles
                        [ icon, text ]

                ( userFeedback, canSubmit ) =
                    case parseRepoInput repo of
                        Nothing ->
                            ( usageHint, False )

                        Just repoId ->
                            let
                                alreadyExists =
                                    Dict.Any.toList
                                        repos
                                        |> List.map Tuple.first
                                        |> List.member repoId

                                text =
                                    GitHub.Model.githubIdToPath repoId
                                        |> Element.text
                                        |> Element.el [ Ui.Style.monospace ]
                            in
                            case alreadyExists of
                                False ->
                                    let
                                        paragraph =
                                            Element.paragraph
                                                []
                                                [ Element.text "Looks good. Using: "
                                                , text
                                                ]
                                    in
                                    ( Element.row
                                        hintStyles
                                        [ icon, paragraph ]
                                    , True
                                    )

                                True ->
                                    let
                                        paragraph =
                                            Element.paragraph
                                                []
                                                [ text
                                                , Element.text " has already beeen imported. Try another one."
                                                ]
                                    in
                                    ( Element.row
                                        hintStyles
                                        [ icon, paragraph ]
                                    , False
                                    )

                closeButton =
                    Element.Input.button
                        [ Element.alignRight
                        , Element.alpha 0.5
                        , Element.mouseOver [ Element.alpha 1 ]
                        ]
                        { label = Ui.Component.icon Ui.Boxicons.bxX
                        , onPress = Just CloseAddRepoField
                        }
            in
            Element.column
                [ Element.spacing 15
                , Element.padding 15
                , Element.Border.width 1
                , Element.Border.color Ui.Style.slightAccent
                , Element.Border.rounded 3
                , Ui.Style.shadowMedium
                , Element.width (Element.px 600)
                , Element.inFront closeButton
                ]
                [ input
                , userFeedback
                , submitButton
                ]



-- VIEW USER ICON SETS


viewIconForUserPickedIconSet : Bool -> DesignSystem.IconBrowser.Model.LocallyCachedIcon -> Element ()
viewIconForUserPickedIconSet setIsSelected icon =
    viewLocallyCachedIcon icon
        |> Element.el
            ([ Element.height (px 27)
             , Element.width (px 27)
             , Element.Border.width 1
             , Element.Border.color Ui.Style.transparent
             , Element.Border.rounded 3
             , Element.padding 2
             , Element.mouseOver
                (if setIsSelected then
                    [ Element.Border.color Ui.Style.black
                    ]

                 else
                    []
                )
             ]
                ++ (if setIsSelected then
                        [ Element.Events.onClick () ]

                    else
                        []
                   )
            )


viewLocallyCachedIcon : DesignSystem.IconBrowser.Model.LocallyCachedIcon -> Element.Element msg
viewLocallyCachedIcon { svg } =
    let
        -- remove size attributes in order to ensure that the SVG is not too big
        removeSize =
            List.filter
                (\( name, _ ) ->
                    case name of
                        "width" ->
                            False

                        "height" ->
                            False

                        _ ->
                            True
                )

        -- do not allow pointer events in order to not catch click handler of element it includes
        -- see https://github.com/funk-team/funk/issues/627
        addNoPointerEvents : List ( String, String ) -> List ( String, String )
        addNoPointerEvents attribs =
            let
                -- add to existing style attrib or add style attrib
                found =
                    attribs
                        |> List.any (\( name, _ ) -> name == "style")

                result =
                    case found of
                        True ->
                            attribs
                                |> List.map
                                    (\( attrib, val ) ->
                                        case attrib of
                                            "style" ->
                                                ( "style", val ++ "; pointer-events: none" )

                                            _ ->
                                                ( attrib, val )
                                    )

                        False ->
                            ( "style", "pointer-events: none" ) :: attribs
            in
            result

        makeColorable : SvgParser.SvgNode -> SvgParser.SvgNode
        makeColorable svg_ =
            let
                hasColorAttributes { attributes } =
                    attributes
                        |> List.any
                            (\( attr, value ) ->
                                case attr of
                                    "fill" ->
                                        True

                                    "stroke" ->
                                        True

                                    _ ->
                                        False
                            )

                check node =
                    case node of
                        SvgParser.SvgElement el ->
                            hasColorAttributes el
                                || List.any check el.children

                        _ ->
                            False
            in
            if check svg_ then
                svg_

            else
                case svg_ of
                    SvgParser.SvgElement el ->
                        { el | attributes = ( "fill", "currentColor" ) :: el.attributes }
                            |> SvgParser.SvgElement

                    _ ->
                        svg_

        refined =
            case svg of
                SvgParser.SvgElement el ->
                    SvgParser.SvgElement
                        { el
                            | attributes =
                                el.attributes
                                    |> removeSize
                                    |> addNoPointerEvents
                        }
                        |> makeColorable

                _ ->
                    svg
    in
    SvgParser.nodeToSvg refined
        |> Element.html



-- view search box


viewSearchTextInput : (String -> Msg) -> String -> Element Msg
viewSearchTextInput toMsg content =
    let
        isActive =
            String.trim content /= ""

        ( bg, font ) =
            if isActive then
                ( Ui.Style.highlightColorSolid, Ui.Style.white )

            else
                ( Ui.Style.lightGrey, Ui.Style.grey )

        searchIcon =
            Element.el
                [ Element.alignRight
                , Element.centerY
                , Element.moveLeft 5
                , Element.Font.color font
                ]
                (Ui.Component.icon Ui.Boxicons.bxSearch)
    in
    Element.Input.text
        [ Element.width (Element.fill |> Element.maximum 400)
        , Element.inFront searchIcon
        , Element.Background.color bg
        , Element.Font.color font
        ]
        { onChange = toMsg
        , text = content
        , placeholder =
            Element.Input.placeholder
                []
                (Element.text "Search")
                |> Just
        , label = Element.Input.labelHidden "Search icon"
        }



---- UPDATE ----


update : Msg -> Model.Product.Mode -> DesignSystem.IconBrowser.Model.Model -> ( DesignSystem.IconBrowser.Model.Model, Cmd Msg )
update msg mode model =
    case msg of
        SelectRepo id ->
            { model
                | selectedRepo = Just id
            }
                |> withNoCmd

        SelectUserPickedIconSet newUserPickedIconSetId ->
            { model
                | selectedUserPickedIconSet = newUserPickedIconSetId
            }
                |> withNoCmd

        UpdateUserPickedIconSetName id name ->
            { model
                | userPickedIconSets =
                    Dict.update
                        id
                        (Maybe.map (\set -> { set | name = name }))
                        model.userPickedIconSets
            }
                |> withNoCmd

        GotRepoData id repoData ->
            { model
                | githubRepos = Dict.Any.insert id repoData model.githubRepos
            }
                |> withNoCmd

        RemoveIconFromUserPickedSet setId iconId ->
            ( { model
                | userPickedIconSets =
                    Dict.update
                        setId
                        (Maybe.map (\set -> { set | icons = Dict.remove iconId set.icons }))
                        model.userPickedIconSets
              }
            , Cmd.none
            )

        UpdateSearchInput inp ->
            { model | searchInput = inp }
                |> withNoCmd

        UpdateAddUserPickedIconSetInput inp ->
            { model | addUserPickedIconSetInput = Just inp }
                |> withNoCmd

        OpenAddUserPickedIconSetField ->
            { model | addUserPickedIconSetInput = Just "" }
                |> withNoCmd

        CancelNewUserPickedIconSet ->
            { model | addUserPickedIconSetInput = Nothing }
                |> withNoCmd

        RequestNewUserPickedIconSet ->
            case model.addUserPickedIconSetInput of
                Nothing ->
                    model |> withNoCmd

                Just addUserPickedIconSetInput ->
                    let
                        newUserPickedIconSets =
                            model.userPickedIconSets
                                |> IntDict.insertNew
                                    (DesignSystem.IconBrowser.Model.UserPickedIconSet
                                        addUserPickedIconSetInput
                                        IntDict.empty
                                    )
                    in
                    { model
                        | userPickedIconSets = newUserPickedIconSets
                        , addUserPickedIconSetInput = Nothing
                        , selectedUserPickedIconSet =
                            newUserPickedIconSets
                                |> Dict.toList
                                |> List.map Tuple.first
                                |> List.maximum
                                |> Maybe.withDefault
                                    model.selectedUserPickedIconSet
                    }
                        |> withNoCmd

        UpdateAddRepoInput inp ->
            { model | addRepoInput = Just inp }
                |> withNoCmd

        OpenAddRepoField ->
            { model | addRepoInput = Just "" }
                |> withNoCmd

        CloseAddRepoField ->
            { model | addRepoInput = Nothing }
                |> withNoCmd

        RequestNewRepo ->
            case model.addRepoInput of
                Nothing ->
                    model |> withNoCmd

                Just addRepoInput ->
                    case parseRepoInput addRepoInput of
                        Nothing ->
                            model |> withNoCmd

                        Just repoId ->
                            ( { model
                                | githubRepos = Dict.Any.insert repoId RemoteData.Loading model.githubRepos
                                , addRepoInput = Nothing
                                , selectedRepo = Just repoId
                              }
                            , GitHub.Api.getRepo
                                mode
                                repoId
                                |> Cmd.map (GotRepoData repoId)
                            )

        RemoveUserPickedIconSet id ->
            { model
                | userPickedIconSets = Dict.remove id model.userPickedIconSets
            }
                |> withNoCmd

        RemoveRepo id ->
            { model
                | githubRepos = Dict.Any.remove id model.githubRepos
            }
                |> withNoCmd

        IconFromRepoSelected forSet repo icon ->
            ( model
            , GitHub.RawGitApi.getIconSvg repo icon
                |> Cmd.map (GotIconSvg forSet repo icon)
            )

        GotIconSvg forSet repo (DesignSystem.IconBrowser.Model.Icon icon) response ->
            case response of
                Err _ ->
                    ( model, Cmd.none )

                Ok rawSvg ->
                    case SvgParser.parseToElementNode rawSvg of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok svg ->
                            let
                                newIcon : DesignSystem.IconBrowser.Model.LocallyCachedIcon
                                newIcon =
                                    { svg = svg
                                    , raw = rawSvg
                                    , sha = icon.sha
                                    , name = icon.name
                                    , folder = icon.folder
                                    , repoId = repo.meta.githubId
                                    }

                                setIcon : DesignSystem.IconBrowser.Model.UserPickedIconSet -> DesignSystem.IconBrowser.Model.UserPickedIconSet
                                setIcon set =
                                    { set | icons = IntDict.insertNew newIcon set.icons }
                            in
                            ( { model
                                | userPickedIconSets =
                                    model.userPickedIconSets
                                        |> Dict.update forSet (Maybe.map setIcon)
                              }
                            , Cmd.none
                            )


{-|

    import DesignSystem.IconBrowser.Model
    import GitHub.Model

    parseRepoInput "https://github.com/akveo/eva-icons" --> Just <| GitHub.Model.GitHubRepoId "akveo" "eva-icons"
    parseRepoInput "https://github.com/akveo/eva-icons/pulls" --> Just <| GitHub.Model.GitHubRepoId "akveo" "eva-icons"
    parseRepoInput "akveo/eva-icons/pulls" --> Nothing
    parseRepoInput "akveo/eva-icons" --> Just <| GitHub.Model.GitHubRepoId "akveo" "eva-icons"

-}
parseRepoInput : String -> Maybe GitHub.Model.GitHubRepoId
parseRepoInput input =
    -- is full url?
    case Url.fromString input of
        Nothing ->
            case String.split "/" input of
                [ userName, repoName ] ->
                    if (Slug.parse repoName /= Nothing) && (Slug.parse userName /= Nothing) then
                        Just <| GitHub.Model.GitHubRepoId userName repoName

                    else
                        Nothing

                _ ->
                    Nothing

        Just { path } ->
            -- we take the last two parts of a URL or any given string separated by slashes and check if they parse as slugs
            case String.split "/" path |> List.filter (String.isEmpty >> not) of
                userName :: repoName :: _ ->
                    Just <| GitHub.Model.GitHubRepoId userName repoName

                _ ->
                    Nothing



---- UTILS ----


equalsUserPickedIconSet : DesignSystem.IconBrowser.Model.UserPickedIconSet -> DesignSystem.IconBrowser.Model.UserPickedIconSet -> Bool
equalsUserPickedIconSet a b =
    a.name == b.name


equalsRepo : GitHub.Model.RepoObject -> GitHub.Model.RepoObject -> Bool
equalsRepo a b =
    a.sha == b.sha


equalsIcon : GitHub.Model.RepoObject -> GitHub.Model.RepoObject -> Bool
equalsIcon a b =
    equalsRepo a b && a.name == b.name


httpErrorToString : Http.Error -> String
httpErrorToString error =
    let
        invalidRepoMsg =
            "We couldn't find the repo. Make sure you didn't misspell it."

        invalidUrlMsg =
            "That repo name seems of. Please double-check it ;)"

        networkErrorMsg =
            "Network error. Verify your internet connection"

        noTagsInRepoMsg =
            "We need the repo we use to have tags. This one does not"
    in
    case error of
        BadUrl message ->
            invalidUrlMsg

        Timeout ->
            networkErrorMsg

        NetworkError ->
            networkErrorMsg

        BadStatus status ->
            invalidRepoMsg

        BadBody message ->
            noTagsInRepoMsg



-- HELPERS
