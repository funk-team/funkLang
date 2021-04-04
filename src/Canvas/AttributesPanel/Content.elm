module Canvas.AttributesPanel.Content exposing
    ( dataConnectionTabEquivalent
    , resolveOutMsg
    , view
    , viewForElement
    )

{-

   NB: we use 'active' to describe the data that is connected with the element
   and we use 'selected' to describe the menu/tab that is currently open.
   In the app you can see the active tab because it is blue and
   the selected one because it is bold.
-}
-- import Canvas.AttributesPanel.Content.Site

import Canvas.AttributesPanel.Content.Api
import Canvas.AttributesPanel.Content.Icon
import Canvas.AttributesPanel.Content.Model
import Canvas.AttributesPanel.Content.Static
import Canvas.AttributesPanel.Content.Tabs
import Canvas.AttributesPanel.Shared
import Canvas.Msg
import Canvas.Selection
import DesignSystem.IconBrowser.Model
import Element
import Element.Font
import Interface.Data
import Interface.Model
import Interface.Scope
import Model.Model
import Persistence
import Spec.DataConnection
import Spec.Element.Id
import Spec.Element.Model
import Spec.Mutation
import Ui.Input
import Ui.Style
import Ui.Tabs


{-| The attributes panel can emit one of two events

either changing the tab or emitting a new connection

-}
type OutMsg
    = NewTab Canvas.AttributesPanel.Content.Tabs.ContentTab
    | UpdateConnection (Maybe Spec.DataConnection.DataConnection)


{-| Combine two different possible messages into one
-}
resolveOutMsg :
    (Canvas.AttributesPanel.Content.Tabs.ContentTab -> msg)
    -> (Maybe Spec.DataConnection.DataConnection -> msg)
    -> OutMsg
    -> msg
resolveOutMsg tabMsg dataConnectionMsg outMsg =
    case outMsg of
        NewTab t ->
            tabMsg t

        UpdateConnection conn ->
            dataConnectionMsg conn


type alias ParamsOnElement =
    { id : Spec.Element.Id.Id
    , selectionItem : Canvas.Selection.SelectionItem

    -- why having id when it is alredy found in selectionItem?
    , userModel : Model.Model.UserModel
    , element : Spec.Element.Model.EitherElement
    , projectMeta : Persistence.ProjectMeta
    }


viewForElement : ParamsOnElement -> Element.Element Canvas.Msg.Msg
viewForElement params =
    let
        shared =
            params.element.shared
    in
    case shared.kind of
        Spec.Element.Model.TextInput inputSettings ->
            let
                placeholderInput =
                    Ui.Input.string "Placeholder" "" inputSettings.placeholder
                        |> Element.map (\newPlaceholder -> Canvas.Msg.GotMutation (Spec.Mutation.UpdateShared params.selectionItem { shared | kind = Spec.Element.Model.TextInput { inputSettings | placeholder = newPlaceholder } }))

                info =
                    Element.paragraph [ Element.Font.color Ui.Style.grey ]
                        [ Element.text "The placeholder is displayed when the input field is empty." ]
            in
            Element.column
                (Element.spacing 15 :: Canvas.AttributesPanel.Shared.sectionStyles)
                [ placeholderInput
                , info
                ]

        _ ->
            let
                { id, userModel } =
                    params

                maybeDataConnection =
                    Spec.Element.Id.getFromDict
                        id
                        userModel.dataConnections

                localOptions =
                    Interface.Scope.findDataScopesForSelection
                        userModel
                        params.selectionItem
            in
            view
                (Params userModel
                    localOptions
                    maybeDataConnection
                    Canvas.AttributesPanel.Content.Tabs.availableTabs
                    userModel.attributesPanelContentTab
                    params.projectMeta
                )
                |> Element.map
                    (resolveOutMsg
                        Canvas.Msg.AttributesPanelContentSubTabChanged
                        (\conn ->
                            case conn of
                                Just connection ->
                                    Canvas.Msg.MakeDataConnection id connection

                                Nothing ->
                                    Canvas.Msg.RemoveDataConnection id
                        )
                    )


type alias Params =
    { userModel : Model.Model.UserModel
    , localOptions : List Interface.Model.ListScopePointer
    , maybeDataConnection : Maybe Spec.DataConnection.DataConnection
    , availableTabs : List Canvas.AttributesPanel.Content.Tabs.ContentTab
    , activeTab : Canvas.AttributesPanel.Content.Tabs.ContentTab
    , projectMeta : Persistence.ProjectMeta
    }


view :
    Params
    -> Element.Element OutMsg
view params =
    Element.column
        [ Element.centerX
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewPanelTabs
            params
            |> Element.map NewTab
        , viewPanelBody params
            |> Element.map UpdateConnection
        ]


viewPanelTabs :
    Params
    -> Element.Element Canvas.AttributesPanel.Content.Tabs.ContentTab
viewPanelTabs { activeTab, availableTabs, maybeDataConnection } =
    Ui.Tabs.tabsWithActiveState
        Canvas.AttributesPanel.Content.Tabs.contentTabToString
        availableTabs
        activeTab
        (maybeDataConnection
            |> Maybe.andThen dataConnectionTabEquivalent
        )


viewPanelBody :
    Params
    -> Element.Element (Maybe Spec.DataConnection.DataConnection)
viewPanelBody { activeTab, userModel, localOptions, maybeDataConnection, projectMeta } =
    case activeTab of
        Canvas.AttributesPanel.Content.Tabs.Icons ->
            Canvas.AttributesPanel.Content.Icon.view
                userModel.designSystem.iconBrowser.userPickedIconSets
                (maybeIconDataConnection maybeDataConnection)
                |> Element.map Just

        Canvas.AttributesPanel.Content.Tabs.Api ->
            Canvas.AttributesPanel.Content.Api.view
                localOptions
                (maybeApiDataConnection maybeDataConnection)
                userModel
                |> Element.map (Spec.DataConnection.FromInterface >> Just)

        Canvas.AttributesPanel.Content.Tabs.Text ->
            maybeStaticDataConnection maybeDataConnection
                |> Canvas.AttributesPanel.Content.Static.viewTextEditor

        Canvas.AttributesPanel.Content.Tabs.Media ->
            maybeStaticDataConnection maybeDataConnection
                |> Canvas.AttributesPanel.Content.Static.viewMedia projectMeta

        Canvas.AttributesPanel.Content.Tabs.Model ->
            Canvas.AttributesPanel.Content.Model.viewOptions maybeDataConnection userModel



---- UTILS ----


{-| This is used to determine which tab to highlight

    import Spec.DataConnection
    import Canvas.AttributesPanel.Content.Tabs
    import Interface.JsonTree
    import Interface.Model
    import Interface.JsonTree.Model
    import ApiExplorer.Model
    import ApiExplorer.Model
    import Interface.Data

    dataConnectionTabEquivalent <| Spec.DataConnection.FromInterface { kind = Nothing, interfaceId = Interface.Model.ApiCallKey 0 }
    --> Nothing

    Spec.DataConnection.FromInterface
        { kind = Just (Interface.Model.RelativeToInterfaceRoot { rootPath = [Interface.JsonTree.Model.ObjectAccessor "backdrop_path"] })
        , interfaceId = Interface.Model.ApiCallKey 0
        }
    |> dataConnectionTabEquivalent
    --> Just Canvas.AttributesPanel.Content.Tabs.Api

    Spec.DataConnection.Static (Interface.Data.YoutubeEmbed {videoId = "areisotneioar"})
    |> dataConnectionTabEquivalent
    --> Just Canvas.AttributesPanel.Content.Tabs.Media

-}
dataConnectionTabEquivalent :
    Spec.DataConnection.DataConnection
    -> Maybe Canvas.AttributesPanel.Content.Tabs.ContentTab
dataConnectionTabEquivalent dataConnection =
    case dataConnection of
        Spec.DataConnection.FromValidation _ ->
            Just Canvas.AttributesPanel.Content.Tabs.Model

        Spec.DataConnection.FromInterface { kind } ->
            case kind of
                Just _ ->
                    Just Canvas.AttributesPanel.Content.Tabs.Api

                Nothing ->
                    Nothing

        Spec.DataConnection.FromModel _ ->
            Just Canvas.AttributesPanel.Content.Tabs.Model

        Spec.DataConnection.Static (Interface.Data.YoutubeEmbed _) ->
            Just Canvas.AttributesPanel.Content.Tabs.Media

        Spec.DataConnection.Static _ ->
            Just Canvas.AttributesPanel.Content.Tabs.Text

        Spec.DataConnection.Embed _ ->
            Just Canvas.AttributesPanel.Content.Tabs.Media

        Spec.DataConnection.Media _ ->
            Just Canvas.AttributesPanel.Content.Tabs.Media

        --        Spec.DataConnection.StaticList _ ->
        --            Just Canvas.AttributesPanel.Content.Tabs.Text
        Spec.DataConnection.Icon _ ->
            Just Canvas.AttributesPanel.Content.Tabs.Icons


maybeIconDataConnection :
    Maybe Spec.DataConnection.DataConnection
    -> Maybe DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet
maybeIconDataConnection maybeDataConnection =
    maybeDataConnection
        |> Maybe.andThen
            (\dataConnection ->
                case dataConnection of
                    Spec.DataConnection.Icon iconRef ->
                        Just iconRef

                    _ ->
                        Nothing
            )


maybeStaticDataConnection :
    Maybe Spec.DataConnection.DataConnection
    -> Maybe Interface.Data.RefinedValue
maybeStaticDataConnection maybeDataConnection =
    maybeDataConnection
        |> Maybe.andThen
            (\dataConnection ->
                case dataConnection of
                    Spec.DataConnection.Static refinedContent ->
                        Just refinedContent

                    Spec.DataConnection.Embed info ->
                        Just (Interface.Data.YoutubeEmbed info)

                    Spec.DataConnection.Media mediaInfo ->
                        Just (Interface.Data.Media mediaInfo)

                    _ ->
                        Nothing
            )


maybeApiDataConnection :
    Maybe Spec.DataConnection.DataConnection
    -> Maybe Interface.Model.InterfacePointer
maybeApiDataConnection maybeDataConnection =
    maybeDataConnection
        |> Maybe.andThen
            (\dataConnection ->
                case dataConnection of
                    Spec.DataConnection.FromInterface src ->
                        Just src

                    _ ->
                        Nothing
            )
