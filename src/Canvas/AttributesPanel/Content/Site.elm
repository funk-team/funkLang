module Canvas.AttributesPanel.Content.Site exposing (viewConnectedList, viewOptions)

{-| Display the pieces of data from an API that have been pre-selected by a user.
-}

import ApiExplorer.Model
import ApiExplorer.Site
import Canvas.AttributesPanel.Content.Shared
import Canvas.Msg
import Dict
import Element
import Element.Input
import Spec.DataConnection
import Ui.Boxicons
import Ui.Component


viewByte : ApiExplorer.Site.Model -> ApiExplorer.Site.TaggedContentByte -> Element.Element Canvas.Msg.Msg
viewByte siteData byte =
    Element.column []
        [ ApiExplorer.Site.preview siteData byte
        , Element.Input.button [] { onPress = Nothing, label = Element.text "Use" }
        ]


viewList : ApiExplorer.Site.Model -> ApiExplorer.Site.ListPath -> Element.Element Spec.DataConnection.DataConnection
viewList siteData listPath =
    case ApiExplorer.Site.getList listPath siteData of
        Nothing ->
            Element.none

        Just list ->
            let
                preview =
                    ApiExplorer.Site.previewList list

                useButton =
                    Element.Input.button
                        []
                        { onPress =
                            Just (Spec.DataConnection.StaticList list)
                        , label = Element.text "Use List"
                        }
            in
            Element.column [] [ preview, useButton ]


viewOptions : Spec.Element.Id.Id -> ApiExplorer.Model.Model -> Element.Element Canvas.Msg.Msg
viewOptions id apiExplorer =
    let
        siteSources =
            Dict.toList apiExplorer.sources
                |> List.filterMap
                    (\( _, source ) ->
                        case source.detail of
                            ApiExplorer.Model.SiteApiSpec ({ selection, request, lists } as siteData) ->
                                case ( selection, lists ) of
                                    ( [], [] ) ->
                                        Nothing

                                    _ ->
                                        let
                                            viewListOptins =
                                                (Element.column [] <| List.map (viewList siteData) siteData.lists)
                                                    |> Element.map
                                                        (Canvas.Msg.MakeDataConnection id)
                                        in
                                        Element.column [ Element.padding 5 ]
                                            [ Element.text source.name
                                            , Element.column [] <| List.map (viewByte siteData) selection
                                            , viewListOptins
                                            ]
                                            |> Just

                            _ ->
                                Nothing
                    )

        content =
            case siteSources of
                [] ->
                    Element.text "no scraped content"

                some ->
                    Element.column [] siteSources
    in
    Element.column [] [ Element.text "From sites", content ]



-- @@TODO: this should actually be in CMS :rolling-eyes:


viewConnectedList id staticList elementStyles =
    let
        refinementOptions =
            case staticList of
                ApiExplorer.Site.ImageList _ ->
                    Canvas.AttributesPanel.Content.Shared.imageCropSelector id elementStyles

                ApiExplorer.Site.TextList _ ->
                    Canvas.AttributesPanel.Content.Shared.typographyStyleSelector id elementStyles

                ApiExplorer.Site.RichTextList _ ->
                    Canvas.AttributesPanel.Content.Shared.typographyStyleSelector id elementStyles

        connectedListUi =
            [ Element.text "Connected to static list"
            , refinementOptions
            , removeButton
            ]

        removeButton =
            Ui.Component.button Nothing Ui.Boxicons.bxX "Remove Content" False
                |> Element.map (Canvas.AttributesPanel.Content.Shared.msgFromMaybe id)
    in
    Element.column [] connectedListUi
