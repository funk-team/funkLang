module Canvas.AttributesPanel.Content.Icon exposing (view)

{-| This module is used to render the content->icon side panel
-}

import Canvas.AttributesPanel.Shared
import DesignSystem.IconBrowser
import DesignSystem.IconBrowser.Model
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import IntDict
import Spec.DataConnection
import Ui.Style


view :
    IntDict.Dict DesignSystem.IconBrowser.Model.UserPickedIconSet
    -> Maybe DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet
    -> Element.Element Spec.DataConnection.DataConnection
view customIconSets maybeActiveIconRef =
    let
        hint =
            Element.text "Hint: You can change the color of icons by using the Typography color settings in the \"Style\" tab above."
                |> List.singleton
                |> Element.paragraph (Element.alpha 0.5 :: Canvas.AttributesPanel.Shared.sectionStyles)

        content =
            customIconSets
                |> Dict.toList
                |> List.filterMap (viewCustomIconSet maybeActiveIconRef)
    in
    if List.isEmpty content then
        Element.paragraph
            [ Element.padding 15
            , Element.width Element.fill
            , Element.Font.color Ui.Style.grey
            ]
            (Element.text "Use the Design mode to select icons"
                |> List.singleton
            )

    else
        Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            (content ++ [ hint ])


viewCustomIconSet :
    Maybe DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet
    -> ( Int, DesignSystem.IconBrowser.Model.UserPickedIconSet )
    -> Maybe (Element.Element Spec.DataConnection.DataConnection)
viewCustomIconSet maybeActiveIconRef ( customIconSetId, customIconSet ) =
    let
        header =
            Element.el
                [ Element.spacing 5
                , Element.paddingEach { edges | bottom = 5 }
                ]
                (Element.el [ Element.Font.bold ] <| Element.text customIconSet.name)

        iconSelection =
            let
                content =
                    customIconSet.icons
                        |> Dict.toList
                        |> List.map (viewIcon customIconSetId maybeActiveIconRef)
            in
            Element.wrappedRow
                [ Element.spacing 5
                , Element.paddingEach { edges | top = 15 }
                , Element.width Element.fill
                ]
                content
    in
    if Dict.isEmpty customIconSet.icons then
        Nothing

    else
        Element.column
            Canvas.AttributesPanel.Shared.sectionStyles
            [ header
            , iconSelection
            ]
            |> Just


viewIcon :
    Int
    -> Maybe DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet
    -> ( Int, DesignSystem.IconBrowser.Model.LocallyCachedIcon )
    -> Element.Element Spec.DataConnection.DataConnection
viewIcon customIconSetId maybeActiveIconRef ( iconId, icon ) =
    let
        wrapAttribs =
            [ Element.Events.onClick
                (Spec.DataConnection.Icon
                    (DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet customIconSetId iconId)
                )
            , Element.padding 3
            ]
                |> List.append
                    (if isActive maybeActiveIconRef customIconSetId iconId then
                        [ Element.Background.color Ui.Style.highlightColor
                        , Element.Border.rounded 6
                        ]

                     else
                        []
                    )

        icon_ =
            DesignSystem.IconBrowser.viewLocallyCachedIcon icon
                |> Element.el [ Element.width (Element.px 25) ]
    in
    icon_ |> Element.el wrapAttribs



---- UTILS ----


edges =
    { top = 0, right = 0, left = 0, bottom = 0 }


isActive maybeActiveIconRef customIconSetId iconId =
    maybeActiveIconRef
        |> Maybe.map
            (\activeIconRef ->
                DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet customIconSetId iconId
                    |> (==) activeIconRef
            )
        |> Maybe.withDefault False
