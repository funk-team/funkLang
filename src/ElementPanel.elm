module ElementPanel exposing (view)

import Action
import Canvas.Msg
import Dict
import Dynamic.Data
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ModelEditor
import ModelEditor.Model
import Spec.Element
import Ui.Component
import Ui.Input
import Ui.Overlay
import Ui.Style
import Url


view :
    Spec.Element.Id.Id
    -> Maybe (Spec.Element.Model.Element a)
    -> Model.Model.UserModel
    -> Bool
    -> Element.Attribute Canvas.Msg.Msg
view elementId el userModel isOpen =
    let
        header =
            Element.row [ Element.spacing 10, Element.width Element.fill ]
                [ Ui.Component.overLayHeaderText ("Element Editor: " ++ label)
                , Element.el [ Element.alignRight ] (Ui.Component.buttonLink "/" "Close")
                ]

        label =
            el
                |> Maybe.map Spec.Element.getLabelOrId
                |> Maybe.withDefault "Element not found"
    in
    Ui.Overlay.view
        Element.column
        isOpen
        [ header

        -- , label
        , renderActions userModel elementId
        ]


renderActions : Model.Model.UserModel -> Spec.Element.Id.Id -> Element.Element Canvas.Msg.Msg
renderActions userModel elementId =
    case Spec.Element.Id.getFromDict elementId userModel.actions of
        Nothing ->
            renderAction userModel Action.default
                |> Element.map (Canvas.Msg.SetActions elementId)

        Just actions ->
            renderAction userModel actions
                |> Element.map (Canvas.Msg.SetActions elementId)


renderAction :
    Model.Model.UserModel
    -> Action.ActionsForElement
    -> Element.Element Action.ActionsForElement
renderAction userModel actions =
    let
        { onClick } =
            actions

        onClickField : Element.Element (Maybe Action.ClickAction)
        onClickField =
            case onClick of
                Nothing ->
                    Element.row
                        [ Element.spacing 10 ]
                        [ Ui.Component.buttonOnClick
                            (Just Action.defaultGoTo)
                            "On Click Navigate"
                            False
                        , Ui.Component.buttonOnClick
                            (Just Action.defaultUpdateModel)
                            "On Click Update Model"
                            False
                        ]

                Just onClick_ ->
                    let
                        deleteButton =
                            Element.row [ Element.spacing 10 ]
                                [ Element.el [ Element.Font.size 22, Element.Font.color Ui.Style.highlightColorSolid ] (Element.text "Action 1")
                                , Ui.Component.buttonOnClick Nothing "X" False
                                ]

                        onClickActionEditor =
                            Element.row
                                [ Element.spacing 10 ]
                                [ Element.el [ Element.alignTop ] (Element.text "On Click -> ")
                                , case onClick_ of
                                    Action.GoTo goToParams ->
                                        linkEditor userModel goToParams
                                            |> Element.map Action.GoTo

                                    Action.UpdateModel modelUpdateParams ->
                                        modelUpdateEditor userModel modelUpdateParams
                                            |> Element.map Action.UpdateModel
                                ]
                    in
                    Element.column [ Element.spacing 20 ]
                        [ deleteButton
                        , onClickActionEditor
                            |> Element.map Just
                        ]
    in
    onClickField
        |> Element.map Action.ActionsForElement


linkEditor : Model.Model.UserModel -> Action.GoToParams -> Element.Element Action.GoToParams
linkEditor userModel goToParams =
    Element.column [ Element.alignTop, Element.spacing 20 ]
        [ Element.row [ Element.alignTop ]
            [ Element.el [ Element.alignTop, Element.width (Element.px 120) ] (Element.text "Go To -> ")
            , linkLocationPicker userModel goToParams.linkLocation
                |> Element.map (\linkLocation -> { goToParams | linkLocation = linkLocation })
            ]
        , Element.row [ Element.alignTop ]
            [ Element.el [ Element.alignTop, Element.width (Element.px 120) ] (Element.text "Open In -> ")
            , openTargetPicker goToParams.openIn
                |> Element.map (\openIn -> { goToParams | openIn = openIn })
            ]
        ]


{-| Allow the user to select a target field and value that happens on an interaction
-}
modelUpdateEditor : Model.Model.UserModel -> Action.UpdateModelParams -> Element.Element Action.UpdateModelParams
modelUpdateEditor userModel updateParams =
    let
        targetFields =
            ModelEditor.getNamedFields userModel.modelEditor

        targetSelector =
            targetFields
                |> Dict.toList
                |> List.map viewTarget
                |> Element.row [ Element.spacing 10 ]

        viewTarget ( key, ( name, _ ) ) =
            Element.Input.button
                [ Element.alpha
                    (if updateParams.field == Just key then
                        1

                     else
                        0.5
                    )
                ]
                { label = Element.text name, onPress = Just key }

        valueSelector =
            case updateParams.field of
                Nothing ->
                    Element.none

                Just key ->
                    case Dict.get key userModel.modelEditor.fields of
                        Just (ModelEditor.Model.WithDefault { instance }) ->
                            viewInstanceEditor (Dynamic.Data.toKind instance) updateParams.value

                        Just (ModelEditor.Model.Typed { type_ }) ->
                            viewInstanceEditor type_ updateParams.value

                        Just (ModelEditor.Model.Unspecified _) ->
                            Element.text "Define a type in your model first"

                        Nothing ->
                            Element.none
    in
    Element.column []
        [ targetSelector
            |> Element.map (\newTarget -> { updateParams | field = Just newTarget })
        , valueSelector
            |> Element.map (\newValue -> { updateParams | value = newValue })
        ]


{-| Allow the user to define an instance based on a type
-}
viewInstanceEditor : Dynamic.Data.Kind -> Maybe Dynamic.Data.Instance -> Element.Element (Maybe Dynamic.Data.Instance)
viewInstanceEditor kind instance =
    targetValueEditor kind instance


openTargetPicker : Action.OpenTarget -> Element.Element Action.OpenTarget
openTargetPicker openTarget =
    let
        optionA =
            Element.Input.button
                [ Element.alpha
                    (if openTarget == Action.SameTab then
                        1

                     else
                        0.6
                    )
                ]
                { label = Element.text "Same Tab", onPress = Just Action.SameTab }

        optionB =
            Element.Input.button
                [ Element.alpha
                    (if openTarget == Action.NewTab then
                        1

                     else
                        0.6
                    )
                ]
                { label = Element.text "New Tab", onPress = Just Action.NewTab }

        viewOptions =
            Element.column [ Element.spacing 10 ] [ optionA, optionB ]
    in
    Element.column []
        [ viewOptions
        ]


linkLocationPicker :
    Model.Model.UserModel
    -> Action.LinkLocation
    -> Element.Element Action.LinkLocation
linkLocationPicker model linkLocation =
    let
        attribsSelected =
            [ Element.padding 10
            , Element.Background.color Ui.Style.highlightColorMid
            , Element.mouseOver <|
                [ Element.Background.color <| Ui.Style.highlightColorMid ]
            , Element.Border.color Ui.Style.grey
            , Element.Font.color Ui.Style.highlightColorSolid
            , Element.Border.width 1
            , Element.alignTop
            , Element.moveUp 10
            ]

        attribsNotSelected =
            [ Element.padding 10
            , Element.Background.color Ui.Style.transparent
            , Element.mouseOver <|
                [ Element.Background.color <| Ui.Style.highlightColorLow ]
            , Element.Border.color Ui.Style.grey
            , Element.Font.color Ui.Style.black
            , Element.Border.width 1
            , Element.alignTop
            , Element.moveUp 10
            ]

        linkPicker : Element.Element Action.LinkLocation
        linkPicker =
            case linkLocation of
                Action.Internal maybeScreen ->
                    Element.column [ Element.spacing 10 ]
                        [ Element.text "Select a screen:"
                        , renderScreens maybeScreen
                            |> Element.map (Just >> Action.Internal)
                        ]

                Action.External url ->
                    let
                        isValid =
                            case url.valid of
                                Just _ ->
                                    True

                                Nothing ->
                                    False

                        styles =
                            [ Element.Font.color Ui.Style.black
                            ]

                        input =
                            Element.Input.text
                                styles
                                { onChange = validateUrl
                                , text = url.input
                                , placeholder = Nothing
                                , label = Element.Input.labelAbove [] <| Element.text "Enter URL:"
                                }
                                |> Element.map Action.External
                    in
                    if isValid then
                        input

                    else
                        input

        renderScreens : Maybe Spec.Element.Id.Id -> Element.Element Spec.Element.Id.Id
        renderScreens selected =
            List.map
                (renderScreenTarget selected)
                model.itemsOnCanvas
                |> Element.column [ Element.spacing 10 ]

        renderScreenTarget :
            Maybe Spec.Element.Id.Id
            -> Spec.Element.Model.Screen
            -> Element.Element Spec.Element.Id.Id
        renderScreenTarget selectedScreen thisScreen =
            Element.Input.button
                [ Element.alpha
                    (if Just thisScreen.shared.id == selectedScreen then
                        1

                     else
                        0.6
                    )
                ]
                { label =
                    Element.text
                        (Spec.Element.getLabelOrId thisScreen)
                , onPress = Just thisScreen.shared.id
                }

        internalExternalPickerButton =
            case linkLocation of
                Action.Internal maybeScreen ->
                    Element.row []
                        [ Element.Input.button
                            attribsNotSelected
                            { onPress = Just <| Action.External { input = "https://", valid = Nothing }
                            , label = Element.text "External"
                            }
                        , Element.el attribsSelected (Element.text "Internal")
                        ]

                Action.External value ->
                    Element.row []
                        [ Element.el attribsSelected (Element.text "External")
                        , Element.Input.button
                            attribsNotSelected
                            { onPress = Just <| Action.Internal Nothing
                            , label = Element.text "Internal"
                            }
                        ]
    in
    Element.column
        [ Element.spacing 10
        , Element.width <| Element.px 300
        ]
        [ internalExternalPickerButton, linkPicker ]


validateUrl : String -> Action.ExternalUrl
validateUrl string =
    case Url.fromString string of
        Just url ->
            { input = string, valid = Just url }

        Nothing ->
            { input = string, valid = Nothing }


{-| @@TODO: find better data structure for union types
types should generally have an ID and a constructor...
maybe store types in a central location
-}
targetValueEditor : Dynamic.Data.Kind -> Maybe Dynamic.Data.Instance -> Element.Element (Maybe Dynamic.Data.Instance)
targetValueEditor kind instance =
    case kind of
        Dynamic.Data.UnionKind variants ->
            let
                variantToHighlight =
                    case instance of
                        Just (Dynamic.Data.UnionInstance variantsOfSelectedType selecteVariant) ->
                            -- make sure we are on the right union type
                            if variantsOfSelectedType == variants then
                                Just selecteVariant

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            unionInstanceEditor variants variantToHighlight
                |> Element.map
                    (\( newVariants, newSelectedVariant ) ->
                        case newSelectedVariant of
                            Just s ->
                                Just <| Dynamic.Data.UnionInstance newVariants s

                            Nothing ->
                                Nothing
                    )

        Dynamic.Data.NumberKind ->
            let
                value =
                    case instance of
                        Just (Dynamic.Data.NumberInstance n) ->
                            Just n

                        _ ->
                            Nothing
            in
            Ui.Input.integer "On load" "0" (Maybe.map round value)
                |> Element.map (Maybe.map (toFloat >> Dynamic.Data.NumberInstance))

        Dynamic.Data.StringKind ->
            let
                value =
                    case instance of
                        Just (Dynamic.Data.StringInstance s) ->
                            s

                        _ ->
                            ""
            in
            Ui.Input.string "On load" "Hello World" value
                |> Element.map (Dynamic.Data.StringInstance >> Just)

        _ ->
            Element.text "unsupported"


unionInstanceEditor :
    Dynamic.Data.UnionConstructors
    -> Maybe Dynamic.Data.UnionValue
    -> Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
unionInstanceEditor variants activeVariant =
    let
        viewVariant : ( Int, Dynamic.Data.UnionConstructor ) -> Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
        viewVariant ( key, ( variantName, tags ) ) =
            let
                nameText : Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
                nameText =
                    let
                        label =
                            case String.trim variantName of
                                "" ->
                                    "Select " ++ ModelEditor.indexToFood key

                                _ ->
                                    "Select " ++ variantName
                    in
                    Element.text label

                selectionIndicator : Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
                selectionIndicator =
                    if Maybe.map Tuple.first activeVariant == Just key then
                        Element.text "selected"

                    else
                        Element.Input.button
                            [ Element.alpha 0.5 ]
                            { label = Element.text "Set active"
                            , onPress = Just ( variants, Just ( key, [] ) )
                            }
            in
            Element.row [ Element.spacing 10 ]
                [ nameText
                , selectionIndicator

                -- , Element.text "Union tags not supported yet"
                ]

        variantsList : Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
        variantsList =
            case Dict.isEmpty variants of
                True ->
                    Element.text "Define a variant to select it as default first"

                False ->
                    variants
                        |> Dict.toList
                        |> List.map viewVariant
                        |> Element.column [ Element.spacing 10, Element.padding 10 ]
    in
    variantsList
