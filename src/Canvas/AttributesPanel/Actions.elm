module Canvas.AttributesPanel.Actions exposing (view)

{-
   This module is used to on the attribute panel for the user to be able to attribute
   actions like onClick to an element.
-}

import Action
import ApiExplorer.Api
import ApiExplorer.Api.Param
import Canvas.AttributesPanel.Actions.ApiCall
import Canvas.AttributesPanel.Shared
import Canvas.Msg
import Canvas.Selection
import Dict
import Dynamic.Data
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Interface.Selection
import List.Extra
import Model
import Model.Model
import ModelEditor
import ModelEditor.Model
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Mutation
import Ui.Dropdown
import Ui.Input
import Ui.RadioRow
import Ui.Style
import Url


view :
    Canvas.Selection.SelectionItem
    -> Spec.Element.Model.Element a
    -> Model.Model.UserModel
    -> Element.Element Canvas.Msg.Msg
view selectionItem el userModel =
    case el.shared.kind of
        Spec.Element.Model.TextInput settings ->
            let
                shared =
                    el.shared

                fieldSelector =
                    viewConnectedFieldSelectorForInput userModel settings.modelField
                        |> Element.map (\newConnectedField -> Canvas.Msg.GotMutation (Spec.Mutation.UpdateShared selectionItem { shared | kind = Spec.Element.Model.TextInput { settings | modelField = newConnectedField } }))
            in
            Element.column
                (Element.spacing 15 :: Canvas.AttributesPanel.Shared.sectionStyles)
                [ Element.el [ Element.Font.bold ] (Element.text "Connected model field")
                , fieldSelector
                , Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text "The value of this field will be in sync with selected in the model" ]
                ]

        Spec.Element.Model.Button ->
            viewClickAction
                { el = el
                , userModel = userModel
                , mandatory = True
                }

        Spec.Element.Model.Box ->
            viewClickAction
                { el = el
                , userModel = userModel
                , mandatory = False
                }


{-| Render UI that allows the user to control which model field an input field is bound to
-}
viewConnectedFieldSelectorForInput : Model.Model.UserModel -> Int -> Element.Element Int
viewConnectedFieldSelectorForInput userModel connectedField =
    let
        fieldName =
            Dict.get connectedField userModel.modelEditor.fields
                |> Maybe.map .name
                |> Maybe.withDefault "Field not found"

        -- get all fields
        renderedFields =
            userModel.modelEditor.fields
                |> Dict.toList
                |> List.filterMap viewField

        viewField : ( Int, ModelEditor.Model.Field ) -> Maybe (Ui.Dropdown.Row Int)
        viewField ( key, { name, kind, comment } ) =
            let
                viewFor dataKind =
                    case dataKind of
                        Dynamic.Data.StringKind ->
                            Ui.Dropdown.viewRow
                                { isSelected = key == connectedField
                                , onSelect = key
                                , detail = Ui.Dropdown.Description comment
                                , label = Ui.Dropdown.Description name
                                , sideNote = Ui.Dropdown.Description "String"
                                , rightHandText = Nothing
                                }
                                |> Just

                        _ ->
                            Nothing
            in
            case kind of
                ModelEditor.Model.Typed dataKind ->
                    viewFor dataKind

                ModelEditor.Model.WithDefault value ->
                    viewFor (Dynamic.Data.toKind value)

                ModelEditor.Model.Unspecified ->
                    Nothing

        dropdown =
            Ui.Dropdown.view []
                { contents = renderedFields
                , label = fieldName
                }
    in
    dropdown


viewClickAction :
    { el : Spec.Element.Model.Element a
    , userModel : Model.Model.UserModel
    , mandatory : Bool
    }
    -> Element.Element Canvas.Msg.Msg
viewClickAction { el, userModel, mandatory } =
    let
        -- HEADER --
        header =
            Element.row
                [ Element.spacing 5
                , Element.paddingEach { edges | bottom = 5 }
                , Element.width Element.fill
                ]
                [ title
                , if mandatory then
                    Element.none

                  else
                    toggleActionButton
                ]

        title =
            Element.el
                [ Element.Font.bold ]
                (Element.text "On Click")

        toggleActionButton =
            Element.el
                [ Element.alignRight ]
                (Spec.Element.Id.getFromDict el.shared.id userModel.actions
                    |> Maybe.andThen .onClick
                    |> (\current -> Canvas.AttributesPanel.Shared.viewPlusOrMinusToggleButton_ { default = Action.Unspecified, current = current })
                )

        -- DESCRIPTION --
        description =
            Element.paragraph
                [ Element.Font.color Ui.Style.grey ]
                [ Element.text
                    "When this element is clicked it will take the user to an internal, an external URL or scroll to an element on the current page"
                ]

        {- Select what to do on click -}
        selectDropdown : Action.ClickAction -> Element.Element Action.ClickAction
        selectDropdown current =
            let
                goToOption =
                    Ui.Dropdown.viewRow
                        { onSelect = Action.defaultNavigate
                        , label = Ui.Dropdown.Description "Navigate"
                        , detail = Ui.Dropdown.NoDetail
                        , sideNote = Ui.Dropdown.NoDetail
                        , isSelected =
                            case current of
                                Action.Navigate _ ->
                                    True

                                _ ->
                                    False
                        , rightHandText = Nothing
                        }

                updateModelOption =
                    Ui.Dropdown.viewRow
                        { onSelect = Action.defaultUpdateModel
                        , label = Ui.Dropdown.Description "Update Model"
                        , detail = Ui.Dropdown.NoDetail
                        , sideNote = Ui.Dropdown.NoDetail
                        , isSelected =
                            case current of
                                Action.UpdateModel _ ->
                                    True

                                _ ->
                                    False
                        , rightHandText = Nothing
                        }

                makeApiCallOption =
                    Ui.Dropdown.viewRow
                        { onSelect = Action.defaultMakeApiCall
                        , label = Ui.Dropdown.Description "Make API Call"
                        , detail = Ui.Dropdown.NoDetail
                        , sideNote = Ui.Dropdown.NoDetail
                        , isSelected =
                            case current of
                                Action.MakeApiCall _ ->
                                    True

                                _ ->
                                    False
                        , rightHandText = Nothing
                        }
            in
            Ui.Dropdown.view
                []
                { contents = [ goToOption, updateModelOption, makeApiCallOption ]
                , label =
                    case current of
                        Action.Navigate _ ->
                            "Navigate"

                        Action.MakeApiCall _ ->
                            "MakeApiCall"

                        Action.UpdateModel _ ->
                            "Update Model"

                        Action.Unspecified ->
                            "Pick action..."
                }

        -- EDITOR --
        clickAction =
            maybeClickAction userModel el.shared.id

        editor =
            let
                editorView =
                    case ( clickAction, mandatory ) of
                        ( Nothing, False ) ->
                            Element.none

                        ( Nothing, True ) ->
                            actionPicker Action.Unspecified

                        ( Just a, _ ) ->
                            actionPicker a
            in
            editorView
                |> Element.map
                    (Canvas.Msg.SetActions el.shared.id
                        << Action.ActionsForElement
                        << Just
                    )

        actionPicker a =
            case a of
                Action.Unspecified ->
                    selectDropdown a

                Action.UpdateModel updateModelParams ->
                    Element.column [ Element.width Element.fill, Element.spacing 10 ]
                        [ selectDropdown a
                        , modelUpdateEditor userModel updateModelParams
                            |> Element.map Action.UpdateModel
                        ]

                Action.Navigate goToParams ->
                    Element.column [ Element.width Element.fill, Element.spacing 10 ]
                        [ selectDropdown a
                        , linkEditor userModel goToParams
                            |> Element.map Action.Navigate
                        ]

                Action.MakeApiCall apiCallParams ->
                    Element.column [ Element.width Element.fill, Element.spacing 10 ]
                        [ selectDropdown a
                        , Canvas.AttributesPanel.Actions.ApiCall.viewEditor userModel apiCallParams
                            |> Element.map Action.MakeApiCall
                        ]
    in
    Element.column
        (Element.spacing 15 :: Canvas.AttributesPanel.Shared.sectionStyles)
        [ header
            |> Element.map
                (\newOnClickAction -> Canvas.Msg.SetActions el.shared.id { onClick = newOnClickAction })
        , description
        , editor
        ]



{- all the parameters that are visible when Navigate OnClick action is activated -}


linkEditor : Model.Model.UserModel -> Action.NavigateParams -> Element.Element Action.NavigateParams
linkEditor userModel goToParams =
    let
        updateTargetLocation =
            \linkLocation -> { goToParams | linkLocation = linkLocation }

        updateOpenIn =
            \openIn -> { goToParams | openIn = openIn }

        targetPicker =
            case goToParams.linkLocation of
                Action.Scroll _ _ ->
                    Element.none

                _ ->
                    openTargetPicker goToParams.openIn
                        |> Element.map updateOpenIn

        destinationPicker linkLocation =
            case linkLocation of
                Action.Internal maybeScreenId ->
                    screenSelection userModel maybeScreenId

                Action.Scroll maybeElementId offset ->
                    scrollToEditor userModel maybeElementId offset

                Action.External url ->
                    urlInput url
    in
    Element.column
        [ Element.alignTop
        , Element.spacing 15
        , Element.width Element.fill
        ]
        [ internalExternalRadioButton userModel goToParams.linkLocation
            |> Element.map updateTargetLocation
        , destinationPicker goToParams.linkLocation
            |> Element.map updateTargetLocation
        , targetPicker
        ]


offsetPicker : Maybe Int -> Element.Element (Maybe Int)
offsetPicker int =
    Element.column [ Element.spacing 10 ]
        [ Element.paragraph [ Element.Font.color Ui.Style.grey, Element.paddingEach { edges | top = 10 } ] [ Element.text "How far above the element should the scrolling stop? This is relative to the size of the viewport." ]
        , Ui.Input.smartInt "Offset in percent" Element.Input.labelAbove int (Maybe.withDefault 0 int)
        ]


internalExternalRadioButton :
    Model.Model.UserModel
    -> Action.TargetLocation
    -> Element.Element Action.TargetLocation
internalExternalRadioButton userModel linkLocation =
    let
        internalExternalIsSelected selected comparable =
            Action.linkLocationToString comparable
                |> (==) (Action.linkLocationToString selected)
    in
    Ui.RadioRow.view
        { items =
            [ Action.External { input = "https://", valid = Nothing }
            , Action.Internal Nothing
            , Action.Scroll Nothing Nothing
            ]
        , toLabel = Action.linkLocationToString
        , selected = internalExternalIsSelected linkLocation
        }



{- |> Element.map
   (\newTargetLocation ->
       -- TODO: understand what this does
       case ( newTargetLocation, linkLocation ) of
           ( Action.External _, Action.External _ ) ->
               linkLocation

           ( Action.Internal _, Action.Internal _ ) ->
               linkLocation

           ( Action.External _, Action.Internal _ ) ->
               newTargetLocation

           ( Action.Internal _, Action.External _ ) ->
               newTargetLocation
   )
-}


screenSelection :
    Model.Model.UserModel
    -> Maybe Spec.Element.Id.Id
    -> Element.Element Action.TargetLocation
screenSelection userModel maybeScreenId =
    let
        makeLabel ( index, screen ) =
            Spec.Element.getLabel screen
                |> Maybe.withDefault ("Screen " ++ String.fromInt (index + 1))

        toDropDownRow maybeSelectedScreenIdb index thisScreen =
            Ui.Dropdown.viewRow
                { isSelected = Just thisScreen.shared.id == maybeSelectedScreenIdb
                , label = makeLabel ( index, thisScreen ) |> Ui.Dropdown.Description
                , detail = Ui.Dropdown.NoDetail
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect = Just thisScreen.shared.id
                , rightHandText = Nothing
                }

        screenSelectionDropDown =
            Ui.Dropdown.view []
                { label =
                    userModel.itemsOnCanvas
                        |> List.indexedMap Tuple.pair
                        |> List.Extra.find
                            (\( _, thisScreen ) ->
                                Just thisScreen.shared.id == maybeScreenId
                            )
                        |> Maybe.map makeLabel
                        |> Maybe.withDefault "None selected"
                , contents =
                    userModel.itemsOnCanvas
                        |> List.indexedMap (toDropDownRow maybeScreenId)
                }
    in
    Element.row []
        [ Element.text "Screen: "
        , screenSelectionDropDown
        ]
        |> Element.map Action.Internal


scrollToEditor :
    Model.Model.UserModel
    -> Maybe Spec.Element.Id.Id
    -> Maybe Int
    -> Element.Element Action.TargetLocation
scrollToEditor userModel targetId offset =
    Element.column []
        [ elementOnScreenSelection userModel targetId
            |> Element.map (\targetId_ -> Action.Scroll (Just targetId_) offset)
        , offsetPicker offset
            |> Element.map (Action.Scroll targetId)
        ]


elementOnScreenSelection :
    Model.Model.UserModel
    -> Maybe Spec.Element.Id.Id
    -> Element.Element Spec.Element.Id.Id
elementOnScreenSelection userModel maybeElementId =
    let
        makeLabel ( index, element ) =
            Spec.Element.getLabel element
                |> Maybe.withDefault (Spec.Element.Id.toHtmlIdRaw element.shared.id)

        toDropDownRow maybeSelectedElementIdb index thisElement =
            Ui.Dropdown.viewRow
                { isSelected = Just thisElement.shared.id == maybeSelectedElementIdb
                , label = makeLabel ( index, thisElement ) |> Ui.Dropdown.Description
                , detail = Ui.Dropdown.NoDetail
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect = thisElement.shared.id
                , rightHandText = Nothing
                }

        targets =
            case Canvas.Selection.toRoot userModel.selection of
                Just root ->
                    case Spec.Mutation.drillDownAndGet root userModel of
                        Just screen ->
                            Spec.Mutation.flattenChildren screen

                        Nothing ->
                            []

                Nothing ->
                    []

        elementSelectionDropDown =
            Ui.Dropdown.view []
                { label =
                    targets
                        |> List.indexedMap Tuple.pair
                        |> List.Extra.find
                            (\( _, thisElement ) ->
                                Just thisElement.shared.id == maybeElementId
                            )
                        |> Maybe.map makeLabel
                        |> Maybe.withDefault "None selected"
                , contents =
                    targets
                        |> List.indexedMap (toDropDownRow maybeElementId)
                }
    in
    Element.row []
        [ Element.text "Element: "
        , elementSelectionDropDown
        ]


urlInput url =
    let
        validateUrl : String -> Action.ExternalUrl
        validateUrl string =
            case Url.fromString string of
                Just url_ ->
                    { input = string, valid = Just url_ }

                Nothing ->
                    { input = string, valid = Nothing }

        input =
            Element.Input.text
                [ Element.width Element.fill
                , Element.Border.color Ui.Style.transparent
                , Element.mouseOver [ Element.Border.color Ui.Style.grey ]
                , Element.paddingXY 3 9
                , Element.Border.rounded 1
                , Element.Background.color
                    (if isValidUrl url.input then
                        Element.rgba 0 0 0 0

                     else
                        Element.rgba 1 0.8 0.8 1
                    )
                , Element.Font.size 12
                ]
                { onChange = validateUrl
                , text = url.input
                , placeholder =
                    Element.text "https://"
                        |> Element.Input.placeholder []
                        |> Just
                , label = Element.Input.labelLeft [] (Element.text "URL:")
                }
    in
    input
        |> Element.map Action.External


openTargetPicker : Action.OpenTarget -> Element.Element Action.OpenTarget
openTargetPicker openTarget =
    let
        selectOption =
            Ui.Dropdown.view []
                { label =
                    case openTarget of
                        Action.SameTab ->
                            "Same Tab"

                        Action.NewTab ->
                            "New Tab"
                , contents = [ optionA, optionB ]
                }

        optionA =
            Ui.Dropdown.viewRow
                { isSelected = openTarget == Action.SameTab
                , label = Ui.Dropdown.Description "Same Tab"
                , detail = Ui.Dropdown.Description "Stay on the currently opened tab and navigate inside it"
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect = Action.SameTab
                , rightHandText = Nothing
                }

        optionB =
            Ui.Dropdown.viewRow
                { isSelected = openTarget == Action.NewTab
                , label = Ui.Dropdown.Description "New Tab"
                , sideNote = Ui.Dropdown.NoDetail
                , detail = Ui.Dropdown.Description "Opens a new browser tab and navigate to it"
                , onSelect = Action.NewTab
                , rightHandText = Nothing
                }
    in
    Element.row []
        [ Element.text "Open in: "
        , selectOption
        ]


{-| Allow the user to select a target field and value that happens on an interaction
-}
modelUpdateEditor : Model.Model.UserModel -> Action.UpdateModelParams -> Element.Element Action.UpdateModelParams
modelUpdateEditor userModel updates =
    let
        -- get all fields
        renderedFields =
            userModel.modelEditor.fields
                |> Dict.toList
                |> List.map viewField

        {- View a field in the model and options to update it when an action is performed -}
        viewField : ( Int, ModelEditor.Model.Field ) -> Element.Element Action.UpdateModelParams
        viewField ( key, modelFieldSettings ) =
            let
                maybeUpdate =
                    Dict.get key updates

                header =
                    Element.row
                        [ Element.width Element.fill ]
                        [ Element.text modelFieldSettings.name
                        , button
                        ]

                button =
                    Canvas.AttributesPanel.Shared.viewPlusOrMinusToggleButton_ { default = Nothing, current = maybeUpdate }
                        |> Element.map
                            (\newUpdate ->
                                case newUpdate of
                                    Nothing ->
                                        Dict.remove key updates

                                    Just v ->
                                        Dict.insert key v updates
                            )

                apiParam =
                    Model.paramForField key userModel

                -- if an update is specified for the field, view the editor
                editor =
                    case maybeUpdate of
                        Just update ->
                            viewValueEditor apiParam modelFieldSettings update
                                |> Element.map (\newUpdate -> Dict.insert key newUpdate updates)

                        Nothing ->
                            Element.none
            in
            Element.column [ Element.width Element.fill ] [ header, editor ]

        allFields =
            Element.column [ Element.width Element.fill, Element.spacing 10 ] renderedFields
    in
    allFields


{-| Allow the user to visually define what to update a field in the model to when an action is performed.
-}
viewValueEditor :
    Maybe ApiExplorer.Api.ApiParam
    -> ModelEditor.Model.Field
    -> Maybe Dynamic.Data.Instance
    -> Element.Element (Maybe Dynamic.Data.Instance)
viewValueEditor param field valueToUpdateFieldTo =
    let
        ( locked, icon ) =
            case param |> Maybe.map ApiExplorer.Api.Param.simplify of
                Nothing ->
                    ( False, Element.none )

                Just p ->
                    ( True, p.refinedType |> Interface.Selection.refinedTypeToIcon )

        input =
            case ( field.kind, valueToUpdateFieldTo ) of
                ( ModelEditor.Model.Unspecified, _ ) ->
                    [ Element.text "Specify a type in the model editor first" ]
                        |> Element.paragraph []

                ( ModelEditor.Model.Typed t, Nothing ) ->
                    ModelEditor.controlsForWithoutDefault icon { kind = t, readOnly = True }
                        |> Element.map Tuple.second

                ( ModelEditor.Model.WithDefault initialValueAndType, Nothing ) ->
                    ModelEditor.controlsForWithoutDefault icon { kind = Dynamic.Data.toKind initialValueAndType, readOnly = True }
                        |> Element.map Tuple.second

                ( ModelEditor.Model.Typed t, Just val ) ->
                    let
                        compatible =
                            t
                                == Dynamic.Data.toKind val
                    in
                    case compatible of
                        True ->
                            ModelEditor.controlsForWithDefault icon { instance = val, readOnly = True }
                                |> Element.map Tuple.second

                        False ->
                            ModelEditor.controlsForWithoutDefault icon { kind = t, readOnly = True }
                                |> Element.map Tuple.second

                ( ModelEditor.Model.WithDefault initialValueAndType, Just val ) ->
                    let
                        initialKind =
                            Dynamic.Data.toKind initialValueAndType

                        compatible =
                            initialKind
                                == Dynamic.Data.toKind val
                    in
                    case compatible of
                        True ->
                            ModelEditor.controlsForWithDefault icon { instance = val, readOnly = True }
                                |> Element.map Tuple.second

                        False ->
                            ModelEditor.controlsForWithoutDefault icon { kind = initialKind, readOnly = True }
                                |> Element.map Tuple.second
    in
    input



---- UTILS ----


edges =
    { bottom = 0, left = 0, right = 0, top = 0 }


maybeAction userModel elementId =
    Spec.Element.Id.getFromDict elementId userModel.actions


maybeClickAction userModel elementId =
    maybeAction userModel elementId
        |> Maybe.andThen .onClick


isValidUrl : String -> Bool
isValidUrl str =
    case Url.fromString str of
        Just _ ->
            True

        Nothing ->
            False
