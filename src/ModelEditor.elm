module ModelEditor exposing (controlsForWithDefault, controlsForWithoutDefault, update, view)

import ApiExplorer.Api
import ApiExplorer.Api.Param
import Canvas.AttributesPanel.Content
import Canvas.AttributesPanel.Content.Tabs
import Canvas.Msg
import Dict
import Dynamic.Data
import Dynamic.Data.Custom
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Interface.Selection
import Model
import Model.Model
import ModelEditor.Associations
import ModelEditor.Help
import ModelEditor.Model
import ModelEditor.Msg
import Persistence
import Spec.DataConnection
import Ui.Boxicons
import Ui.Component
import Ui.DesignSystem
import Ui.Dropdown
import Ui.Help
import Ui.Input
import Ui.Style exposing (edges)


view :
    Persistence.ProjectMeta
    -> Model.Model.UserModel
    -> Element.Element Canvas.Msg.Msg
view projectMeta userModel =
    let
        model =
            userModel.modelEditor

        leftSidebar =
            Element.column
                [ Element.height Element.fill
                , Element.width (Element.minimum 200 Element.shrink)
                ]
                (buttonRow :: sidebarRows)

        sidebarRows =
            model.fields
                |> Dict.toList
                |> List.map viewFieldInSidebar

        viewFieldInSidebar : ( Int, ModelEditor.Model.Field ) -> Element.Element ModelEditor.Msg.Msg
        viewFieldInSidebar ( key, field ) =
            let
                isSelected =
                    Just key == model.selectedField

                fieldNameInput =
                    Ui.Component.contenteditable
                        { text = field.name
                        , placeholder = "Unnamed field"
                        , enabled = isSelected
                        }
                        |> Element.map (\newName -> ModelEditor.Msg.SetField key { field | name = newName })

                fieldRow =
                    Ui.DesignSystem.viewSidebarRowSimple
                        { isSelected = isSelected
                        , label =
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing 5
                                ]
                                [ Element.row [ Element.width Element.fill ]
                                    [ fieldNameInput
                                    , Ui.Component.trashCan [ Element.alignRight ] (ModelEditor.Msg.RemoveField key)
                                    ]
                                , Element.column [ Element.width Element.fill, Element.spacing 5 ] associationRows
                                ]
                        , msg = ModelEditor.Msg.SelectField key
                        , attribs = []
                        , onRemove = Nothing
                        }

                associationRows =
                    field.associations
                        |> Dict.toList
                        |> List.map
                            (\( associationKey, association ) ->
                                let
                                    associationNameInput =
                                        Ui.Component.contenteditable
                                            { text = association.name
                                            , placeholder = "Unnamed association"
                                            , enabled = isSelected
                                            }
                                            |> Element.map
                                                (\newName ->
                                                    { field
                                                        | associations =
                                                            Dict.insert
                                                                associationKey
                                                                { association | name = newName }
                                                                field.associations
                                                    }
                                                        |> ModelEditor.Msg.SetField key
                                                )
                                in
                                Element.row
                                    [ Element.width Element.fill
                                    , Element.paddingEach { left = 20, right = 0, bottom = 0, top = 0 }
                                    ]
                                    [ associationNameInput
                                    , Ui.Component.trashCan [ Element.alignRight ] (ModelEditor.Msg.RemoveAssociation key associationKey)
                                    ]
                            )
            in
            fieldRow

        helpButton =
            case model.selectedField of
                Nothing ->
                    Element.none

                Just _ ->
                    helpButton_

        helpButton_ =
            case Dict.isEmpty model.fields of
                True ->
                    Element.none

                False ->
                    Ui.Component.helpButton model ModelEditor.Msg.ShowOrHideHelpPanel

        buttonRow =
            Element.row
                [ Element.spacing 10
                , Element.padding 10
                , Element.Border.widthEach { edges | bottom = 1 }
                , Element.width Element.fill
                ]
                [ addButton
                , helpButton
                ]

        -- create and help button 😂
        addButton =
            Element.Input.button
                Ui.Component.buttonStyle
                { label = Element.text "Add field"
                , onPress = Just ModelEditor.Msg.AddField
                }

        mainBody =
            case ( model.selectedField, Dict.isEmpty model.fields || model.helpOpen ) of
                ( _, True ) ->
                    Element.column
                        [ Element.padding 20, Element.spacing 10 ]
                        [ Ui.Component.textListWithHeader
                            (not (Dict.isEmpty model.fields) && model.helpOpen)
                            "Setup the `state` of your app to modify what is displayed to a user"
                            [ "➡ The model stores the central state of your app"
                            , "➡ Use simple states like `True` or `False` to show or hide a image image"
                            , "➡ Use `Custom Types` when a state can have multiple values"
                            , "➡ Store what data a user enters into a form using `Text` or `Number`"
                            , "➡ POST form data to an API when a user clicks submit"
                            , "➡ Connect the state to your design using the Model tab in the Canvas sidebar"
                            , "➡ Component conditional rendering coming in March"
                            , "➡ STATUS: WIP, UX update coming late February"
                            ]
                        ]
                        |> Element.map (always ModelEditor.Msg.ShowOrHideHelpPanel)
                        |> Element.map Canvas.Msg.ModelEditorMsg

                ( Nothing, _ ) ->
                    Element.column [ Element.padding 20, Element.spacing 20 ]
                        [ Element.text "Select one of the fields on the left or create new field."
                        ]

                ( Just selectedField, False ) ->
                    case Dict.get selectedField model.fields of
                        Nothing ->
                            Element.el [ Element.padding 20, Element.spacing 20 ] <|
                                Element.text "Select one of the fields on the left or create new field."

                        Just field ->
                            viewSelectedField projectMeta userModel ( selectedField, field )
    in
    Ui.DesignSystem.view
        { sidebar = leftSidebar |> Element.map Canvas.Msg.ModelEditorMsg
        , mainBody = mainBody
        }


viewSelectedField : Persistence.ProjectMeta -> Model.Model.UserModel -> ( Int, ModelEditor.Model.Field ) -> Element.Element Canvas.Msg.Msg
viewSelectedField projectMeta userModel ( key, field ) =
    let
        param =
            Model.paramForField key userModel
                |> Maybe.map ApiExplorer.Api.Param.simplify

        ( locked, icon ) =
            case param of
                Nothing ->
                    ( False, Element.none )

                Just p ->
                    ( True, p.refinedType |> Interface.Selection.refinedTypeToIcon )

        mapUpdate =
            Element.map (\kind -> ModelEditor.Msg.SetField key { field | kind = kind })

        ( typePicker_, details ) =
            Tuple.mapFirst mapUpdate <|
                Tuple.mapSecond mapUpdate <|
                    case field.kind of
                        ModelEditor.Model.Unspecified ->
                            ( typePicker locked Nothing
                                |> Element.map (\type_ -> ModelEditor.Model.Typed type_)
                            , Element.none
                            )

                        ModelEditor.Model.Typed t ->
                            ( typePicker locked (Just t)
                                |> Element.map (\type_ -> ModelEditor.Model.Typed type_)
                            , controlsForWithoutDefault icon { kind = t, readOnly = False }
                                |> Element.map
                                    (\( newKind, newInstance ) ->
                                        case newInstance of
                                            Just instance ->
                                                instance
                                                    |> ModelEditor.Model.WithDefault

                                            Nothing ->
                                                newKind
                                                    |> ModelEditor.Model.Typed
                                    )
                            )

                        ModelEditor.Model.WithDefault t ->
                            ( typePicker locked (Just (Dynamic.Data.toKind t))
                                |> Element.map
                                    (\type_ -> ModelEditor.Model.Typed type_)
                            , controlsForWithDefault icon { instance = t, readOnly = False }
                                |> Element.map
                                    (\( newKind, newInstance ) ->
                                        case newInstance of
                                            Just instance ->
                                                instance
                                                    |> ModelEditor.Model.WithDefault

                                            Nothing ->
                                                newKind
                                                    |> ModelEditor.Model.Typed
                                    )
                            )

        deleteButton =
            Element.Input.button
                Ui.Component.buttonStyle
                { onPress = Just (ModelEditor.Msg.RemoveField key), label = Element.text "Remove field" }

        commentField =
            let
                input =
                    Ui.Component.contenteditable { enabled = True, text = field.comment, placeholder = "Add a comment to this field" }
                        |> Element.map (\comment -> ModelEditor.Msg.SetField key { field | comment = comment })
                        |> Element.el [ Element.alignTop ]
            in
            Element.column [ Element.spacing 10, Element.alignTop ]
                [ Element.el [ Element.Font.bold ] (Element.text "Notes:")
                , input
                ]

        leftArea =
            Element.column
                [ Element.width Element.fill
                , Element.spacing 20
                , Element.padding 20
                , Element.height Element.fill
                ]
                [ Element.row [ Element.spacing 10 ]
                    [ typePicker_ |> Element.el [ Element.width (Element.px 200) ]
                    , commentField
                    ]
                , details
                , ModelEditor.Associations.view field
                    |> Element.map
                        (\newDataOrSignal ->
                            case newDataOrSignal of
                                ModelEditor.Associations.NewData fieldData ->
                                    ModelEditor.Msg.SetField key fieldData

                                ModelEditor.Associations.Signal ( associationKey, matcherKey ) ->
                                    ModelEditor.Msg.SetField key { field | connectionWorkflowTarget = Just { associationKey = associationKey, matcherKey = matcherKey } }
                        )
                ]

        setConnection : Maybe Spec.DataConnection.DataConnection -> ModelEditor.Msg.Msg
        setConnection conn =
            ModelEditor.Msg.SetField key <|
                case field.connectionWorkflowTarget of
                    Just t ->
                        updateConnection conn t field

                    Nothing ->
                        field

        connectionPanel =
            case field.connectionWorkflowTarget of
                Nothing ->
                    Element.none

                Just connectionToWorkWith ->
                    let
                        -- check if any other matcher has been connected to content and find the right tab
                        availableTabs =
                            case Dict.get connectionToWorkWith.associationKey field.associations of
                                Nothing ->
                                    Canvas.AttributesPanel.Content.Tabs.availableTabs

                                Just assoc ->
                                    case assoc.projection of
                                        ModelEditor.Model.Match matchers ->
                                            case
                                                Dict.toList matchers
                                                    |> List.head
                                                    |> Maybe.andThen (Tuple.second >> Canvas.AttributesPanel.Content.dataConnectionTabEquivalent)
                                            of
                                                Nothing ->
                                                    Canvas.AttributesPanel.Content.Tabs.availableTabs

                                                Just oneTab ->
                                                    [ oneTab ]

                                        _ ->
                                            Canvas.AttributesPanel.Content.Tabs.availableTabs

                        maybeDataConnection =
                            case field.connectionWorkflowTarget of
                                Just { associationKey, matcherKey } ->
                                    case Dict.get associationKey field.associations of
                                        Just association ->
                                            case association.projection of
                                                ModelEditor.Model.Match matchers ->
                                                    Dict.get matcherKey matchers

                                                _ ->
                                                    Nothing

                                        Nothing ->
                                            Nothing

                                _ ->
                                    Nothing

                        closeButton =
                            Ui.Component.icon Ui.Boxicons.bxX
                                |> Element.el
                                    [ Element.Events.onClick (ModelEditor.Msg.SetField key { field | connectionWorkflowTarget = Nothing } |> Canvas.Msg.ModelEditorMsg)
                                    , Element.moveDown 15
                                    , Element.moveRight 15
                                    , Element.pointer
                                    , Element.alpha 0.5
                                    , Element.mouseOver [ Element.alpha 1 ]
                                    ]
                                |> Element.inFront

                        panel =
                            Canvas.AttributesPanel.Content.view
                                { userModel = userModel
                                , activeTab = field.activeContentTab
                                , maybeDataConnection = maybeDataConnection
                                , localOptions = []
                                , availableTabs = availableTabs
                                , projectMeta = projectMeta

                                -- TODO: prevent infinite loops / circular dependencies by associations referencing associations
                                }
                                |> Element.map
                                    (Canvas.AttributesPanel.Content.resolveOutMsg
                                        ((\tab -> { field | activeContentTab = tab }) >> ModelEditor.Msg.SetField key >> Canvas.Msg.ModelEditorMsg)
                                        (setConnection >> Canvas.Msg.ModelEditorMsg)
                                    )
                    in
                    Element.el
                        [ Element.padding 20
                        , Element.Background.color Ui.Style.lightGrey
                        , closeButton
                        , Element.width (Element.fill |> Element.maximum 400)
                        , Element.height Element.fill
                        ]
                        panel
    in
    Element.row
        [ Element.width Element.fill, Element.height Element.fill, Element.spacing 30 ]
        [ leftArea |> Element.map Canvas.Msg.ModelEditorMsg, connectionPanel ]



{-
   let
       currentRuntimeModel =
           Preview.Model.initRuntimeModel modelSpec runtimeModel
   in
   case ( Dict.get fieldKey modelSpec.associations, Dict.get fieldKey currentRuntimeModel ) of
       ( Just association, Just value ) ->
           case ( association.projection, value ) of
               ( Just ModelEditor.Model.Verbatim, Dynamic.Data.StringInstance str ) ->
                   Just <| ApiExplorer.Model.ParagraphText str

               ( Just (ModelEditor.Model.Match matchers), Dynamic.Data.UnionInstance variants ( activeVariantKey, activeTags ) ) ->
                   case Dict.get activeVariantKey matchers of
                       Just (ModelEditor.Model.Literal str) ->
                           Just <| ApiExplorer.Model.ParagraphText str

                       _ ->
                           Nothing

               _ ->
                   Nothing

       _ ->
           Nothing
-}


updateConnection :
    Maybe Spec.DataConnection.DataConnection
    -> ModelEditor.Model.ConnectTarget
    -> ModelEditor.Model.Field
    -> ModelEditor.Model.Field
updateConnection conn target =
    let
        updateAssociation : ModelEditor.Model.Association -> ModelEditor.Model.Association
        updateAssociation association =
            { association
                | projection =
                    case association.projection of
                        ModelEditor.Model.Match matchers ->
                            matchers
                                |> (case conn of
                                        Nothing ->
                                            Dict.remove target.matcherKey

                                        Just c ->
                                            Dict.insert target.matcherKey c
                                   )
                                |> ModelEditor.Model.Match

                        _ ->
                            association.projection
            }

        updateField : ModelEditor.Model.Field -> ModelEditor.Model.Field
        updateField field =
            { field
                | associations =
                    Dict.update target.associationKey
                        (Maybe.map updateAssociation)
                        field.associations
            }
    in
    updateField


ensureFieldHasRightAssociationType : ModelEditor.Model.Field -> Maybe ModelEditor.Model.Field -> Maybe ModelEditor.Model.Field
ensureFieldHasRightAssociationType newFieldSpec maybeExistingField =
    case maybeExistingField of
        Nothing ->
            Just newFieldSpec

        Just existingField ->
            case ( newFieldSpec.kind, existingField.kind ) of
                ( ModelEditor.Model.Typed (Dynamic.Data.UnionKind _), ModelEditor.Model.Typed (Dynamic.Data.UnionKind _) ) ->
                    Just newFieldSpec

                ( ModelEditor.Model.Typed (Dynamic.Data.UnionKind _), ModelEditor.Model.WithDefault (Dynamic.Data.UnionInstance _ _) ) ->
                    Just newFieldSpec

                ( ModelEditor.Model.WithDefault (Dynamic.Data.UnionInstance _ _), ModelEditor.Model.Typed (Dynamic.Data.UnionKind _) ) ->
                    Just newFieldSpec

                ( ModelEditor.Model.WithDefault (Dynamic.Data.UnionInstance _ _), ModelEditor.Model.WithDefault (Dynamic.Data.UnionInstance _ _) ) ->
                    Just newFieldSpec

                _ ->
                    Just { newFieldSpec | associations = Dict.empty }


update : ModelEditor.Msg.Msg -> ModelEditor.Model.Model -> ( ModelEditor.Model.Model, Cmd msg )
update msg model =
    case msg of
        ModelEditor.Msg.ShowOrHideHelpPanel ->
            ( { model | helpOpen = not model.helpOpen }, Cmd.none )

        ModelEditor.Msg.SelectField key ->
            ( { model | selectedField = Just key }, Cmd.none )

        ModelEditor.Msg.AddField ->
            ( (ModelEditor.Help.addField model).model, Cmd.none )

        ModelEditor.Msg.SetField key field ->
            ( { model
                | fields =
                    Dict.update key (ensureFieldHasRightAssociationType field) model.fields
              }
            , Cmd.none
            )

        ModelEditor.Msg.RemoveField key ->
            ( { model
                | fields = Dict.remove key model.fields
              }
            , Cmd.none
            )

        ModelEditor.Msg.RemoveAssociation fieldKey associationKey ->
            ( { model
                | fields =
                    model.fields
                        |> Dict.update
                            fieldKey
                            (Maybe.map (\field -> { field | associations = Dict.remove associationKey field.associations }))
              }
            , Cmd.none
            )


styleFieldSpecific =
    [ Element.spacing 20 ]


{-| Provide visual controls that allow the user to edit a union type when there is no default value defined yet
-}
controlsForWithoutDefault : Element.Element ( Dynamic.Data.Kind, Maybe Dynamic.Data.Instance ) -> { kind : Dynamic.Data.Kind, readOnly : Bool } -> Element.Element ( Dynamic.Data.Kind, Maybe Dynamic.Data.Instance )
controlsForWithoutDefault icon { kind, readOnly } =
    case kind of
        Dynamic.Data.UnionKind variants ->
            Dynamic.Data.Custom.viewInstanceEditor variants Nothing readOnly
                |> Element.map
                    (\( newVariants, newSelectedVariant ) ->
                        case newSelectedVariant of
                            Just s ->
                                ( Dynamic.Data.UnionKind newVariants, Just <| Dynamic.Data.UnionInstance newVariants s )

                            Nothing ->
                                ( Dynamic.Data.UnionKind newVariants, Nothing )
                    )

        Dynamic.Data.NumberKind ->
            Ui.Input.smartInt "Default" Element.Input.labelAbove Nothing 0
                |> Element.map (Maybe.map (toFloat >> Dynamic.Data.NumberInstance) >> Tuple.pair kind)

        Dynamic.Data.StringKind ->
            Element.row []
                [ Element.el [ Element.scale 0.7 ] icon
                , Ui.Input.string "Default" "Hello World" ""
                    |> Element.map (Dynamic.Data.StringInstance >> Just >> Tuple.pair kind)
                ]

        _ ->
            Element.text "unsupported"


{-| Same as without default but this time we have a default value associated.

NOTE: The two functions should be merged: A re-render can cause the user to lose input focus plus
they are inherently connected.

-}
controlsForWithDefault : Element.Element (Maybe Dynamic.Data.Instance) -> { instance : Dynamic.Data.Instance, readOnly : Bool } -> Element.Element ( Dynamic.Data.Kind, Maybe Dynamic.Data.Instance )
controlsForWithDefault icon { instance, readOnly } =
    Element.map (Tuple.pair (Dynamic.Data.toKind instance)) <|
        case instance of
            Dynamic.Data.UnionInstance variants activeVariant ->
                Dynamic.Data.Custom.viewInstanceEditor variants (Just activeVariant) readOnly
                    |> Element.map
                        (\( newVariants, newSelectedVariant ) ->
                            case newSelectedVariant of
                                Just s ->
                                    Just <| Dynamic.Data.UnionInstance newVariants s

                                Nothing ->
                                    Nothing
                        )

            Dynamic.Data.NumberInstance num ->
                Ui.Input.smartInt "Default" Element.Input.labelAbove (Just (round num)) (round num)
                    |> Element.map (Maybe.map (toFloat >> Dynamic.Data.NumberInstance))

            Dynamic.Data.StringInstance str ->
                Element.row []
                    [ Element.el [ Element.scale 0.7 ] icon
                    , Ui.Input.string "Default" "default" str
                        |> Element.map (Dynamic.Data.StringInstance >> Just)
                    ]

            _ ->
                Element.text "unsupported"


pickableKinds : List Dynamic.Data.Kind
pickableKinds =
    [ Dynamic.Data.UnionKind Dict.empty
    , Dynamic.Data.NumberKind
    , Dynamic.Data.StringKind
    ]


typePicker : Bool -> Maybe Dynamic.Data.Kind -> Element.Element Dynamic.Data.Kind
typePicker locked kind =
    case locked of
        True ->
            Element.el
                [ Element.padding 10
                , Element.Background.color Ui.Style.slightAccent
                , Element.Border.rounded 3
                ]
                (Element.text "Type locked")

        False ->
            let
                kindsDropdown =
                    Ui.Dropdown.view []
                        { label =
                            case kind of
                                Just k ->
                                    Dynamic.Data.humanReadableKind k

                                Nothing ->
                                    "Select a type"
                        , contents = options
                        }

                options =
                    pickableKinds
                        |> List.map
                            (\k ->
                                Ui.Dropdown.viewRow
                                    { label =
                                        Dynamic.Data.humanReadableKind k
                                            |> Ui.Dropdown.Description
                                    , isSelected = Just k == kind
                                    , onSelect = k
                                    , detail = Ui.Dropdown.Description (Dynamic.Data.kindDescription k)
                                    , sideNote = Ui.Dropdown.NoDetail
                                    , rightHandText = Nothing
                                    }
                            )
            in
            kindsDropdown
                |> Element.el
                    (if locked then
                        [ Ui.Help.noPointerEvents ]

                     else
                        []
                    )


viewKind : Maybe Dynamic.Data.Kind -> Dynamic.Data.Kind -> Element.Element Dynamic.Data.Kind
viewKind active kind =
    Element.Input.button
        (if Just kind == active then
            [ Element.Font.bold ]

         else
            []
        )
        { onPress = Just kind
        , label = Dynamic.Data.humanReadableKind kind |> Element.text
        }
