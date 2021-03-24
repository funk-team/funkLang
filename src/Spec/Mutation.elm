module Spec.Mutation exposing (..)

{-| The changes that are possible to the spec are called a mutation

TODO: put everything into a mutation and think about real-time stuff as well as logux

-}

import Action
import Canvas.Events
import Canvas.Selection
import Canvas.Tool.Draw.Model
import DesignSystem.Color
import DesignSystem.Shadow
import DesignSystem.Typography
import Dict
import Interface.Data
import Interface.Scope
import List.Extra
import ModelEditor.Help
import Rectangle
import Spec
import Spec.DataConnection
import Spec.Element
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Padding
import Spec.Element.Model
import Spec.Element.Style
import Spec.Model
import Ui.ColorPicker.Advanced


defaultElementStyles =
    Spec.Element.Style.default Spec.Element.Model.Box


{-| Update the styles in the spec using an update function
-}
setStyles :
    Spec.Element.Id.Id
    -> (Spec.Element.Style.Style -> Spec.Element.Style.Style)
    -> Spec.Model.WithSpec a
    -> Spec.Model.WithSpec a
setStyles id fn spec =
    { spec
        | elementStyles =
            Spec.Element.Id.updateDict
                id
                (Maybe.withDefault defaultElementStyles >> fn >> Just)
                spec.elementStyles
    }


{-| Any layout modification the user can do is corresponds to a mutation
Lateron we can stream this mutation between clients
in order ement realtime collaboration
-}
type Mutation
    = AddElementToScene (AddData Spec.Element.Model.ScreenSize)
    | CompleteDraw
        { drawResultData : Canvas.Tool.Draw.Model.ElementDrawResultData
        , id : Spec.Element.Id.Id
        , mode : Canvas.Tool.Draw.Model.Mode
        }
    | CompleteTransform Canvas.Selection.SelectionItem Spec.Element.Model.EitherElement
    | MoveElementToScene Spec.Element.Model.Screen
    | UpdateScreen Canvas.Selection.SelectionItem Spec.Element.Model.Screen
    | UpdateAbsoluteElement Canvas.Selection.SelectionItem Spec.Element.Model.AbsoluteElement
    | UpdateFlowElement Canvas.Selection.SelectionItem Spec.Element.Model.FlowElement
    | UpdateShared Canvas.Selection.SelectionItem Spec.Element.Model.Shared
    | SetLabel Canvas.Selection.SelectionItem String
    | ChangeElementKind Canvas.Selection.SelectionItem Canvas.Tool.Draw.Model.Mode
    | Delete Canvas.Selection.SelectionItem
      -- Typography
    | SetTypoOnElement Spec.Element.Id.Id Spec.Element.Style.TypographySelection DesignSystem.Typography.TypoTab
    | UpdateTypo Int DesignSystem.Typography.Typo
      -- Shadow
    | SetShadowOnElement Spec.Element.Id.Id Spec.Element.Style.ShadowSelection
    | UpdateShadows Int DesignSystem.Shadow.Shadow
      -- Color
    | UpdateSwatchInColorSystem Ui.ColorPicker.Advanced.SwatchUpdate
    | BatchMutation (List Mutation)
      -- copy paste updates
    | Paste PasteData
    | PasteScreen Spec.Element.Model.AbsoluteElement
    | SetAction Spec.Element.Id.Id Action.ActionsForElement
    | SetStyle Spec.Element.Id.Id Spec.Element.Style.Style
    | SetDataConnection Spec.Element.Id.Id Spec.DataConnection.DataConnection


type alias PasteData =
    { selectionItem : Canvas.Selection.SelectionItem
    , children : List Spec.Element.Model.AbsoluteElement
    }


type alias AddData geometry =
    { containments : Containments
    , id : Spec.Element.Id.Id
    , resultingRect : geometry
    }


{-| Functional ternay
-}
whenThenElse : (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
whenThenElse condition hit miss data =
    if condition data then
        hit data

    else
        miss data


screenToAbsolute :
    { outerGeometry : Spec.Element.Model.ScreenSize, shared : Spec.Element.Model.Shared }
    -> Spec.Element.Model.Element Canvas.Events.AbsoluteRectangle
screenToAbsolute { shared, outerGeometry } =
    { shared = shared
    , outerGeometry = screenSizeToAbsoluteRectangle outerGeometry
    }


screenSizeToAbsoluteRectangle : Spec.Element.Model.ScreenSize -> Canvas.Events.AbsoluteRectangle
screenSizeToAbsoluteRectangle outerGeometry =
    Spec.Element.screenSizeToRectangle outerGeometry
        |> Canvas.Events.AbsoluteRectangle


implementMutation : Mutation -> MutationImplementation model
implementMutation mutation =
    case mutation of
        BatchMutation mutations ->
            BatchedImplementation (List.map implementMutation mutations)

        UpdateSwatchInColorSystem swatchUpdate ->
            TargetedAtWholeSpec <|
                \spec ->
                    let
                        designSystem =
                            spec.designSystem

                        newDesignSystem =
                            { designSystem
                                | colorEditor =
                                    DesignSystem.Color.updateSwatch swatchUpdate designSystem.colorEditor
                            }
                    in
                    { spec | designSystem = newDesignSystem }

        UpdateShadows id_ updatedShadow ->
            TargetedAtWholeSpec <|
                \spec ->
                    let
                        designSystem =
                            spec.designSystem

                        shadows_ =
                            spec.designSystem.shadows

                        newDesignSystem =
                            { designSystem
                                | shadows =
                                    { shadows_
                                        | shadows =
                                            Dict.update
                                                id_
                                                (always (Just updatedShadow))
                                                designSystem.shadows.shadows
                                    }
                            }
                    in
                    { spec | designSystem = newDesignSystem }

        UpdateTypo id_ updatedTypo ->
            TargetedAtWholeSpec <|
                \spec ->
                    let
                        designSystem =
                            spec.designSystem

                        typo_ =
                            spec.designSystem.typoEditor

                        newDesignSystem =
                            { designSystem
                                | typoEditor =
                                    { typo_
                                        | typos =
                                            Dict.update
                                                id_
                                                (always (Just updatedTypo))
                                                designSystem.typoEditor.typos
                                    }
                            }
                    in
                    { spec | designSystem = newDesignSystem }

        SetStyle id styles ->
            TargetedAtWholeSpec <|
                \spec ->
                    { spec
                        | elementStyles =
                            Spec.Element.Id.insertIntoDict
                                id
                                styles
                                spec.elementStyles
                    }

        SetDataConnection id dataConnection ->
            TargetedAtWholeSpec <|
                \spec ->
                    { spec
                        | dataConnections =
                            Spec.Element.Id.insertIntoDict
                                id
                                dataConnection
                                spec.dataConnections
                    }

        SetAction id action ->
            TargetedAtWholeSpec <|
                \spec ->
                    { spec
                        | actions =
                            Spec.Element.Id.insertIntoDict
                                id
                                action
                                spec.actions
                    }

        SetShadowOnElement id newShadowSelection ->
            TargetedAtWholeSpec <| setStyles id (\styles -> { styles | shadow = newShadowSelection })

        SetTypoOnElement id newTypoSelection elementType ->
            TargetedAtWholeSpec <|
                setStyles id
                    (\({ placeholderText, elementText } as styles) ->
                        { styles
                            | elementText =
                                case elementType of
                                    DesignSystem.Typography.Box ->
                                        newTypoSelection

                                    DesignSystem.Typography.Placeholder ->
                                        elementText
                            , placeholderText =
                                case elementType of
                                    DesignSystem.Typography.Box ->
                                        -- placeholderText
                                        newTypoSelection

                                    DesignSystem.Typography.Placeholder ->
                                        -- newTypoSelection
                                        placeholderText
                        }
                    )

        SetLabel selection label ->
            let
                apply_ =
                    Spec.Element.mapShared (\shared -> { shared | label = label })
            in
            { onScreen = apply_ >> Just
            , onAbsoluteElement = apply_ >> Just
            , onFlowElement = apply_ >> Just
            }
                |> TargetedAtSpecificElement selection

        UpdateShared selection shared ->
            let
                apply_ =
                    Spec.Element.mapShared
                        (always shared)
            in
            { onScreen = apply_ >> Just
            , onAbsoluteElement = apply_ >> Just
            , onFlowElement = apply_ >> Just
            }
                |> TargetedAtSpecificElement selection

        MoveElementToScene el ->
            let
                mutate : Spec.Model.WithSpec model -> Spec.Model.WithSpec model
                mutate spec =
                    { spec | itemsOnCanvas = spec.itemsOnCanvas ++ [ el ] }
            in
            TargetedAtWholeSpec mutate

        AddElementToScene el ->
            let
                mutate : Spec.Model.WithSpec model -> Spec.Model.WithSpec model
                mutate spec =
                    let
                        rootEls : List Spec.Element.Model.Screen
                        rootEls =
                            addToGeneration
                                el
                                { absolute = spec.itemsOnCanvas
                                , flow = []
                                }
                                |> .absolute
                    in
                    { spec | itemsOnCanvas = rootEls }
            in
            TargetedAtWholeSpec mutate

        -- { spec | itemsOnCanvas = spec.itemsOnCanvas ++  }
        Delete sel ->
            TargetedAtSpecificElement sel
                (ElementMutator
                    (always Nothing)
                    (always Nothing)
                    (always Nothing)
                )

        ChangeElementKind selectionItem elementKind ->
            case elementKind of
                Canvas.Tool.Draw.Model.Box ->
                    let
                        apply_ =
                            Just
                                << Spec.Element.mapShared
                                    (\shared -> { shared | kind = Spec.Element.Model.Box })

                        mutate =
                            ElementMutator apply_ apply_ apply_
                    in
                    TargetedAtSpecificElement selectionItem mutate

                Canvas.Tool.Draw.Model.Button ->
                    let
                        apply_ =
                            Just
                                << Spec.Element.mapShared
                                    (\shared -> { shared | kind = Spec.Element.Model.Button })

                        mutate =
                            ElementMutator apply_ apply_ apply_
                    in
                    TargetedAtSpecificElement selectionItem mutate

                Canvas.Tool.Draw.Model.TextInput ->
                    let
                        updateElementMutation : Int -> MutationImplementation model
                        updateElementMutation fieldId =
                            let
                                apply_ =
                                    Just
                                        << Spec.Element.mapShared
                                            (\shared -> { shared | kind = Spec.Element.Model.TextInput { modelField = fieldId, placeholder = "" } })

                                mutate =
                                    ElementMutator apply_ apply_ apply_
                            in
                            TargetedAtSpecificElement selectionItem mutate

                        specUpdate spec =
                            let
                                modelEditorResult =
                                    ModelEditor.Help.addTextField spec.modelEditor
                            in
                            ( { spec | modelEditor = modelEditorResult.model }, modelEditorResult.fieldId )
                    in
                    DependentImplementation specUpdate updateElementMutation

                Canvas.Tool.Draw.Model.Text ->
                    let
                        apply_ =
                            Just
                                << Spec.Element.mapShared
                                    (\shared -> { shared | kind = Spec.Element.Model.Box })

                        mutate =
                            ElementMutator apply_ apply_ apply_
                    in
                    TargetedAtSpecificElement selectionItem mutate

        Paste { selectionItem, children } ->
            let
                apply_ =
                    Just
                        << Spec.Element.mapShared
                            (\shared -> { shared | children = Spec.Element.Model.AbsoluteChildren children })

                mutator : ElementMutator
                mutator =
                    { onScreen = apply_
                    , onFlowElement = apply_
                    , onAbsoluteElement = apply_
                    }
            in
            TargetedAtSpecificElement selectionItem mutator

        PasteScreen element ->
            TargetedAtWholeSpec
                (\spec ->
                    let
                        screenSize : Spec.Element.Model.ScreenSize
                        screenSize =
                            Spec.getAbsoluteElementRectangle
                                spec
                                Interface.Scope.empty
                                element.outerGeometry
                                |> (\(Canvas.Events.ElementRectangle rect) ->
                                        rect
                                            |> Rectangle.moveTo newScreenPosition
                                            |> Canvas.Events.AbsoluteRectangle
                                   )
                                |> Spec.Element.Model.Custom

                        newScreen =
                            { shared = element.shared
                            , outerGeometry = screenSize
                            }

                        -- align with top of other screen and make 300 px distance
                        -- if artboard is empty, put at 0 0
                        newScreenPosition =
                            spec.itemsOnCanvas
                                |> List.map (.outerGeometry >> Spec.Element.screenSizeToRectangle)
                                |> (\sizes ->
                                        let
                                            maxX =
                                                List.maximum (List.map Rectangle.x2 sizes)

                                            minY =
                                                List.minimum (List.map Rectangle.y1 sizes)
                                        in
                                        { x =
                                            maxX
                                                |> Maybe.map ((+) 200)
                                                |> Maybe.withDefault 0
                                        , y =
                                            minY |> Maybe.withDefault 0
                                        }
                                   )
                    in
                    { spec
                        | itemsOnCanvas = spec.itemsOnCanvas ++ [ newScreen ]
                    }
                )

        CompleteDraw { id, mode, drawResultData } ->
            let
                implementation : MutationImplementation model
                implementation =
                    case mode of
                        Canvas.Tool.Draw.Model.TextInput ->
                            let
                                newElMutation : Int -> MutationImplementation model
                                newElMutation fieldId =
                                    let
                                        newEl =
                                            makeNewTextInput
                                                { id = id
                                                , modelField = fieldId
                                                , geo = Spec.Element.createAbsoluteElementDimensions drawResultData.snappedRect
                                                }

                                        siblings =
                                            [ newEl ] ++ drawResultData.notContainedSiblings ++ drawResultData.containedSiblings

                                        apply_ =
                                            Just
                                                << Spec.Element.mapShared
                                                    (\shared -> { shared | children = Spec.Element.Model.AbsoluteChildren siblings })

                                        mutate =
                                            ElementMutator apply_ apply_ apply_
                                    in
                                    TargetedAtSpecificElement drawResultData.selectionItem mutate

                                specUpdate spec =
                                    let
                                        modelEditorResult =
                                            ModelEditor.Help.addTextField spec.modelEditor
                                    in
                                    ( { spec | modelEditor = modelEditorResult.model }, modelEditorResult.fieldId )
                            in
                            DependentImplementation specUpdate newElMutation

                        Canvas.Tool.Draw.Model.Text ->
                            let
                                newEl =
                                    makeNewTextBox
                                        id
                                        (Spec.Element.createAbsoluteElementDimensions drawResultData.snappedRect)
                                        drawResultData.containedSiblings

                                siblings =
                                    [ newEl ] ++ drawResultData.notContainedSiblings

                                apply_ =
                                    Just
                                        << Spec.Element.mapShared
                                            (\shared -> { shared | children = Spec.Element.Model.AbsoluteChildren siblings })

                                exampleText =
                                    Spec.DataConnection.Static
                                        (Interface.Data.ParagraphText "Click here to edit.")

                                insertNewDataConnectionWithExampleText : Spec.Model.WithSpec model -> Spec.Model.WithSpec model
                                insertNewDataConnectionWithExampleText spec =
                                    { spec
                                        | dataConnections =
                                            Spec.Element.Id.insertIntoDict
                                                id
                                                exampleText
                                                spec.dataConnections
                                    }

                                mutate =
                                    ElementMutator apply_ apply_ apply_
                            in
                            BatchedImplementation
                                [ TargetedAtSpecificElement drawResultData.selectionItem mutate
                                , TargetedAtWholeSpec insertNewDataConnectionWithExampleText
                                ]

                        Canvas.Tool.Draw.Model.Box ->
                            let
                                newEl =
                                    makeNewBox
                                        id
                                        (Spec.Element.createAbsoluteElementDimensions drawResultData.snappedRect)
                                        drawResultData.containedSiblings

                                siblings =
                                    [ newEl ] ++ drawResultData.notContainedSiblings

                                apply_ =
                                    Just
                                        << Spec.Element.mapShared
                                            (\shared -> { shared | children = Spec.Element.Model.AbsoluteChildren siblings })

                                mutate =
                                    ElementMutator apply_ apply_ apply_
                            in
                            TargetedAtSpecificElement drawResultData.selectionItem mutate

                        Canvas.Tool.Draw.Model.Button ->
                            let
                                newEl =
                                    makeNewButton
                                        id
                                        (Spec.Element.createAbsoluteElementDimensions drawResultData.snappedRect)
                                        drawResultData.containedSiblings

                                siblings =
                                    [ newEl ] ++ drawResultData.notContainedSiblings

                                apply_ =
                                    Just
                                        << Spec.Element.mapShared
                                            (\shared -> { shared | children = Spec.Element.Model.AbsoluteChildren siblings })

                                mutate =
                                    ElementMutator apply_ apply_ apply_
                            in
                            TargetedAtSpecificElement drawResultData.selectionItem mutate
            in
            implementation

        CompleteTransform selection newElement ->
            let
                eitherToMutation : Spec.Element.Model.EitherElement -> ElementMutator
                eitherToMutation either =
                    case either.outerGeometry of
                        Spec.Element.Model.ScreenGeometry screenSize ->
                            { dontMutate
                                | onScreen =
                                    Just << always { shared = either.shared, outerGeometry = screenSize }
                            }

                        Spec.Element.Model.AbsoluteElementGeometry absoluteElementGeometry ->
                            { dontMutate
                                | onAbsoluteElement =
                                    Just << always { shared = either.shared, outerGeometry = absoluteElementGeometry }
                            }

                        Spec.Element.Model.FlowElementGeometry flowElementGeometry ->
                            { dontMutate
                                | onFlowElement =
                                    Just << always { shared = either.shared, outerGeometry = flowElementGeometry }
                            }
            in
            TargetedAtSpecificElement selection (eitherToMutation newElement)

        UpdateScreen selection newData ->
            TargetedAtSpecificElement selection
                { onScreen = always newData >> Just
                , onFlowElement = Just
                , onAbsoluteElement = Just
                }

        UpdateFlowElement selection newData ->
            TargetedAtSpecificElement selection
                { onScreen = Just
                , onFlowElement = always newData >> Just
                , onAbsoluteElement = Just
                }

        UpdateAbsoluteElement selection newData ->
            TargetedAtSpecificElement selection
                { onScreen = Just
                , onFlowElement = Just
                , onAbsoluteElement = always newData >> Just
                }


type MutationImplementation model
    = TargetedAtSpecificElement Canvas.Selection.SelectionItem ElementMutator
    | TargetedAtWholeSpec (Spec.Model.WithSpec model -> Spec.Model.WithSpec model)
    | BatchedImplementation (List (MutationImplementation model))
    | DependentImplementation (Spec.Model.WithSpec model -> ( Spec.Model.WithSpec model, Int )) (Int -> MutationImplementation model)


dontMutate =
    ElementMutator (always Nothing) (always Nothing) (always Nothing)


type alias ElementMutator =
    { onScreen : Spec.Element.Model.Screen -> Maybe Spec.Element.Model.Screen
    , onFlowElement : Spec.Element.Model.FlowElement -> Maybe Spec.Element.Model.FlowElement
    , onAbsoluteElement : Spec.Element.Model.AbsoluteElement -> Maybe Spec.Element.Model.AbsoluteElement
    }


apply :
    Mutation
    -> Spec.Model.WithSpec model
    -> Spec.Model.WithSpec model
apply mutationSpec =
    use (implementMutation mutationSpec)


use :
    MutationImplementation model
    -> Spec.Model.WithSpec model
    -> Spec.Model.WithSpec model
use implementation spec =
    case implementation of
        DependentImplementation first second ->
            let
                ( newSpec, dependentData ) =
                    first spec
            in
            use (second dependentData) newSpec

        TargetedAtSpecificElement sel mutator ->
            drillDownAndMutate mutator sel spec

        TargetedAtWholeSpec mutate ->
            mutate spec

        BatchedImplementation mutations ->
            List.foldl
                (\mutation latestSpec -> use mutation latestSpec)
                spec
                mutations



-- recursion hack...


drillDeep_ =
    drillDeep


matches id =
    .shared >> .id >> (==) id


{-| Functional if
-}
when : (a -> Bool) -> (a -> a) -> a -> a
when condition fn data =
    if condition data then
        fn data

    else
        data


{-| Happy path
-}
drillDownAndMutate :
    ElementMutator
    -> Canvas.Selection.SelectionItem
    -> Spec.Model.WithSpec model
    -> Spec.Model.WithSpec model
drillDownAndMutate mutate selection spec =
    case Canvas.Selection.toTargetSelection selection of
        Canvas.Selection.TargetScreen id ->
            let
                itemsOnCanvas =
                    spec.itemsOnCanvas
                        |> List.filterMap (whenThenElse (matches id) mutate.onScreen Just)
            in
            { spec
                | itemsOnCanvas = itemsOnCanvas
            }

        Canvas.Selection.TargetElement deeperSelection ->
            case deeperSelection.path of
                idOfElementToGoInto :: deeperPath ->
                    let
                        itemsOnCanvas =
                            spec.itemsOnCanvas
                                |> List.map (when (matches idOfElementToGoInto) (drillDeep mutate { deeperSelection | path = deeperPath }))
                    in
                    { spec
                        | itemsOnCanvas = itemsOnCanvas
                    }

                [] ->
                    let
                        itemsOnCanvas =
                            spec.itemsOnCanvas
                                |> List.map
                                    (when
                                        (matches deeperSelection.parent)
                                        (drillToDirectChild mutate deeperSelection)
                                    )
                    in
                    { spec
                        | itemsOnCanvas = itemsOnCanvas
                    }


{-| When the path has been completely resolved
-}
drillToDirectChild :
    ElementMutator
    -> Canvas.Selection.PathToElement
    -> Spec.Element.Model.Element shared
    -> Spec.Element.Model.Element shared
drillToDirectChild mutate deeperSelection parentOfTarget =
    let
        s =
            parentOfTarget.shared

        shared =
            { s
                | children =
                    case s.children of
                        Spec.Element.Model.AbsoluteChildren abs ->
                            (abs |> List.filterMap (whenThenElse (matches deeperSelection.target) mutate.onAbsoluteElement Just))
                                |> Spec.Element.Model.AbsoluteChildren

                        Spec.Element.Model.FlowChildren ch ->
                            (ch |> List.filterMap (whenThenElse (matches deeperSelection.target) mutate.onFlowElement Just))
                                |> Spec.Element.Model.FlowChildren
            }
    in
    { parentOfTarget | shared = shared }


{-| Resolve the path
-}
drillDeep : ElementMutator -> Canvas.Selection.PathToElement -> Spec.Element.Model.Element shared -> Spec.Element.Model.Element shared
drillDeep mutate deeperSelection element =
    case deeperSelection.path of
        -- drill as far as the path goes
        idOfElementToGoInto :: deeper ->
            let
                goDeeper : Spec.Element.Model.Element shared -> Spec.Element.Model.Element shared
                goDeeper element_ =
                    let
                        s =
                            element_.shared

                        shared =
                            { s
                                | children =
                                    case s.children of
                                        Spec.Element.Model.FlowChildren ch ->
                                            ch
                                                |> List.map
                                                    (when
                                                        (matches idOfElementToGoInto)
                                                        (drillDeep_ mutate { deeperSelection | path = deeper })
                                                    )
                                                |> Spec.Element.Model.FlowChildren

                                        Spec.Element.Model.AbsoluteChildren abs ->
                                            abs
                                                |> List.map
                                                    (when
                                                        (matches idOfElementToGoInto)
                                                        (drillDeep_ mutate { deeperSelection | path = deeper })
                                                    )
                                                |> Spec.Element.Model.AbsoluteChildren
                            }
                    in
                    { element_
                        | shared = shared
                    }
            in
            goDeeper element

        [] ->
            let
                s =
                    element.shared

                shared =
                    { s
                        | children =
                            case s.children of
                                Spec.Element.Model.FlowChildren ch ->
                                    ch
                                        |> List.map
                                            (when
                                                (matches deeperSelection.parent)
                                                (drillToDirectChild mutate deeperSelection)
                                            )
                                        |> Spec.Element.Model.FlowChildren

                                Spec.Element.Model.AbsoluteChildren abs ->
                                    abs
                                        |> List.map
                                            (when
                                                (matches deeperSelection.parent)
                                                (drillToDirectChild mutate deeperSelection)
                                            )
                                        |> Spec.Element.Model.AbsoluteChildren
                    }
            in
            { element
                | shared = shared
            }


getById : Spec.Element.Id.Id -> Spec.Model.WithSpec model -> Maybe Spec.Element.Model.EitherElement
getById id { itemsOnCanvas } =
    let
        flat =
            List.concatMap
                (flattenOne Spec.Element.wrapScreen)
                itemsOnCanvas
    in
    List.Extra.find (\el -> el.shared.id == id) flat


{-| turn a parent element and its children into a flat list of elements
parent :: children
-}
flattenOne :
    (Spec.Element.Model.Element a -> Spec.Element.Model.EitherElement)
    -> Spec.Element.Model.Element a
    -> List Spec.Element.Model.EitherElement
flattenOne wrap el =
    wrap el :: flattenChildren el


flattenOne_ =
    flattenOne


flattenChildren =
    Spec.Element.getChildren
        >> List.concatMap (flattenOne_ identity)


{-| Just get an element using a SelectionItem
-}
drillDownAndGet : Canvas.Selection.SelectionItem -> Spec.Model.WithSpec model -> Maybe Spec.Element.Model.EitherElement
drillDownAndGet selection spec =
    case Canvas.Selection.toTargetSelection selection of
        Canvas.Selection.TargetScreen id ->
            spec.itemsOnCanvas
                |> List.Extra.find (matches id)
                |> Maybe.map Spec.Element.wrapScreen

        Canvas.Selection.TargetElement deeperSelection ->
            getHelp deeperSelection spec.itemsOnCanvas


getHelp_ =
    getHelp


{-| This function uses a DeeperSelection (path + parent of target + target) to find the right element in a nested element structure
-}
getHelp : Canvas.Selection.PathToElement -> List (Spec.Element.Model.Element a) -> Maybe Spec.Element.Model.EitherElement
getHelp deeperSelection_ elements =
    case deeperSelection_.path of
        -- if the path is empty find the target
        [] ->
            elements
                -- find the parent
                |> List.Extra.find (\{ shared } -> shared.id == deeperSelection_.parent)
                -- find the target
                |> Maybe.andThen
                    (\parentOfTarget ->
                        let
                            s =
                                parentOfTarget.shared

                            found =
                                case s.children of
                                    Spec.Element.Model.AbsoluteChildren ch ->
                                        List.Extra.find (\{ shared } -> shared.id == deeperSelection_.target) ch
                                            |> Maybe.map Spec.Element.wrapAbsolute

                                    Spec.Element.Model.FlowChildren ch ->
                                        List.Extra.find (\{ shared } -> shared.id == deeperSelection_.target) ch
                                            |> Maybe.map Spec.Element.wrapFlow
                        in
                        found
                    )

        idOfElementToGoInto :: deeperPath ->
            elements
                -- use the next ID to find the right child
                |> List.Extra.find (\{ shared } -> shared.id == idOfElementToGoInto)
                |> Maybe.andThen
                    (\foundChild ->
                        Spec.Element.getChildren foundChild
                            |> getHelp_ { deeperSelection_ | path = deeperPath }
                    )


type alias Containments =
    List ( Spec.Element.Id.Id, Canvas.Events.ElementRectangle )


type alias ProcessedContainments a =
    ( List Spec.Element.Model.AbsoluteElement, List (Spec.Element.Model.Element a) )


{-| If an element was contained by an operation
then bake it as an element rectangle relative to its parent
-}
bakeFromContainment :
    Containments
    -> Spec.Element.Model.Element a
    -> Maybe Spec.Element.Model.AbsoluteElement
bakeFromContainment containments element =
    let
        sameId =
            Tuple.first >> (==) element.shared.id

        maybeContainedAs =
            List.Extra.find
                sameId
                containments
    in
    case maybeContainedAs of
        Just ( _, newGeo ) ->
            Just
                { outerGeometry =
                    Spec.Element.createAbsoluteElementDimensions newGeo
                , shared = element.shared
                }

        Nothing ->
            Nothing


{-| Calculate which elements were not contained
and turn other elements into Spec.Element.Model.AbsoluteElements for putting them inside the new parent
-}
processContainments :
    Containments
    -> List (Spec.Element.Model.Element a)
    -> ProcessedContainments a
processContainments containments elements =
    List.foldr
        (\element ( inside_, outside_ ) ->
            case bakeFromContainment containments element of
                Nothing ->
                    ( inside_, element :: outside_ )

                Just contained ->
                    ( contained :: inside_, outside_ )
        )
        ( [], [] )
        elements


makeNewTextInput :
    { id : Spec.Element.Id.Id
    , geo : geo
    , modelField : Int
    }
    -> Spec.Element.Model.Element geo
makeNewTextInput { id, geo, modelField } =
    { outerGeometry = geo
    , shared =
        { id = id
        , children = Spec.Element.Model.AbsoluteChildren [] -- input field can't have children
        , padding = Spec.Element.Layout.Padding.defaultPadding
        , spacing = Nothing
        , flow = Spec.Element.Layout.Column
        , label = ""
        , kind = Spec.Element.Model.TextInput { modelField = modelField, placeholder = "Placeholder Text" }
        }
    }


makeNewBox : Spec.Element.Id.Id -> geo -> List Spec.Element.Model.AbsoluteElement -> Spec.Element.Model.Element geo
makeNewBox id geo absoluteChildren =
    { outerGeometry = geo
    , shared =
        { id = id
        , children = Spec.Element.Model.AbsoluteChildren absoluteChildren
        , padding = Spec.Element.Layout.Padding.defaultPadding
        , spacing = Nothing
        , flow = Spec.Element.Layout.Column
        , label = ""
        , kind = Spec.Element.Model.Box
        }
    }


makeNewTextBox : Spec.Element.Id.Id -> geo -> List Spec.Element.Model.AbsoluteElement -> Spec.Element.Model.Element geo
makeNewTextBox id geo absoluteChildren =
    { outerGeometry = geo
    , shared =
        { id = id
        , children = Spec.Element.Model.AbsoluteChildren absoluteChildren
        , padding = Spec.Element.Layout.Padding.defaultPadding
        , spacing = Nothing
        , flow = Spec.Element.Layout.Column
        , label = ""
        , kind = Spec.Element.Model.Box
        }
    }


makeNewButton : Spec.Element.Id.Id -> geo -> List Spec.Element.Model.AbsoluteElement -> Spec.Element.Model.Element geo
makeNewButton id geo absoluteChildren =
    { outerGeometry = geo
    , shared =
        { id = id
        , children = Spec.Element.Model.AbsoluteChildren absoluteChildren
        , padding = Spec.Element.Layout.Padding.defaultPadding
        , spacing = Nothing
        , flow = Spec.Element.Layout.Column
        , label = ""
        , kind = Spec.Element.Model.Button
        }
    }


{-| -}
addToGeneration :
    { id : Spec.Element.Id.Id, containments : Containments, resultingRect : geo }
    -> { absolute : List (Spec.Element.Model.Element geo), flow : List Spec.Element.Model.FlowElement }
    -> { absolute : List (Spec.Element.Model.Element geo), flow : List Spec.Element.Model.FlowElement }
addToGeneration newElData elements =
    let
        ( childrenFromAbsolute, siblingsAfterContainments ) =
            processContainments
                newElData.containments
                elements.absolute

        ( childrenFromFlow, flow ) =
            processContainments
                newElData.containments
                elements.flow

        newEl =
            makeNewBox
                newElData.id
                newElData.resultingRect
                (childrenFromAbsolute ++ childrenFromFlow)

        absolute =
            siblingsAfterContainments ++ [ newEl ]
    in
    { absolute = absolute, flow = flow }



-- FOR DELETE


{-| only keep elements that are not within the selection
-}
filterOutSelected ids =
    let
        notSelected el =
            not (List.member el.shared.id ids)
    in
    List.filter notSelected
