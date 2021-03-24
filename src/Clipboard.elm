port module Clipboard exposing (..)

{-| TODO: integrate with undo? or defer?

  - caveats: linked design system things will not be transferred between projects

-}

import Canvas.Camera.Convert
import Canvas.Events
import Canvas.Selection
import Clipboard.Model
import Clipboard.Msg
import Dom
import Interface.Scope
import Json.Decode as Decode
import List.Extra
import Maybe.Extra
import Model
import Model.Model
import Random
import Rectangle
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Mutation


set : Clipboard.Model.Contents -> Clipboard.Model.Model
set =
    Just


noFocus ( a, b ) =
    ( a, b, Nothing )


{-| Process any copy / paste actions

The third element of the return triplet is the new screen to focus on it as it is automatically placed

-}
update :
    Clipboard.Msg.Msg
    -> Model.Model.Model
    -> ( Model.Model.Model, Cmd Clipboard.Msg.Msg )
update msg model =
    case msg of
        Clipboard.Msg.NoOp ->
            ( model, Cmd.none )

        Clipboard.Msg.ClipboardChanged contents ->
            ( { model
                | clipboard = set contents
              }
            , Cmd.none
            )

        Clipboard.Msg.Copy ->
            case clip model of
                Nothing ->
                    ( model, Cmd.none )

                Just ( contents, _ ) ->
                    ( { model
                        | clipboard = set contents
                      }
                    , storeClipboard_ contents
                    )

        Clipboard.Msg.Cut ->
            case clip model of
                Nothing ->
                    ( model, Cmd.none )

                Just ( contents, selectionItem ) ->
                    let
                        newModel =
                            { model
                                | clipboard = set contents
                            }
                                |> Model.mapPending (Spec.Mutation.apply (Spec.Mutation.Delete selectionItem))
                    in
                    ( newModel, storeClipboard_ contents )

        Clipboard.Msg.Paste inside ->
            case model.clipboard of
                Nothing ->
                    ( model, Cmd.none )

                Just contents ->
                    let
                        ( newContents, newSeed ) =
                            injectNewIds
                                model.seed
                                contents

                        model_ =
                            { model
                                | seed = newSeed
                                , clipboard = Just newContents
                            }
                    in
                    case
                        (Model.latest model_).selection
                            |> Maybe.andThen
                                (if inside then
                                    Just

                                 else
                                    Canvas.Selection.removeOne
                                )
                    of
                        Nothing ->
                            let
                                ( mutation, newSelection ) =
                                    bakeMutationAndSelectionForScreen model_ newContents

                                newModel =
                                    model_
                                        |> Model.mapPending (Spec.Mutation.apply mutation)
                                        |> Model.mapPending (\userModel -> { userModel | selection = newSelection })
                            in
                            ( newModel, Cmd.none )

                        Just selectionItem ->
                            case bakeMutationAndSelection selectionItem model_ newContents of
                                Just ( mutation, newSelection ) ->
                                    let
                                        newModel =
                                            model_
                                                |> Model.mapPending (Spec.Mutation.apply mutation)
                                                |> Model.mapPending (\userModel -> { userModel | selection = newSelection })
                                    in
                                    ( newModel, Cmd.none )

                                Nothing ->
                                    ( model_, Cmd.none )



-- TODOS
-- make other mutations
-- make random seends


{-| generates a map of new IDs for a tree
-}
getIdMap : Random.Seed -> Spec.Element.Model.EitherElement -> ( Spec.Element.Id.Dict Spec.Element.Id.Id, Random.Seed )
getIdMap seed element =
    let
        newIdReducer : Spec.Element.Id.Id -> ( List ( Spec.Element.Id.Id, Spec.Element.Id.Id ), Random.Seed ) -> ( List ( Spec.Element.Id.Id, Spec.Element.Id.Id ), Random.Seed )
        newIdReducer nextId ( entries, seed_ ) =
            let
                ( newId, newSeed ) =
                    Spec.Element.Id.random seed_
            in
            ( ( nextId, newId ) :: entries, newSeed )

        ( ids, returnSeed ) =
            Spec.Mutation.flattenOne identity element
                |> List.map (.shared >> .id)
                |> List.foldl newIdReducer ( [], seed )

        dict =
            Spec.Element.Id.dictFromList ids
    in
    ( dict, returnSeed )



-- TWO FUNCTIONS REQUIRED BECAUSE OF ELM'S TYPING SYSTEM?


mapTreeAbs :
    (Spec.Element.Id.Id -> Spec.Element.Id.Id)
    -> Spec.Element.Model.AbsoluteElement
    -> Spec.Element.Model.AbsoluteElement
mapTreeAbs fn ({ shared } as element) =
    { element
        | shared =
            { shared
                | id = fn shared.id
                , children =
                    case shared.children of
                        Spec.Element.Model.FlowChildren ch ->
                            List.map (mapTreeFlow fn) ch
                                |> Spec.Element.Model.FlowChildren

                        Spec.Element.Model.AbsoluteChildren ch ->
                            List.map (mapTreeAbs fn) ch
                                |> Spec.Element.Model.AbsoluteChildren
            }
    }


mapTreeFlow :
    (Spec.Element.Id.Id -> Spec.Element.Id.Id)
    -> Spec.Element.Model.FlowElement
    -> Spec.Element.Model.FlowElement
mapTreeFlow fn ({ shared } as element) =
    { element
        | shared =
            { shared
                | id = fn shared.id
                , children =
                    case shared.children of
                        Spec.Element.Model.FlowChildren ch ->
                            List.map (mapTreeFlow fn) ch
                                |> Spec.Element.Model.FlowChildren

                        Spec.Element.Model.AbsoluteChildren ch ->
                            List.map (mapTreeAbs fn) ch
                                |> Spec.Element.Model.AbsoluteChildren
            }
    }


injectNewIds : Random.Seed -> Clipboard.Model.Contents -> ( Clipboard.Model.Contents, Random.Seed )
injectNewIds seed contents =
    let
        ( idMap, newSeed ) =
            contents.element
                |> Spec.Element.wrapAbsolute
                |> getIdMap seed

        replaceId : Spec.Element.Id.Id -> Spec.Element.Id.Id
        replaceId oldId =
            Spec.Element.Id.getFromDict oldId idMap
                |> Maybe.withDefault oldId

        replaceIdOnRelated : ( Spec.Element.Id.Id, a ) -> Maybe ( Spec.Element.Id.Id, a )
        replaceIdOnRelated ( oldId, related ) =
            case Spec.Element.Id.getFromDict oldId idMap of
                Nothing ->
                    Nothing

                Just newId ->
                    Just ( newId, related )

        element =
            mapTreeAbs replaceId contents.element
    in
    ( { contents
        | element = element
        , dataConnections =
            contents.dataConnections
                |> List.filterMap replaceIdOnRelated
        , styles =
            contents.styles
                |> List.filterMap replaceIdOnRelated
        , actions =
            contents.actions
                |> List.filterMap replaceIdOnRelated
      }
    , newSeed
    )


bakeMutationAndSelectionForScreen : Model.Model.Model -> Clipboard.Model.Contents -> ( Spec.Mutation.Mutation, Canvas.Selection.Selection )
bakeMutationAndSelectionForScreen model content =
    let
        mutations =
            Spec.Mutation.PasteScreen content.element
                :: bakeOtherMutations content
                |> Spec.Mutation.BatchMutation
    in
    ( mutations, Just (Canvas.Selection.fresh content.element.shared.id) )


bakeMutationAndSelection :
    Canvas.Selection.SelectionItem
    -> Model.Model.Model
    -> Clipboard.Model.Contents
    -> Maybe ( Spec.Mutation.Mutation, Canvas.Selection.Selection )
bakeMutationAndSelection selectionItem model content =
    case prepChildren selectionItem model content of
        Just ( pasteMutation, pastedElementId ) ->
            let
                mutation =
                    Spec.Mutation.Paste pasteMutation
                        :: bakeOtherMutations content
                        |> Spec.Mutation.BatchMutation
            in
            Just ( mutation, Just (Canvas.Selection.addOne pastedElementId selectionItem) )

        Nothing ->
            Nothing


bakeOtherMutations content =
    let
        actionMutations =
            content.actions
                |> List.map (\( id, action ) -> Spec.Mutation.SetAction id action)

        dataConnectionMutations =
            content.dataConnections
                |> List.map (\( id, dataConnection ) -> Spec.Mutation.SetDataConnection id dataConnection)

        styleMutations =
            content.styles
                |> List.map (\( id, dataConnection ) -> Spec.Mutation.SetStyle id dataConnection)
    in
    actionMutations
        ++ styleMutations
        ++ dataConnectionMutations


{-| find the soon-to-be siblings of the element to paste and prepare it
-}
prepChildren :
    Canvas.Selection.SelectionItem
    -> Model.Model.Model
    -> Clipboard.Model.Contents
    -> Maybe ( Spec.Mutation.PasteData, Spec.Element.Id.Id )
prepChildren selectionItem model ({ element } as content) =
    let
        targetId =
            Canvas.Selection.getTargetId selectionItem

        userModel =
            Model.latest model

        -- only paste-in-place when the parent has not changed
        adjustedElement =
            case Just targetId == content.originalParentId of
                True ->
                    element

                False ->
                    let
                        (Canvas.Events.ElementRectangle rect) =
                            Spec.getAbsoluteElementRectangle
                                userModel
                                Interface.Scope.empty
                                element.outerGeometry
                    in
                    { element
                        | outerGeometry =
                            Spec.Element.createAbsoluteElementDimensions
                                (Rectangle.moveTo
                                    { x = 0, y = 0 }
                                    rect
                                    |> Canvas.Events.ElementRectangle
                                )
                    }
    in
    case Spec.Mutation.drillDownAndGet selectionItem userModel of
        Nothing ->
            Nothing

        Just targetElement ->
            case targetElement.shared.children of
                Spec.Element.Model.AbsoluteChildren abs ->
                    Just
                        ( { selectionItem = selectionItem
                          , children = abs ++ [ adjustedElement ]
                          }
                        , adjustedElement.shared.id
                        )

                Spec.Element.Model.FlowChildren ch ->
                    let
                        readSize =
                            Dom.getBoundingClientRect model.dom

                        childrenSizes =
                            ch
                                |> List.map (\{ shared } -> readSize shared.id |> Maybe.map (Tuple.pair shared.id))
                                |> Maybe.Extra.combine

                        absSiblings : Maybe (List Spec.Element.Model.AbsoluteElement)
                        absSiblings =
                            case Maybe.map2 Tuple.pair childrenSizes (readSize targetElement.shared.id) of
                                Just ( childrenDimensiosn, parentSceneRect ) ->
                                    let
                                        relSiblings =
                                            childrenDimensiosn
                                                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle userModel.camera))
                                                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle userModel.camera parentSceneRect)))
                                                |> List.map (Tuple.mapSecond (\(Canvas.Events.ElementRectangle r_) -> r_))
                                    in
                                    ch
                                        |> List.map (flowToAbsolute relSiblings)
                                        |> Maybe.Extra.combine

                                Nothing ->
                                    Nothing

                        result =
                            case absSiblings of
                                Just sibls ->
                                    Just
                                        ( { selectionItem = selectionItem
                                          , children = sibls ++ [ adjustedElement ]
                                          }
                                        , adjustedElement.shared.id
                                        )

                                Nothing ->
                                    Nothing
                    in
                    result


flowToAbsolute : List ( Spec.Element.Id.Id, Rectangle.Rectangle ) -> Spec.Element.Model.FlowElement -> Maybe Spec.Element.Model.AbsoluteElement
flowToAbsolute relSiblings el =
    relSiblings
        |> List.Extra.find (\( id, _ ) -> id == el.shared.id)
        |> Maybe.map
            (\( _, relativeDimensions ) ->
                { shared = el.shared
                , outerGeometry =
                    Canvas.Events.ElementRectangle relativeDimensions
                        |> Spec.Element.createAbsoluteElementDimensions
                }
            )


clip : Model.Model.Model -> Maybe ( Clipboard.Model.Contents, Canvas.Selection.SelectionItem )
clip model =
    let
        userModel =
            Model.latest model
    in
    case userModel.selection of
        Nothing ->
            Nothing

        Just selectionItem ->
            case Spec.Mutation.drillDownAndGet selectionItem userModel of
                Nothing ->
                    Nothing

                Just element ->
                    let
                        dimensions =
                            Dom.getBoundingClientRect model.dom element.shared.id

                        absoluteElement : Maybe Spec.Element.Model.AbsoluteElement
                        absoluteElement =
                            case element.outerGeometry of
                                Spec.Element.Model.AbsoluteElementGeometry abs ->
                                    Just { shared = element.shared, outerGeometry = abs }

                                Spec.Element.Model.FlowElementGeometry flow ->
                                    case dimensions of
                                        Nothing ->
                                            Nothing

                                        Just d ->
                                            let
                                                (Canvas.Events.AbsoluteRectangle geo) =
                                                    Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle
                                                        userModel.camera
                                                        d

                                                res =
                                                    flowToAbsolute
                                                        [ ( element.shared.id, geo ) ]
                                                        { shared = element.shared
                                                        , outerGeometry = flow
                                                        }
                                            in
                                            res

                                Spec.Element.Model.ScreenGeometry screen ->
                                    let
                                        abs =
                                            Spec.Element.screenSizeToRectangle screen
                                                |> Canvas.Events.ElementRectangle
                                                |> Spec.Element.createAbsoluteElementDimensions
                                    in
                                    Just { shared = element.shared, outerGeometry = abs }

                        -- find dimensions
                        -- fix offset
                    in
                    case absoluteElement of
                        Nothing ->
                            Nothing

                        Just absEl ->
                            let
                                id =
                                    Canvas.Selection.getTargetId selectionItem

                                actions =
                                    ids
                                        |> List.filterMap (\id_ -> Spec.Element.Id.getFromDict id_ userModel.actions |> Maybe.map (Tuple.pair id_))

                                dataConnections =
                                    ids
                                        |> List.filterMap (\id_ -> Spec.Element.Id.getFromDict id_ userModel.dataConnections |> Maybe.map (Tuple.pair id_))

                                styles =
                                    ids
                                        |> List.filterMap (\id_ -> Spec.Element.Id.getFromDict id_ userModel.elementStyles |> Maybe.map (Tuple.pair id_))

                                ids =
                                    Spec.Mutation.flattenOne identity (absEl |> Spec.Element.wrapAbsolute)
                                        |> List.map (.shared >> .id)

                                contents =
                                    { element = absEl
                                    , originalParentId =
                                        selectionItem
                                            |> Canvas.Selection.removeOne
                                            |> Maybe.map Canvas.Selection.getTargetId
                                    , actions = actions
                                    , dataConnections = dataConnections
                                    , styles = styles
                                    }
                            in
                            Just
                                ( contents, selectionItem )


storeClipboard_ : Clipboard.Model.Contents -> Cmd msg
storeClipboard_ =
    Clipboard.Model.encodeContents >> storeClipboard


port clipboardChanged : (Decode.Value -> msg) -> Sub msg


port storeClipboard : Decode.Value -> Cmd msg


subscriptions : Sub Clipboard.Msg.Msg
subscriptions =
    clipboardChanged handleClipboardChange


handleClipboardChange : Decode.Value -> Clipboard.Msg.Msg
handleClipboardChange val =
    case Decode.decodeValue Clipboard.Model.decodeContents val of
        Err _ ->
            Clipboard.Msg.NoOp

        Ok contents ->
            Clipboard.Msg.ClipboardChanged contents
