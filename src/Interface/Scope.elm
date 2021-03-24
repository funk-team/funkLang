module Interface.Scope exposing (..)

{-| When an element is mapped over a list, it provides a scope to its children.
The scope contains the data of a list item

This module can later be extended to provide other scopes like login

-}

import Canvas.Selection
import Dict.Any
import Interface
import Interface.Data
import Interface.Model
import Interface.Selection
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Spec.DataConnection
import Spec.Element.Id
import Spec.Model


empty : Interface.Model.ScopeData
empty =
    Dict.Any.empty (Encode.encode 2 << Interface.Model.encodeScopeId)


add : Interface.Data.ListData -> Decode.Value -> Interface.Model.ScopeData -> Interface.Model.ScopeData
add apiListData value =
    Dict.Any.insert
        apiListData.scopeId
        { data = value
        , selectionForEachItemInListScope = apiListData.selections
        }


populateForElement : Canvas.Selection.SelectionItem -> Spec.Model.WithSpec a -> Interface.Model.ScopeData
populateForElement selectionItem userModel =
    let
        scopeReducer : Canvas.Selection.IdWithIndex -> Interface.Model.ScopeData -> Interface.Model.ScopeData
        scopeReducer { id, index } scope =
            case index of
                Nothing ->
                    scope

                Just index_ ->
                    case Spec.Element.Id.getFromDict id userModel.dataConnections of
                        Nothing ->
                            scope

                        Just connection ->
                            case connection of
                                Spec.DataConnection.FromInterface listPointer ->
                                    case Interface.retrieveWithRefinedType listPointer scope userModel of
                                        Nothing ->
                                            scope

                                        Just (Interface.Data.InterfaceDataList listData) ->
                                            case List.Extra.getAt index_ listData.values of
                                                Nothing ->
                                                    scope

                                                Just value ->
                                                    add
                                                        listData
                                                        value
                                                        scope

                                        _ ->
                                            scope

                                _ ->
                                    scope

        path_ : List Canvas.Selection.IdWithIndex
        path_ =
            case selectionItem of
                Canvas.Selection.SelectedRoot _ ->
                    []

                Canvas.Selection.SelectedShallow _ id ->
                    [ id ]

                Canvas.Selection.SelectedDeeper _ { path, parent, target } ->
                    path ++ [ parent, target ]

        populatedScope =
            List.foldl scopeReducer empty path_
    in
    populatedScope


{-| given an element ID find the list that has been connected
-}
getConnectedList :
    Canvas.Selection.SelectionItem
    -> Spec.Model.WithSpec a
    -> Spec.Element.Id.Id
    -> Maybe Interface.Model.ListScopePointer
getConnectedList selectionItem userModel elId =
    let
        connection =
            Spec.Element.Id.getFromDict
                elId
                userModel.dataConnections
    in
    -- find out if an element was connected
    case connection of
        Nothing ->
            Nothing

        -- if it was connected, was it connected to an interface?
        Just (Spec.DataConnection.FromInterface aConnection) ->
            case getConnectedSelection selectionItem userModel aConnection of
                Nothing ->
                    Nothing

                Just selectionSettings ->
                    case ( selectionSettings.kind, aConnection.kind ) of
                        -- was it connected to the list inside an API?
                        -- we need the JSON path to put things into scope
                        ( Interface.Selection.List selectionForEachItemInListScope, Just (Interface.Model.RelativeToInterfaceRoot pointerToListInInterface) ) ->
                            Just
                                { interfaceId = aConnection.interfaceId
                                , name = selectionSettings.name
                                , selectionForEachItemInListScope = selectionForEachItemInListScope
                                , pointerToListInInterface = pointerToListInInterface
                                }

                        _ ->
                            Nothing

        Just _ ->
            Nothing


getConnectedSelection :
    Canvas.Selection.SelectionItem
    -> Spec.Model.WithSpec a
    -> Interface.Model.InterfacePointer
    -> Maybe Interface.Selection.SelectedEntrySettings
getConnectedSelection selectionItem userModel interfacePointer =
    case Interface.getSource interfacePointer.interfaceId userModel of
        Just apiSource ->
            case interfacePointer.kind of
                Just (Interface.Model.RelativeToInterfaceRoot { rootPath }) ->
                    Dict.Any.get
                        rootPath
                        apiSource.outputSelection

                -- if we are connected to a list scope
                Just (Interface.Model.FromListScope { scopeId, subPath }) ->
                    let
                        -- try to find the right list scope
                        foundScope =
                            List.Extra.find
                                (\ctxInfo -> ( ctxInfo.interfaceId, ctxInfo.pointerToListInInterface.rootPath ) == scopeId)
                                (findDataScopesForSelection userModel selectionItem)
                    in
                    case foundScope of
                        Nothing ->
                            Nothing

                        -- and if found, get the selection item the pointer is pointing to
                        Just { selectionForEachItemInListScope } ->
                            Dict.Any.get subPath selectionForEachItemInListScope

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| Find data made available by the scope an element is living within

TODO: introduce check for whether it was called from the findDataScope

-}
findDataScopesForSelection : Spec.Model.WithSpec a -> Canvas.Selection.SelectionItem -> List Interface.Model.ListScopePointer
findDataScopesForSelection userModel selectionItem =
    case Canvas.Selection.toTargetSelection selectionItem of
        Canvas.Selection.TargetScreen _ ->
            []

        -- if we have a deeper selection we can extract a scope from the ancestry element binding
        Canvas.Selection.TargetElement deeperSelection ->
            let
                options : List Interface.Model.ListScopePointer
                options =
                    List.filterMap
                        (getConnectedList selectionItem userModel)
                        (deeperSelection.path ++ [ deeperSelection.parent ])
            in
            options
