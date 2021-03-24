module Interface exposing (..)

{-| An interface is anything that some part of a project can be connected to

This module contains methods to work with interfaces

-}

import ApiExplorer.Model
import CodeEditor.Model
import Dict.Any
import Interface.Data
import Interface.Model
import Json.Decode as Decode
import RemoteData
import Spec.Model


wrapTransformationKey (CodeEditor.Model.TransformationKey k) =
    Interface.Model.TransformationKey k


wrapApiCallKey (ApiExplorer.Model.ApiCallKey k) =
    Interface.Model.ApiCallKey k



-- GET AND LIST


list : Spec.Model.WithSpec userModel -> List Interface.Model.GenericInterface
list { apiExplorer, codeEditor } =
    let
        transformations =
            codeEditor
                |> CodeEditor.Model.listCodes
                |> List.map (Tuple.mapFirst (\(CodeEditor.Model.TransformationKey key) -> Interface.Model.TransformationKey key))
                |> List.map makeTransformationGeneric

        apiCalls : List Interface.Model.GenericInterface
        apiCalls =
            apiExplorer
                |> ApiExplorer.Model.listApiSpecs
                |> List.map (Tuple.mapFirst (\(ApiExplorer.Model.ApiCallKey key) -> Interface.Model.ApiCallKey key))
                |> List.map makeApiCallGeneric
    in
    transformations ++ apiCalls



-- HELP


makeApiCallGeneric : ( Interface.Model.InterfaceKey, ApiExplorer.Model.ApiSpec ) -> Interface.Model.GenericInterface
makeApiCallGeneric ( key, apiCallSpec ) =
    { data = RemoteData.toMaybe apiCallSpec.request
    , id = key
    , outputSelection = apiCallSpec.responseDataSelection
    , name = apiCallSpec.name
    }


makeTransformationGeneric : ( Interface.Model.InterfaceKey, CodeEditor.Model.Transformation ) -> Interface.Model.GenericInterface
makeTransformationGeneric ( key, transformation ) =
    { data = transformation.executionState |> Maybe.andThen (.return >> Result.toMaybe)
    , outputSelection = transformation.outputSelection
    , id = key
    , name = transformation.name
    }


{-| Retrieve the interface
-}
getSource : Interface.Model.InterfaceKey -> Spec.Model.WithSpec userModel -> Maybe Interface.Model.GenericInterface
getSource interfaceId { apiExplorer, codeEditor } =
    case interfaceId of
        Interface.Model.TransformationKey key ->
            CodeEditor.Model.getItem (CodeEditor.Model.TransformationKey key) codeEditor
                |> Maybe.map (Tuple.pair interfaceId >> makeTransformationGeneric)

        Interface.Model.ApiCallKey key ->
            ApiExplorer.Model.getApiSpec (ApiExplorer.Model.ApiCallKey key) apiExplorer
                |> Maybe.map (Tuple.pair interfaceId >> makeApiCallGeneric)


{-| Retrieve data from available interfaces

-- TODO: spit out error / propagate data requirements a là redux suspense

-}
retrieveWithRefinedType :
    Interface.Model.InterfacePointer
    -> Interface.Model.ScopeData
    -> Spec.Model.WithSpec userModel
    -> Maybe Interface.Data.RefinedValue
retrieveWithRefinedType interfacePointer scopeData model =
    let
        apiPrimitiveCase jsonPath =
            case getSource interfacePointer.interfaceId model of
                Nothing ->
                    Nothing

                Just interface ->
                    let
                        dataFromStore =
                            Maybe.andThen (Interface.Data.getFromValue jsonPath) interface.data

                        settingsFromDataExplorer =
                            Dict.Any.get jsonPath interface.outputSelection
                    in
                    case ( dataFromStore, settingsFromDataExplorer ) of
                        ( Just value, Just settings ) ->
                            Interface.Data.refineValue
                                interfacePointer.interfaceId
                                jsonPath
                                settings
                                value

                        _ ->
                            Nothing

        getDataFromListContext subPath scopeId =
            Dict.Any.get scopeId scopeData
                |> Maybe.andThen (Interface.Data.retrieveWithRefinedTypeFromContext scopeId subPath)
    in
    interfacePointer.kind
        |> Maybe.andThen
            (\selectionKind ->
                case selectionKind of
                    Interface.Model.RelativeToInterfaceRoot { rootPath } ->
                        apiPrimitiveCase rootPath

                    Interface.Model.FromListScope { subPath, scopeId } ->
                        getDataFromListContext subPath scopeId
            )


{-| This will not work with lists because it is only meant for direct values between interfaces
-}
retrieveValue :
    { interfacePointer : Interface.Model.InterfacePointer
    , model : Spec.Model.WithSpec userModel
    , scope : Interface.Model.ScopeData
    }
    -> Maybe Decode.Value
retrieveValue { interfacePointer, model, scope } =
    case getSource interfacePointer.interfaceId model of
        Nothing ->
            Nothing

        Just source ->
            case interfacePointer.kind of
                Nothing ->
                    Nothing

                Just kind ->
                    case kind of
                        Interface.Model.RelativeToInterfaceRoot { rootPath } ->
                            case source.data of
                                Nothing ->
                                    Nothing

                                Just value ->
                                    Interface.Data.getFromValue
                                        rootPath
                                        value

                        Interface.Model.FromListScope { subPath, scopeId } ->
                            Dict.Any.get scopeId scope
                                |> Maybe.andThen (\scopeEntry -> Interface.Data.getFromValue subPath scopeEntry.data)
