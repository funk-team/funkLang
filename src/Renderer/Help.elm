module Renderer.Help exposing (..)

{-| Helpers, mainly for retrieving content
-}

import ApiExplorer.Api.Param
import DesignSystem.IconBrowser
import Dict
import Dynamic.Data
import Interface
import Interface.Data
import Interface.Model
import List.Extra
import Model
import Model.Model
import ModelEditor.Model
import Spec.DataConnection
import Spec.Element.Id


{-| Return content and whether it can be edited
@@TODO: return record instead of tuple
-}
getContent :
    Spec.Element.Id.Id
    -> Model.Model.UserModel
    -> Interface.Model.ScopeData
    -> Maybe Interface.Data.CanvasRenderable
getContent id userModel scope =
    let
        connection : Maybe Spec.DataConnection.DataConnection
        connection =
            Spec.Element.Id.getFromDict id userModel.dataConnections
    in
    case connection of
        Nothing ->
            Nothing

        Just c ->
            retrieveContentForDataConnection c userModel scope


type alias InfoRequiredForRetrieval =
    { userModel : Model.Model.UserModel
    , target : Spec.DataConnection.ModelTarget
    , scope : Interface.Model.ScopeData
    }


retrieveContentForDataConnection :
    Spec.DataConnection.DataConnection
    -> Model.Model.UserModel
    -> Interface.Model.ScopeData
    -> Maybe Interface.Data.CanvasRenderable
retrieveContentForDataConnection connection userModel scope =
    case connection of
        Spec.DataConnection.Static staticContent ->
            { value = staticContent, inlineEditable = True }
                |> Just

        Spec.DataConnection.FromValidation modelField ->
            let
                maybeField =
                    Model.allApiParams userModel
                        |> List.Extra.find (\{ fieldKey } -> fieldKey == modelField)
            in
            case maybeField of
                Nothing ->
                    Nothing

                Just field ->
                    case ApiExplorer.Api.Param.validate field of
                        Ok _ ->
                            Nothing

                        Err e ->
                            { value = Interface.Data.ParagraphText e
                            , inlineEditable = False
                            }
                                |> Just

        Spec.DataConnection.FromInterface interfacePointer ->
            Interface.retrieveWithRefinedType
                interfacePointer
                scope
                userModel
                |> Maybe.map (\d -> { value = d, inlineEditable = False })

        Spec.DataConnection.FromModel target ->
            retrieveFromModel
                { userModel = userModel
                , target = target
                , scope = scope
                }

        Spec.DataConnection.Embed info ->
            Just
                { value = Interface.Data.YoutubeEmbed info
                , inlineEditable = False
                }

        Spec.DataConnection.Icon iconRef ->
            userModel.designSystem.iconBrowser
                |> DesignSystem.IconBrowser.getCorrespondingIcon iconRef
                |> Maybe.map Interface.Data.Icon
                |> Maybe.map (\d -> { value = d, inlineEditable = False })

        Spec.DataConnection.Media details ->
            Just
                { value = Interface.Data.Media details
                , inlineEditable = False
                }


retrieveFromModel : InfoRequiredForRetrieval -> Maybe Interface.Data.CanvasRenderable
retrieveFromModel { userModel, target, scope } =
    case target of
        Spec.DataConnection.PlainField fieldKey ->
            case Dict.get fieldKey userModel.runtimeModel of
                Just (Dynamic.Data.StringInstance str) ->
                    case str of
                        "" ->
                            Nothing

                        _ ->
                            Just { inlineEditable = False, value = Interface.Data.ParagraphText str }

                Just (Dynamic.Data.NumberInstance float) ->
                    Just { inlineEditable = False, value = Interface.Data.ParagraphText (String.fromFloat float) }

                _ ->
                    Nothing

        Spec.DataConnection.Association associationTarget ->
            let
                currentRuntimeModel =
                    userModel.runtimeModel
            in
            case ( Dict.get associationTarget.fieldKey currentRuntimeModel, Dict.get associationTarget.fieldKey userModel.modelEditor.fields ) of
                -- then we get the association that the user has selected as well as the value
                ( Just value, Just field ) ->
                    case Dict.get associationTarget.associationKey field.associations of
                        Just association ->
                            case ( value, association.projection ) of
                                -- we refine the value using the defined association
                                ( Dynamic.Data.UnionInstance type_ ( constructorId, _ ), ModelEditor.Model.Match matchers ) ->
                                    case Dict.get constructorId matchers of
                                        Just dataConnection ->
                                            retrieveContentForDataConnection
                                                dataConnection
                                                userModel
                                                scope

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
