module Spec.DataConnection.Help exposing (..)

import Dict
import Dynamic.Data
import Element
import Interface.Data
import Interface.Selection
import Interface.Ui
import Model
import Model.Model
import ModelEditor.Model
import Spec.DataConnection exposing (..)
import Ui.Boxicons
import Ui.Component


iconForModelField : Model.Model.UserModel -> ( Int, ModelEditor.Model.Field ) -> Element.Element msg
iconForModelField userModel field =
    let
        ( fieldKey, { kind } ) =
            field

        param =
            Model.paramForField fieldKey userModel

        kind_ : Maybe Dynamic.Data.Kind
        kind_ =
            case kind of
                ModelEditor.Model.Typed t ->
                    Just t

                ModelEditor.Model.WithDefault def ->
                    Just <| Dynamic.Data.toKind def

                ModelEditor.Model.Unspecified ->
                    Nothing

        icon =
            case kind_ of
                Just Dynamic.Data.StringKind ->
                    case param of
                        Just { refinedType } ->
                            Interface.Selection.refinedTypeToIcon refinedType

                        Nothing ->
                            Interface.Selection.refinedTypeToIcon
                                (Interface.Selection.Text Interface.Selection.emptyTemplateRefinements)

                Just Dynamic.Data.NumberKind ->
                    Ui.Component.icon
                        Ui.Boxicons.bxsOffer

                Just (Dynamic.Data.UnionKind _) ->
                    Ui.Component.icon
                        Ui.Boxicons.bxsCategory

                _ ->
                    questionMarkIcon
    in
    icon


toLabel : DataConnection -> String
toLabel conn =
    case conn of
        FromModel _ ->
            "FromModel"

        FromValidation _ ->
            "FromValidation"

        Static value ->
            Interface.Data.refinedValueToLabel value

        --         StaticList _ ->
        --             "StaticList"
        Icon _ ->
            "Icon"

        Embed _ ->
            "Embed"

        Media _ ->
            "Media"

        FromInterface interfacePointer ->
            Interface.Ui.pointerToLabel interfacePointer


questionMarkIcon =
    Ui.Component.icon Ui.Boxicons.bxQuestionMark


toIcon : Model.Model.UserModel -> DataConnection -> Element.Element msg
toIcon userModel conn =
    case conn of
        FromInterface interfacePointer ->
            Interface.Ui.pointerToIcon interfacePointer

        FromModel (PlainField fieldKey) ->
            Dict.get fieldKey userModel.modelEditor.fields
                |> Maybe.map (Tuple.pair fieldKey >> iconForModelField userModel)
                |> Maybe.withDefault questionMarkIcon

        FromModel (Association { fieldKey, associationKey }) ->
            Dict.get fieldKey userModel.modelEditor.fields
                |> Maybe.map (Tuple.pair fieldKey >> iconForModelField userModel)
                |> Maybe.withDefault questionMarkIcon

        FromValidation _ ->
            Ui.Component.icon <| Ui.Boxicons.bxText

        Static value ->
            Interface.Data.refinedValueToIcon value

        --        StaticList _ ->
        --            Ui.Component.icon <| Ui.Boxicons.bxListUl
        Icon _ ->
            Ui.Component.icon <| Ui.Boxicons.bxImageAlt

        Embed _ ->
            Ui.Component.icon <| Ui.Boxicons.bxlYoutube

        Media _ ->
            Ui.Component.icon <| Ui.Boxicons.bxFileBlank
