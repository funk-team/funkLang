module Spec.Model exposing (..)

import Action
import ApiExplorer.Model
import CodeEditor.Model
import DeployEditor
import DesignSystem
import Json.Decode as Decode
import Json.Decode.Extra as Extra
import Json.Encode as Encode
import ModelEditor.Model
import Rectangle
import Spec.DataConnection
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Style


type alias Rectangles =
    Spec.Element.Id.Dict Rectangle.Rectangle


{-| We have a flat model and this flat model includes the spec.
But not all of the model is persisted.
-}
type alias WithSpec model =
    { model
        | itemsOnCanvas : List Spec.Element.Model.Screen
        , apiExplorer : ApiExplorer.Model.Model
        , dataConnections : DataConnections

        -- , customCode : CustomHtmls
        , elementStyles : ElementStyles
        , actions : Action.Actions
        , designSystem : DesignSystem.Model
        , deployEditor : DeployEditor.Model
        , modelEditor : ModelEditor.Model.Model
        , codeEditor : CodeEditor.Model.Model
    }


{-| applies to local user model, but could theoretically also be reconciled with remote models
-}



-- [generator-start]
-- The whole thing that a user can modify in the funk editor minus large assets like images


type alias Spec =
    { itemsOnCanvas : List Spec.Element.Model.Screen
    , elementStyles : ElementStyles
    , dataConnections : DataConnections
    , actions : Action.Actions

    -- RENAME
    , apiExplorer : ApiExplorer.Model.Model
    , designSystem : DesignSystem.Model
    , deployEditor : DeployEditor.Model
    , modelEditor : ModelEditor.Model.Model
    , codeEditor : CodeEditor.Model.Model
    }


type alias ElementStyles =
    Spec.Element.Id.Dict Spec.Element.Style.Style


type alias DataConnections =
    Spec.Element.Id.Dict Spec.DataConnection.DataConnection



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeDataConnections =
    Spec.Element.Id.decodeDict Spec.DataConnection.decodeDataConnection


decodeElementStyles =
    Spec.Element.Id.decodeDict Spec.Element.Style.decodeStyle


decodeSpec =
    Decode.succeed
        Spec
        |> Extra.andMap (Decode.field "itemsOnCanvas" (Decode.list Spec.Element.Model.decodeScreen))
        |> Extra.andMap (Decode.field "elementStyles" decodeElementStyles)
        |> Extra.andMap (Decode.field "dataConnections" decodeDataConnections)
        |> Extra.andMap (Decode.field "actions" Action.decodeActions)
        |> Extra.andMap (Decode.field "apiExplorer" ApiExplorer.Model.decodeModel)
        |> Extra.andMap (Decode.field "designSystem" DesignSystem.decodeModel)
        |> Extra.andMap (Decode.field "deployEditor" DeployEditor.decodeModel)
        |> Extra.andMap (Decode.field "modelEditor" ModelEditor.Model.decodeModel)
        |> Extra.andMap (Decode.field "codeEditor" CodeEditor.Model.decodeModel)


encodeDataConnections a =
    Spec.Element.Id.encodeDict Spec.DataConnection.encodeDataConnection a


encodeElementStyles a =
    Spec.Element.Id.encodeDict Spec.Element.Style.encodeStyle a


encodeSpec a =
    Encode.object
        [ ( "itemsOnCanvas", Encode.list Spec.Element.Model.encodeScreen a.itemsOnCanvas )
        , ( "elementStyles", encodeElementStyles a.elementStyles )
        , ( "dataConnections", encodeDataConnections a.dataConnections )
        , ( "actions", Action.encodeActions a.actions )
        , ( "apiExplorer", ApiExplorer.Model.encodeModel a.apiExplorer )
        , ( "designSystem", DesignSystem.encodeModel a.designSystem )
        , ( "deployEditor", DeployEditor.encodeModel a.deployEditor )
        , ( "modelEditor", ModelEditor.Model.encodeModel a.modelEditor )
        , ( "codeEditor", CodeEditor.Model.encodeModel a.codeEditor )
        ]



-- [generator-end]
