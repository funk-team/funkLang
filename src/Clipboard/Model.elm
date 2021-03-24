module Clipboard.Model exposing (..)

import Action
import Json.Decode as Decode
import Json.Encode as Encode
import Spec.DataConnection
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Style


init : Decode.Value -> Model
init val =
    case Decode.decodeValue (Decode.field "funk_clipboard" Decode.string |> Decode.andThen decodeContentsValue) val of
        Err _ ->
            Nothing

        Ok contents ->
            Just contents


decodeContentsValue : String -> Decode.Decoder Contents
decodeContentsValue str =
    case Decode.decodeString decodeContents str of
        Err _ ->
            Decode.fail "Err"

        Ok contents ->
            Decode.succeed contents


type alias Model =
    Maybe Contents



-- [generator-start]


type alias Contents =
    { element : Spec.Element.Model.AbsoluteElement
    , originalParentId : Maybe Spec.Element.Id.Id
    , styles : List ( Spec.Element.Id.Id, Spec.Element.Style.Style )
    , actions : List ( Spec.Element.Id.Id, Action.ActionsForElement )
    , dataConnections : List ( Spec.Element.Id.Id, Spec.DataConnection.DataConnection )
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeContents =
    Decode.map5
        Contents
        (Decode.field "element" Spec.Element.Model.decodeAbsoluteElement)
        (Decode.field "originalParentId" (Decode.maybe Spec.Element.Id.decodeId))
        (Decode.field "styles" (Decode.list decodeTuple_Spec_Element_Id_Id_Spec_Element_Style_Style_))
        (Decode.field "actions" (Decode.list decodeTuple_Spec_Element_Id_Id_Action_ActionsForElement_))
        (Decode.field "dataConnections" (Decode.list decodeTuple_Spec_Element_Id_Id_Spec_DataConnection_DataConnection_))


decodeTuple_Spec_Element_Id_Id_Action_ActionsForElement_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" Action.decodeActionsForElement)


decodeTuple_Spec_Element_Id_Id_Spec_DataConnection_DataConnection_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" Spec.DataConnection.decodeDataConnection)


decodeTuple_Spec_Element_Id_Id_Spec_Element_Style_Style_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" Spec.Element.Style.decodeStyle)


encodeContents a =
    Encode.object
        [ ( "element", Spec.Element.Model.encodeAbsoluteElement a.element )
        , ( "originalParentId", encodeMaybeSpec_Element_Id_Id a.originalParentId )
        , ( "styles", Encode.list encodeTuple_Spec_Element_Id_Id_Spec_Element_Style_Style_ a.styles )
        , ( "actions", Encode.list encodeTuple_Spec_Element_Id_Id_Action_ActionsForElement_ a.actions )
        , ( "dataConnections", Encode.list encodeTuple_Spec_Element_Id_Id_Spec_DataConnection_DataConnection_ a.dataConnections )
        ]


encodeMaybeSpec_Element_Id_Id a =
    case a of
        Just b ->
            Spec.Element.Id.encodeId b

        Nothing ->
            Encode.null


encodeTuple_Spec_Element_Id_Id_Action_ActionsForElement_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", Action.encodeActionsForElement a2 )
        ]


encodeTuple_Spec_Element_Id_Id_Spec_DataConnection_DataConnection_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", Spec.DataConnection.encodeDataConnection a2 )
        ]


encodeTuple_Spec_Element_Id_Id_Spec_Element_Style_Style_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", Spec.Element.Style.encodeStyle a2 )
        ]



-- [generator-end]
