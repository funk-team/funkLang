module Ui.LinkableInput exposing (view, viewNullable)

import ApiExplorer.Model
import Canvas.Selection
import CodeEditor.Model
import Dict.Any
import Element
import Html
import Html.Attributes
import Html.Events
import Interface
import Interface.JsonTree.Model
import Interface.Model
import Interface.Scope
import Interface.Selection
import Json.Decode as Decode
import Keyboard
import Spec
import Spec.Element.Layout.Length
import Spec.Model
import Ui.Dropdown
import Ui.Scrubbable


{-| Render a single value that can either be retreived from an interface or
input statically by the user
-}
viewNullable :
    Spec.Model.WithSpec userModel
    -> Canvas.Selection.SelectionItem
    -> { label : String, value : Spec.Element.Layout.Length.NullableLength }
    -> Element.Element Spec.Element.Layout.Length.NullableLength
viewNullable userModel selectionItem { label, value } =
    let
        onScrub val =
            { value
                | current = Spec.Element.Layout.Length.UserInput (String.fromFloat val)
                , last = Just val
            }

        -- build the scope manually
        -- find all available scopes for parent elements
        -- retrieve the data for them
        scope =
            Interface.Scope.populateForElement selectionItem userModel

        onInput newInput =
            { value
                | current = Spec.Element.Layout.Length.UserInput newInput
            }

        currentValue =
            Spec.resolveNullable userModel scope value

        {-
           Interaction copied from figma:
           Try to parse the value given by the user
        -}
        onBlur : String -> Spec.Element.Layout.Length.NullableLength
        onBlur input =
            case String.toFloat input of
                -- if it does not parse, reset the input value
                Nothing ->
                    { value
                        | current =
                            Spec.Element.Layout.Length.UserInput "-"
                        , last = Nothing
                    }

                Just parsed ->
                    -- if it does parse, check if it is the same as before. if it is the same, keep data connection if it exists
                    if Just parsed == currentValue then
                        value
                        -- if not, update the value

                    else
                        { value
                            | last = Just parsed
                        }

        stringValue =
            Spec.getStringValueNullable userModel scope value

        params : Params userModel Spec.Element.Layout.Length.NullableLength
        params =
            { placeholder =
                case value.current of
                    Spec.Element.Layout.Length.UserInput input ->
                        case String.toFloat input of
                            Nothing ->
                                "-"

                            Just f ->
                                input

                    Spec.Element.Layout.Length.Linked _ ->
                        case value.last of
                            Nothing ->
                                "-"

                            Just some ->
                                String.fromFloat some
            , onBlur = onBlur
            , onScrub = onScrub
            , onInput = onInput
            , label = label
            , value = stringValue
            , userModel = userModel
            , selectionItem = selectionItem
            , current = value.current
            , onLink = \link -> { value | current = link }
            , labelOnLeft = True
            }
    in
    params
        |> help


{-| Render a single value that can either be retreived from an interface or
input statically by the user
-}
view :
    Spec.Model.WithSpec userModel
    -> Canvas.Selection.SelectionItem
    -> { label : String, value : Spec.Element.Layout.Length.Length }
    -> Bool
    -> Element.Element Spec.Element.Layout.Length.Length
view userModel selectionItem { label, value } labelLeft =
    let
        onScrub val =
            { value
                | current = Spec.Element.Layout.Length.UserInput (String.fromFloat val)
                , last = val
            }

        -- build the scope manually
        -- find all available scopes for parent elements
        -- retrieve the data for them
        scope =
            Interface.Scope.populateForElement selectionItem userModel

        onInput newInput =
            { value
                | current = Spec.Element.Layout.Length.UserInput newInput
            }

        currentValue =
            Spec.resolve userModel scope value

        {-
           Interaction copied from figma:
           Try to parse the value given by the user
        -}
        onBlur : String -> Spec.Element.Layout.Length.Length
        onBlur input =
            case String.toFloat input of
                -- if it does not parse, reset the input value
                Nothing ->
                    { value
                        | current =
                            Spec.Element.Layout.Length.UserInput (String.fromFloat value.last)
                    }

                Just parsed ->
                    -- if it does parse, check if it is the same as before. if it is the same, keep data connection if it exists
                    if parsed == currentValue then
                        value
                        -- if not, update the value

                    else
                        { value
                            | last = parsed
                        }

        stringValue =
            Spec.getStringValue userModel scope value

        params : Params userModel Spec.Element.Layout.Length.Length
        params =
            { placeholder = String.fromFloat value.last
            , onBlur = onBlur
            , onScrub = onScrub
            , onInput = onInput
            , label = label
            , value = stringValue
            , userModel = userModel
            , selectionItem = selectionItem
            , labelOnLeft = labelLeft
            , current = value.current
            , onLink = \link -> { value | current = link }
            }
    in
    help params


{-| Show what can be connected to a length input as px
-}
connectionSelector :
    Spec.Model.WithSpec userModel
    -> Canvas.Selection.SelectionItem
    -> Spec.Element.Layout.Length.LengthInput
    -> Element.Attribute Spec.Element.Layout.Length.LengthInput
connectionSelector userModel selectionItem value =
    let
        localOptions =
            Interface.Scope.findDataScopesForSelection
                userModel
                selectionItem

        bakeRowsFromLocalOption : Interface.Model.ListScopePointer -> List (Ui.Dropdown.Row Interface.Model.InterfacePointer)
        bakeRowsFromLocalOption contextInfo =
            let
                isSelected =
                    False

                row : ( Interface.JsonTree.Model.KeyPath, Interface.Selection.SelectedEntrySettings ) -> Ui.Dropdown.Row Interface.Model.InterfacePointer
                row ( path, selection ) =
                    let
                        pointer : Interface.Model.InterfacePointer
                        pointer =
                            { interfaceId = contextInfo.interfaceId
                            , kind =
                                { subPath = path
                                , scopeId = ( contextInfo.interfaceId, contextInfo.pointerToListInInterface.rootPath )
                                }
                                    |> Interface.Model.FromListScope
                                    |> Just
                            }
                    in
                    Ui.Dropdown.viewRow
                        { isSelected = isSelected
                        , onSelect = pointer
                        , rightHandText = Nothing
                        , detail = Ui.Dropdown.NoDetail
                        , label = Ui.Dropdown.Description (contextInfo.name ++ " - " ++ selection.name)
                        , sideNote = Ui.Dropdown.Description "LIST"
                        }
            in
            contextInfo.selectionForEachItemInListScope
                |> Dict.Any.toList
                |> List.map row

        rowsFromTransformation :
            ( CodeEditor.Model.TransformationKey, CodeEditor.Model.Transformation )
            -> List (Ui.Dropdown.Row Interface.Model.InterfacePointer)
        rowsFromTransformation ( transformationKey, transformation ) =
            transformation.outputSelection
                |> Dict.Any.toList
                |> List.filterMap (bakeRow transformation.name (Interface.wrapTransformationKey transformationKey))

        rowsFromApi :
            ( ApiExplorer.Model.ApiCallKey, ApiExplorer.Model.ApiSpec )
            -> List (Ui.Dropdown.Row Interface.Model.InterfacePointer)
        rowsFromApi ( apiCallKey, apiCall ) =
            apiCall.responseDataSelection
                |> Dict.Any.toList
                |> List.filterMap (bakeRow apiCall.name (Interface.wrapApiCallKey apiCallKey))

        rowsFromScope : List (Ui.Dropdown.Row Interface.Model.InterfacePointer)
        rowsFromScope =
            localOptions
                |> List.concatMap bakeRowsFromLocalOption

        bakeRow :
            String
            -> Interface.Model.InterfaceKey
            -> ( Interface.JsonTree.Model.KeyPath, Interface.Selection.SelectedEntrySettings )
            -> Maybe (Ui.Dropdown.Row Interface.Model.InterfacePointer)
        bakeRow transformationName transformationKey ( selectionKey, selectedEntry ) =
            case selectedEntry.kind of
                Interface.Selection.List _ ->
                    Nothing

                Interface.Selection.Single (Just Interface.Selection.Int) ->
                    let
                        pointer : Interface.Model.InterfacePointer
                        pointer =
                            { interfaceId = transformationKey
                            , kind =
                                Interface.Model.PointerFromRoot selectionKey
                                    |> Interface.Model.RelativeToInterfaceRoot
                                    |> Just
                            }

                        isSelected =
                            case value of
                                Spec.Element.Layout.Length.UserInput _ ->
                                    False

                                Spec.Element.Layout.Length.Linked currentPointer ->
                                    pointer == currentPointer
                    in
                    Ui.Dropdown.viewRow
                        { isSelected = isSelected
                        , onSelect = pointer
                        , rightHandText = Nothing
                        , detail = Ui.Dropdown.NoDetail
                        , label = Ui.Dropdown.Description (transformationName ++ " - " ++ selectedEntry.name)
                        , sideNote = Ui.Dropdown.NoDetail
                        }
                        |> Just

                Interface.Selection.Single _ ->
                    Nothing

        transformationRows =
            userModel.codeEditor
                |> CodeEditor.Model.listCodes
                |> List.concatMap rowsFromTransformation

        apiRows =
            userModel.apiExplorer
                |> ApiExplorer.Model.listApiSpecs
                |> List.concatMap rowsFromApi

        rows =
            apiRows ++ transformationRows ++ rowsFromScope
    in
    Ui.Dropdown.viewAsLink rows
        |> Element.map Spec.Element.Layout.Length.Linked
        |> Element.el [ Element.alignRight ]
        |> Element.inFront


type alias Params userModel length =
    { current : Spec.Element.Layout.Length.LengthInput
    , label : String
    , onBlur : String -> length
    , onScrub : Float -> length
    , onInput :
        String
        -> length
    , onLink :
        Spec.Element.Layout.Length.LengthInput
        -> length
    , placeholder : String
    , selectionItem : Canvas.Selection.SelectionItem
    , userModel : Spec.Model.WithSpec userModel
    , labelOnLeft : Bool
    , value : String
    }


help : Params userModel length -> Element.Element length
help { userModel, selectionItem, label, onInput, onBlur, value, placeholder, current, onLink, onScrub, labelOnLeft } =
    let
        inputElement : Element.Element length
        inputElement =
            let
                isLinked =
                    case current of
                        Spec.Element.Layout.Length.Linked interfacePointer ->
                            True

                        Spec.Element.Layout.Length.UserInput string ->
                            False

                onBlurProcess : Decode.Decoder length
                onBlurProcess =
                    Decode.at [ "target", "value" ] Decode.string
                        |> Decode.map onBlur

                onEnterProcess : Decode.Decoder length
                onEnterProcess =
                    Decode.map2 Tuple.pair
                        Keyboard.eventKeyDecoder
                        (Decode.at [ "target", "value" ] Decode.string)
                        |> Decode.andThen
                            (\( key, newValue ) ->
                                case Keyboard.whitespaceKey key of
                                    Just Keyboard.Enter ->
                                        Decode.succeed newValue

                                    _ ->
                                        Decode.fail "The pressed key was not the enter key"
                            )
                        |> Decode.map onBlur
            in
            Html.input
                [ Html.Events.on "blur" onBlurProcess
                , Html.Events.on "keydown" onEnterProcess
                , Html.Events.onInput onInput
                , Html.Attributes.class "funk-input"
                , Html.Attributes.class
                    (if isLinked then
                        "linked"

                     else
                        ""
                    )
                , Html.Attributes.placeholder placeholder
                , Html.Attributes.value value
                , Html.Attributes.style "min-width" "100%"
                , Html.Attributes.style "box-sizing" "border-box"
                , Html.Attributes.style "width" "0"
                ]
                []
                |> Element.html
                |> Element.el
                    [ connectionSelector userModel selectionItem current
                        |> Element.mapAttribute onLink
                    , Element.width Element.fill
                    ]
    in
    (if labelOnLeft then
        Element.row

     else
        Element.column
    )
        [ Element.spacing 5, Element.width Element.fill ]
        [ Ui.Scrubbable.text label (String.toInt value |> Maybe.withDefault 0) |> Element.map (max 0 >> toFloat >> onScrub)
        , inputElement
        ]
