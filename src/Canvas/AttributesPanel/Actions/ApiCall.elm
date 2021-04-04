module Canvas.AttributesPanel.Actions.ApiCall exposing (viewEditor)

import Action
import ApiExplorer.Api
import ApiExplorer.Model
import Dict
import Dict.Any
import Element
import Interface.JsonTree.Model
import Interface.Selection
import Model
import Model.Model
import ModelEditor.Model
import Ui.Dropdown


viewEditor : Model.Model.UserModel -> Action.ApiCallParams -> Element.Element Action.ApiCallParams
viewEditor userModel apiCallParams =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ callSelector userModel apiCallParams
            |> Element.map (\apiCallKey -> { apiCallParams | apiCallKey = Just apiCallKey })
        , parameterMapper userModel apiCallParams
            |> Element.map (\( parameterKey, modelKey ) -> { apiCallParams | modelToCallMap = Dict.Any.insert parameterKey modelKey apiCallParams.modelToCallMap })
        ]


{-| Allow the user to specify which API call to use
-}
callSelector : Model.Model.UserModel -> Action.ApiCallParams -> Element.Element ApiExplorer.Model.ApiCallKey
callSelector userModel apiCallParams =
    let
        label =
            case Model.getCall apiCallParams userModel of
                Nothing ->
                    "Select API"

                Just { name } ->
                    name

        rows =
            userModel.apiExplorer
                |> ApiExplorer.Model.listApiSpecs
                |> List.map viewApi

        viewApi ( key, { name } ) =
            Ui.Dropdown.viewRow
                { detail = "" |> Ui.Dropdown.Description
                , label = name |> Ui.Dropdown.Description
                , onSelect = key
                , sideNote = Ui.Dropdown.NoDetail
                , isSelected = Just key == apiCallParams.apiCallKey
                , rightHandText = Nothing
                }
    in
    Ui.Dropdown.view []
        { label = label
        , contents = rows
        }


parameterMapper :
    Model.Model.UserModel
    -> Action.ApiCallParams
    -> Element.Element ( Action.ApiCallSlotIdentifier, Int )
parameterMapper userModel apiCallParams =
    case Model.getCall apiCallParams userModel of
        Nothing ->
            Element.text "Select an API from above"

        Just source ->
            case source.kind of
                ApiExplorer.Model.Mock ->
                    Element.text "Mock sources don't support parameters"

                _ ->
                    source.requestBodyParams
                        |> Dict.Any.toList
                        |> List.concatMap (viewApiParam userModel apiCallParams source)
                        |> Element.column [ Element.width Element.fill ]


viewApiParam :
    Model.Model.UserModel
    -> Action.ApiCallParams
    -> ApiExplorer.Model.ApiSpec
    -> ( Interface.JsonTree.Model.KeyPath, Interface.Selection.SelectedEntrySettings )
    -> List (Element.Element ( Action.ApiCallSlotIdentifier, Int ))
viewApiParam userModel apiCallParams source slot =
    let
        ( keyPath, _ ) =
            slot

        -- view a field from the model that can be connected to a slot
        viewFieldAsDropdownRow : Maybe Int -> ( Int, ModelEditor.Model.Field ) -> Ui.Dropdown.Row Int
        viewFieldAsDropdownRow selectedFieldKey ( fieldKey, field ) =
            Ui.Dropdown.viewRow
                { isSelected = Just fieldKey == selectedFieldKey
                , label = field.name |> Ui.Dropdown.Description
                , onSelect = fieldKey
                , detail = field.comment |> Ui.Dropdown.Description
                , sideNote = Ui.Dropdown.NoDetail
                , rightHandText = Nothing
                }

        connectables : Maybe Int -> List (Ui.Dropdown.Row Int)
        connectables modelField =
            userModel.modelEditor.fields
                |> Dict.toList
                |> List.map (viewFieldAsDropdownRow modelField)

        viewSlot :
            { slotName : String
            , variableKey : Maybe Interface.Selection.InterpolationVariableKey
            , refinedType : Maybe Interface.Selection.RefinedType
            }
            -> Element.Element ( Action.ApiCallSlotIdentifier, Int )
        viewSlot { slotName, variableKey, refinedType } =
            let
                callParamIdentifier =
                    ( keyPath, variableKey )

                selectedFieldKey =
                    Dict.Any.get callParamIdentifier apiCallParams.modelToCallMap

                -- for the param we are looking at, find out if it was bound in the mapping
                -- display the name of the selected model field
                modelFieldLabel =
                    case selectedFieldKey of
                        Nothing ->
                            "Select model field"

                        Just f ->
                            case Dict.get f userModel.modelEditor.fields of
                                Nothing ->
                                    "Select model field"

                                Just field ->
                                    field.name

                icon =
                    refinedType
                        |> Maybe.map Interface.Selection.refinedTypeToIcon
                        |> Maybe.map (Element.el [ Element.scale 0.8, Element.alpha 0.7 ])
                        |> Maybe.withDefault Element.none

                dropdownForSlot =
                    Ui.Dropdown.view
                        []
                        { contents = connectables selectedFieldKey
                        , label = modelFieldLabel
                        }
            in
            Element.row
                [ Element.spacing 5 ]
                [ icon
                , Element.text slotName
                , dropdownForSlot
                ]
                |> Element.map (Tuple.pair callParamIdentifier)
    in
    findSlots source slot
        |> List.map viewSlot


findSlots :
    ApiExplorer.Model.ApiSpec
    -> ( Interface.JsonTree.Model.KeyPath, Interface.Selection.SelectedEntrySettings )
    ->
        List
            { refinedType : Maybe Interface.Selection.RefinedType
            , slotName : String
            , variableKey : Maybe Interface.Selection.InterpolationVariableKey
            }
findSlots source ( keyPath, { name, kind } ) =
    case kind of
        Interface.Selection.Single (Just (Interface.Selection.Text textSlots)) ->
            case ApiExplorer.Api.findAllSlots source keyPath textSlots of
                [] ->
                    [ { slotName = name, refinedType = Just (Interface.Selection.Text textSlots), variableKey = Nothing } ]

                some ->
                    some
                        |> List.map
                            (\( variableKey, variableSettings ) ->
                                { slotName =
                                    name ++ " {{" ++ variableSettings.variableName ++ "}}"
                                , variableKey = variableKey
                                , refinedType = Just variableSettings.kind
                                }
                            )

        Interface.Selection.Single refinedType ->
            [ { slotName = name, refinedType = refinedType, variableKey = Nothing } ]

        _ ->
            []
