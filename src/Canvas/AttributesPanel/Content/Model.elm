module Canvas.AttributesPanel.Content.Model exposing (..)

-- externals
-- internals

import ApiExplorer.Api
import ApiExplorer.Api.Param
import Canvas.AttributesPanel.Shared
import Dict
import Dynamic.Data
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Interface.Selection
import Model
import Model.Model
import ModelEditor.Model
import Spec.DataConnection
import Spec.DataConnection.Help
import Ui.Style


viewSelectable :
    Maybe Spec.DataConnection.DataConnection
    -> { text : String, value : Spec.DataConnection.DataConnection, icon : Element.Element (Maybe Spec.DataConnection.DataConnection) }
    -> Element.Element (Maybe Spec.DataConnection.DataConnection)
viewSelectable currentConnection { text, value, icon } =
    let
        isSelected =
            currentConnection == Just value

        onPress : Maybe Spec.DataConnection.DataConnection
        onPress =
            if isSelected then
                Nothing

            else
                Just value

        label =
            Element.row
                [ Element.paddingXY 5 4
                , Element.spacing 2
                , Element.width Element.fill
                ]
                [ Element.el [ Element.alpha 0.7, Element.scale 0.8 ] icon
                , Element.text text
                ]

        selectedStyles =
            [ Element.Background.color Ui.Style.highlightColorSolid, Element.Font.color Ui.Style.white ]

        notSelectedStyles =
            [ Element.mouseOver [ Element.Background.color Ui.Style.slightAccent ]
            ]

        baseStyles =
            [ Element.Border.rounded 3, Element.width Element.fill ]

        styles =
            baseStyles
                ++ (if isSelected then
                        selectedStyles

                    else
                        notSelectedStyles
                   )
    in
    Element.Input.button styles { onPress = Just onPress, label = label }


{-| Present mappings for the model to the user
-}
viewOptions : Maybe Spec.DataConnection.DataConnection -> Model.Model.UserModel -> Element.Element (Maybe Spec.DataConnection.DataConnection)
viewOptions currentConnection ({ modelEditor, actions, apiExplorer } as userModel) =
    let
        viewValidation : ApiExplorer.Api.ApiParam -> Element.Element (Maybe Spec.DataConnection.DataConnection)
        viewValidation { data, refinedType, fieldName, fieldKey, variable } =
            let
                thisConnection =
                    Spec.DataConnection.FromValidation fieldKey

                ( name, kind ) =
                    case variable of
                        Just variable_ ->
                            ( fieldName, variable_.refinedType )

                        Nothing ->
                            ( fieldName, refinedType )

                icon =
                    Interface.Selection.refinedTypeToIcon kind
            in
            viewSelectable currentConnection { value = thisConnection, text = name, icon = icon }

        viewEntries =
            case Dict.toList modelEditor.fields |> List.filterMap (viewField currentConnection userModel) of
                [] ->
                    Element.paragraph
                        [ Element.padding 10, Element.Background.color Ui.Style.slightAccent ]
                        [ Element.text "No usable fields defined in model" ]

                entries ->
                    entries
                        |> Element.column [ Element.width Element.fill, Element.spacing 5 ]

        viewValidations =
            case Model.allApiParams userModel of
                [] ->
                    Element.paragraph
                        [ Element.padding 10, Element.Background.color Ui.Style.slightAccent ]
                        [ Element.text "No fields with validations in model" ]

                validations ->
                    validations
                        |> List.map viewValidation
                        |> Element.column [ Element.spacing 5, Element.width Element.fill ]

        -- for each action
        -- get the connected model fields
        -- and the connected target
        -- find the right validation for the subtype
    in
    Element.column (Element.spacing 15 :: Canvas.AttributesPanel.Shared.sectionStyles)
        [ Element.el [ Element.Font.bold ] (Element.text "Model")
        , viewEntries
        , Element.el [ Element.Font.bold ] (Element.text "Validations")
        , viewValidations
        , Element.paragraph [] [ Element.text "Setup the state of your application using the Model Editor" ]
        ]


viewField :
    Maybe Spec.DataConnection.DataConnection
    -> Model.Model.UserModel
    -> ( Int, ModelEditor.Model.Field )
    -> Maybe (Element.Element (Maybe Spec.DataConnection.DataConnection))
viewField currentConnection userModel field =
    let
        ( fieldKey, { associations, kind, name } ) =
            field

        param =
            Model.paramForField fieldKey userModel
                |> Maybe.map ApiExplorer.Api.Param.simplify

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
            Spec.DataConnection.Help.iconForModelField userModel field
    in
    case kind_ of
        Just Dynamic.Data.StringKind ->
            viewSelectable currentConnection
                { text = name
                , value = Spec.DataConnection.FromModel (Spec.DataConnection.PlainField fieldKey)
                , icon = icon
                }
                |> Just

        Just Dynamic.Data.NumberKind ->
            viewSelectable currentConnection
                { text = name
                , value = Spec.DataConnection.FromModel (Spec.DataConnection.PlainField fieldKey)
                , icon = icon
                }
                |> Just

        Just (Dynamic.Data.UnionKind _) ->
            let
                headline =
                    Element.row
                        [ Element.alpha 0.5 ]
                        [ icon
                        , Element.text name
                        ]

                options =
                    case Dict.toList associations of
                        [] ->
                            Element.text "No associations"

                        assocs ->
                            Element.column
                                [ Element.spacing 15, Element.width Element.fill ]
                                (assocs |> List.map (viewAssociation userModel currentConnection fieldKey))
            in
            Element.column
                [ Element.width Element.fill
                , Element.Border.width 1
                , Element.padding 5
                , Element.Border.rounded 3
                , Element.Border.color Ui.Style.slightAccent
                ]
                [ headline, options ]
                |> Just

        _ ->
            Nothing


viewAssociation :
    Model.Model.UserModel
    -> Maybe Spec.DataConnection.DataConnection
    -> Int
    -> ( Int, ModelEditor.Model.Association )
    -> Element.Element (Maybe Spec.DataConnection.DataConnection)
viewAssociation userModel currentConnection fieldKey ( associationKey, { name, projection } ) =
    let
        icon =
            case projection of
                ModelEditor.Model.Match matchers ->
                    Dict.toList matchers
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.map (Spec.DataConnection.Help.toIcon userModel)
                        |> Maybe.withDefault Spec.DataConnection.Help.questionMarkIcon

                _ ->
                    Element.none

        association =
            Spec.DataConnection.Association { associationKey = associationKey, fieldKey = fieldKey }
                |> Spec.DataConnection.FromModel
    in
    viewSelectable currentConnection { text = name, icon = icon, value = association }
