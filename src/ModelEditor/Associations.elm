module ModelEditor.Associations exposing (..)

import Dict
import Dynamic.Data
import Element
import Element.Background
import Element.Font
import Element.Input
import IntDict
import Interface.Data
import Interface.JsonTree.Model
import Interface.Model
import List.Extra
import ModelEditor.Model
import Spec.DataConnection
import Ui.Boxicons
import Ui.Component
import Ui.Style
import Ui.Table2


{-| Complexity: because dynamic data types can currently not be tagged we can not define invariants
that link different kinds to projections - like union kinds to matcher projection

We have to do diligigence manually until we can

-}
viewNewAssocationButton : ModelEditor.Model.Field -> Element.Element ModelEditor.Model.Field
viewNewAssocationButton field =
    let
        addButton : Maybe ModelEditor.Model.Projection -> Element.Element ModelEditor.Model.Field
        addButton newProjection =
            case newProjection of
                Nothing ->
                    Element.none

                Just newProjection_ ->
                    let
                        associationName =
                            "Association " ++ String.fromInt (Dict.size field.associations + 1)

                        newAssociations =
                            IntDict.insertNew
                                (ModelEditor.Model.Association associationName newProjection_)
                                field.associations
                    in
                    Ui.Component.buttonOnClick
                        { field | associations = newAssociations }
                        "Add association"
                        True
    in
    case field.kind of
        ModelEditor.Model.Unspecified ->
            Element.none

        ModelEditor.Model.Typed kind ->
            addButton <| findProjectionFor kind

        ModelEditor.Model.WithDefault instance ->
            addButton <| findProjectionFor (Dynamic.Data.toKind instance)



-- UNION TYPES
-- use dict.combine to make a matcher preprocessed thing
-- view selectable option (for now only literal string)


findProjectionFor : Dynamic.Data.Kind -> Maybe ModelEditor.Model.Projection
findProjectionFor kind =
    case kind of
        Dynamic.Data.UnionKind _ ->
            Just ModelEditor.Model.emptyMatchers

        _ ->
            Nothing


{-| View the associations for a given field
Returns either a new field or which matcher and which branch of a specific matcher to target for a select operation.
-}
view : ModelEditor.Model.Field -> Element.Element (NewDataOrSignal ModelEditor.Model.Field ( Int, Int ))
view field =
    let
        isMatch =
            case field.kind of
                ModelEditor.Model.Typed (Dynamic.Data.UnionKind _) ->
                    True

                ModelEditor.Model.WithDefault (Dynamic.Data.UnionInstance _ _) ->
                    True

                _ ->
                    False

        renderedAssociations =
            case ( Dict.isEmpty field.associations, isMatch ) of
                ( True, True ) ->
                    Element.column [ Element.width Element.fill, Element.spacingXY 0 20 ]
                        [ Element.paragraph []
                            [ Element.text "Associations allow you to connect data to a Custom Type." ]
                        , Element.paragraph []
                            [ Element.text "This might be images, data from an API, icons or even other values from the mode!" ]
                        , Element.paragraph []
                            [ Element.text "Connect the Model to your application via the Content tab in the sidebar." ]
                        , Element.paragraph []
                            [ Element.text "Update the Model via the Actions tab in the sidebar." ]
                        ]

                ( False, True ) ->
                    field.associations
                        |> Dict.toList
                        |> List.indexedMap (viewAssociation field)
                        |> Element.column [ Element.spacing 40, Element.width Element.fill ]

                _ ->
                    Element.none
    in
    Element.column
        [ Element.spacing 30
        , Element.width Element.fill
        ]
        [ renderedAssociations
            |> Element.map
                (mapNewDataOrSignal
                    (\( key, association ) ->
                        { field | associations = Dict.insert key association field.associations }
                    )
                    identity
                )
        , viewNewAssocationButton field
            |> Element.map NewData
        ]


viewAssociation :
    ModelEditor.Model.Field
    -> Int
    -> ( Int, ModelEditor.Model.Association )
    -> Element.Element (NewDataOrSignal ( Int, ModelEditor.Model.Association ) ( Int, Int ))
viewAssociation field index ( key, association ) =
    let
        projectionSelector :
            Dynamic.Data.Kind
            -> Element.Element (NewDataOrSignal ModelEditor.Model.Projection ( Int, Int ))
        projectionSelector kind =
            case kind of
                Dynamic.Data.StringKind ->
                    Element.Input.button
                        [ Element.alpha
                            (if association.projection == ModelEditor.Model.Verbatim then
                                1

                             else
                                0.5
                            )
                        ]
                        { label = Element.text "Literal"
                        , onPress = Just ModelEditor.Model.Verbatim
                        }
                        |> Element.map NewData

                Dynamic.Data.NumberKind ->
                    Element.Input.button
                        [ Element.alpha
                            (if association.projection == ModelEditor.Model.FormatNumber then
                                1

                             else
                                0.5
                            )
                        ]
                        { label = Element.text "Format"
                        , onPress = Just ModelEditor.Model.FormatNumber
                        }
                        |> Element.map NewData

                Dynamic.Data.UnionKind variants ->
                    case Dict.isEmpty variants of
                        True ->
                            "As soon as you define variants you may associate them with how to display them in the app"
                                |> Element.text

                        False ->
                            let
                                isActiveMatcher : Int -> Bool
                                isActiveMatcher matcherKey =
                                    case field.connectionWorkflowTarget of
                                        Just openConnection ->
                                            key == openConnection.associationKey && matcherKey == openConnection.matcherKey

                                        Nothing ->
                                            False

                                renderMatchers =
                                    viewMatchers association isActiveMatcher variants

                                ( isMatch, matchers ) =
                                    case association.projection of
                                        ModelEditor.Model.Match matchers_ ->
                                            ( True, Just matchers_ )

                                        _ ->
                                            ( False, Nothing )
                            in
                            case matchers of
                                Nothing ->
                                    Element.none

                                Just m ->
                                    renderMatchers m
                                        |> Element.map
                                            (\newDataOrSignal ->
                                                case newDataOrSignal of
                                                    Signal ( matcherKey, StartConnect ) ->
                                                        Signal ( key, matcherKey )

                                                    Signal ( matcherKey, Delete ) ->
                                                        NewData (ModelEditor.Model.Match (Dict.remove matcherKey m))

                                                    NewData newProjection ->
                                                        NewData (ModelEditor.Model.Match newProjection)
                                            )

                _ ->
                    Element.text "This kind of data is not supported yet"

        projectionEditor : Element.Element (NewDataOrSignal ModelEditor.Model.Projection ( Int, Int ))
        projectionEditor =
            case field.kind of
                ModelEditor.Model.Unspecified ->
                    Element.text "specify a type for the field before defining a projection"

                ModelEditor.Model.Typed type_ ->
                    projectionSelector type_

                ModelEditor.Model.WithDefault instance ->
                    projectionSelector <| Dynamic.Data.toKind instance
    in
    Element.column
        [ Element.spacing 10 ]
        [ projectionEditor
            |> Element.map
                (mapNewDataOrSignal
                    (\projection ->
                        ( key, { association | projection = projection } )
                    )
                    identity
                )
        ]



-- CUSTOM-TYPE-SPECFICIC MATCHERS


{-| An element can update its value or emit a signal to open a panel or something
-}
type NewDataOrSignal newData signal
    = NewData newData
    | Signal signal


mapNewDataOrSignal : (newData -> newData_) -> (signal -> signal_) -> NewDataOrSignal newData signal -> NewDataOrSignal newData_ signal_
mapNewDataOrSignal dataFn signalFn emitted =
    case emitted of
        NewData n ->
            NewData (dataFn n)

        Signal signal ->
            Signal (signalFn signal)


type alias Return =
    NewDataOrSignal ModelEditor.Model.Matchers ( Int, StartConnectOrDelete )


viewMatchers :
    ModelEditor.Model.Association
    -> (Int -> Bool)
    -> Dynamic.Data.UnionConstructors
    -> ModelEditor.Model.Matchers
    -> Element.Element (NewDataOrSignal ModelEditor.Model.Matchers ( Int, StartConnectOrDelete ))
viewMatchers association isActiveMatcher constructors matchers =
    let
        tableParams =
            { data = existingMatchers ++ missingMatchers
            , columns = columns
            }

        columns : List (Element.Column MatcherKind (NewDataOrSignal ModelEditor.Model.Matchers ( Int, StartConnectOrDelete )))
        columns =
            [ nameColumn
            , connectedColumn
            , typeColumn
            , valueColumn
            , deleteColumn
            ]

        deleteColumn =
            { header = Element.none
            , width = Element.shrink
            , view =
                \matcher ->
                    case matcher of
                        Mapped _ ( constructorId, _ ) ->
                            Ui.Table2.button
                                (Ui.Table2.icon Ui.Boxicons.bxTrash)
                                (Signal ( constructorId, Delete ))

                        NotMapped ( _, ( constructorId, _ ) ) ->
                            Element.none
            }

        connectedColumn : Element.Column MatcherKind Return
        connectedColumn =
            { width = Element.shrink
            , header = Ui.Table2.headerText "Connected"
            , view =
                \matcher ->
                    let
                        ( constructorId, hasMapping ) =
                            case matcher of
                                Mapped _ ( constructorId_, value ) ->
                                    ( constructorId_, True )

                                NotMapped ( constructorId_, _ ) ->
                                    ( constructorId_, False )
                    in
                    case isActiveMatcher constructorId of
                        False ->
                            let
                                button =
                                    if hasMapping then
                                        Ui.Table2.button (Element.text "Update value") (( constructorId, StartConnect ) |> Signal)
                                            |> Element.map bakeConnectOrDelete

                                    else
                                        Ui.Table2.buttonNoHover (Element.text "Connect value") (( constructorId, StartConnect ) |> Signal)
                                            |> Element.map bakeConnectOrDelete

                                checkmark =
                                    if hasMapping then
                                        Ui.Table2.icon Ui.Boxicons.bxCheck

                                    else
                                        Element.none
                            in
                            Element.row
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Ui.Style.class "funk-table-alternatives"
                                ]
                                [ checkmark, button ]

                        True ->
                            viewSelecting
            }

        valueColumn : Element.Column MatcherKind Return
        valueColumn =
            { width = Element.shrink
            , header = Ui.Table2.headerText "Value"
            , view =
                \matcher ->
                    let
                        userSelectedValue =
                            getFriendlyValueFromAssociation matcher
                    in
                    Ui.Table2.textCell userSelectedValue
            }

        typeColumn : Element.Column MatcherKind Return
        typeColumn =
            { width = Element.shrink
            , header = Ui.Table2.headerText "Type"
            , view =
                \matcher ->
                    let
                        userSelectedValue =
                            getFriendlyTypeNameFromAssociation matcher
                    in
                    Ui.Table2.textCell userSelectedValue
            }

        nameColumn =
            { width = Element.shrink
            , header = Ui.Table2.headerText "Matcher"
            , view =
                \matcher ->
                    case matcher of
                        Mapped constructorName _ ->
                            Ui.Table2.textCell
                                constructorName

                        NotMapped ( _, ( constructorName, _ ) ) ->
                            Ui.Table2.textCell
                                constructorName
            }

        bakeConnectOrDelete =
            identity

        table =
            Ui.Table2.view tableParams

        existingMatchers =
            Dict.toList matchers
                |> List.filterMap
                    (\( constructorId, dataConnection ) ->
                        case Dict.get constructorId constructors of
                            Nothing ->
                                Nothing

                            Just ( constructorName, _ ) ->
                                Just <| Mapped constructorName ( constructorId, dataConnection )
                     -- |> List.map (viewDefinedMatcher isActiveMatcher constructors >> bakeRow)
                    )

        missingMatchers =
            Dict.diff constructors matchers
                |> Dict.toList
                |> List.map NotMapped

        -- |> List.map (viewMatcherWithoutConnection isActiveMatcher >> (bakeConnectOrDelete >> bakeRow))
    in
    Element.column [ Element.width (Element.px 300) ]
        [ Element.paragraph [ Element.Font.italic ] [ Element.text association.name ]
        , Element.row [ Element.spacingXY 5 0 ]
            [ Element.el [ Element.Background.color Ui.Style.lightGrey, Element.width (Element.px 2), Element.height Element.fill ] Element.none
            , table
            ]
        ]


type MatcherKind
    = Mapped String ( Int, Spec.DataConnection.DataConnection )
    | NotMapped ( Int, Dynamic.Data.UnionConstructor )


type StartConnectOrDelete
    = StartConnect
    | Delete


viewSelecting =
    Ui.Table2.textCell
        "Selecting..."



-- HELP


{-| Get the human readable type of an Association to display in the Model table
-}
getFriendlyTypeNameFromAssociation matcher =
    case matcher of
        Mapped _ ( constructorId_, value ) ->
            let
                val =
                    case value of
                        Spec.DataConnection.FromInterface _ ->
                            "API"

                        Spec.DataConnection.FromModel _ ->
                            "Model"

                        Spec.DataConnection.FromValidation _ ->
                            "Validation"

                        Spec.DataConnection.Static refinedValue ->
                            case refinedValue of
                                Interface.Data.ParagraphText _ ->
                                    "Text"

                                Interface.Data.YoutubeEmbed _ ->
                                    "YouTube"

                                _ ->
                                    "Refined Value"

                        Spec.DataConnection.Media _ ->
                            "Media"

                        Spec.DataConnection.Icon _ ->
                            "Icon"

                        _ ->
                            "Unknown"
            in
            val

        NotMapped ( constructorId_, _ ) ->
            ""


{-| Get the value name or path from to display in the Model table

For Media we display the media file name
For YouTube videos we display the videoID
For API values we display the API path
For other values (Icons, Model, Validation) we fall back to hard coded values like

TODO Add better support for other values (i.e. the name of the Model/Association filed)

-}
getFriendlyValueFromAssociation matcher =
    case matcher of
        Mapped _ ( constructorId_, value ) ->
            let
                val =
                    case value of
                        -- API
                        Spec.DataConnection.FromInterface interfacePointer ->
                            let
                                interfacePointer_ =
                                    case interfacePointer.kind of
                                        Just kind ->
                                            getThePathOfAnApiAssociation kind

                                        Nothing ->
                                            "Select a value"
                            in
                            interfacePointer_

                        -- Model
                        Spec.DataConnection.FromModel modelTarget ->
                            case modelTarget of
                                Spec.DataConnection.Association associationTarget ->
                                    "Association"

                                Spec.DataConnection.PlainField int ->
                                    "Plain Field"

                        -- Validatoin
                        Spec.DataConnection.FromValidation int_ ->
                            "Validation"

                        -- YouTube and Text
                        Spec.DataConnection.Static refinedValue ->
                            case refinedValue of
                                Interface.Data.ParagraphText text ->
                                    text

                                Interface.Data.YoutubeEmbed youtubeEmbe ->
                                    youtubeEmbe.videoId

                                _ ->
                                    "Refined Value"

                        -- Media (PDF, Images, Movies)
                        Spec.DataConnection.Media mediaDetails ->
                            mediaDetails.meta.title

                        -- Icon
                        Spec.DataConnection.Icon iconSet ->
                            "Icon"

                        _ ->
                            "Unknown"
            in
            -- Clip the value if it's longer then 30 character
            if String.length val > 30 then
                String.left 27 val ++ "..."

            else
                val

        NotMapped ( constructorId_, _ ) ->
            ""


{-| Get Path of an API connected to the model
Formated as objectAccessor.objectAccessorId (e.g. backdrop\_path.mainImage.jpg)
-}
getThePathOfAnApiAssociation kind =
    case kind of
        Interface.Model.RelativeToInterfaceRoot root ->
            let
                objectAccessor =
                    case List.head root.rootPath of
                        Just (Interface.JsonTree.Model.ObjectAccessor apiKeyName) ->
                            apiKeyName

                        _ ->
                            "List"

                objectAccessorId =
                    case List.Extra.last root.rootPath of
                        Just (Interface.JsonTree.Model.ObjectAccessor apiKeyName) ->
                            apiKeyName

                        _ ->
                            ""
            in
            objectAccessor ++ "." ++ objectAccessorId

        Interface.Model.FromListScope root ->
            "List"


getNamedFields : ModelEditor.Model.Model -> IntDict.Dict ( String, ModelEditor.Model.Field )
getNamedFields model =
    Dict.toList model.fields
        |> List.indexedMap
            (\index ( key, field ) ->
                let
                    displayName =
                        field.name
                in
                ( key, ( displayName, field ) )
            )
        |> Dict.fromList


{-| placeholder texts
-}
indexToFood index =
    case index of
        0 ->
            "Tab1"

        1 ->
            "Tab2"

        2 ->
            "Tab3"

        _ ->
            "Something else"
