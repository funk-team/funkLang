module CodeEditor.OutputSelection exposing (view)

import ApiExplorer
import ApiExplorer.Api
import CodeEditor.Model
import CodeEditor.Msg
import Dict.Any
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Model.Model
import Ui.Component
import Ui.Style


view : Model.Model.UserModel -> CodeEditor.Model.Transformation -> Element.Element ( CodeEditor.Msg.CodeMsgDetail, CodeEditor.Model.Transformation )
view userModel transformation =
    let
        result =
            transformation.executionState

        selectionTable =
            case result of
                Just { return } ->
                    case return of
                        Err problem ->
                            Element.none

                        Ok data ->
                            case transformation.outputSelection |> Dict.Any.toList of
                                [] ->
                                    Element.none

                                _ ->
                                    Element.column [ Element.width Element.fill ]
                                        [ Element.text "These are the values you have selected"
                                        , ApiExplorer.Api.viewTable data transformation.outputSelection
                                            |> Element.map (\selection -> ( CodeEditor.Msg.Set, { transformation | outputSelection = selection } ))
                                        ]

                _ ->
                    Element.none

        runCodeButton suffix =
            Element.Input.button
                Ui.Component.buttonStyle
                { onPress = Just ( CodeEditor.Msg.Run, transformation )
                , label = Element.text <| "Run module" ++ suffix
                }

        results =
            case result of
                Nothing ->
                    Element.column []
                        [ Element.text "no result available"
                        , runCodeButton ""
                        ]

                Just { return } ->
                    case return of
                        Err problem ->
                            let
                                msg =
                                    case problem of
                                        "TypeError: error loading dynamically imported module" ->
                                            problem ++ ". Please reload the funk editor."

                                        _ ->
                                            problem
                            in
                            Element.column
                                [ Element.padding 20
                                , Element.Background.color Ui.Style.error
                                , Element.Font.color Ui.Style.white
                                , Element.spacing 10
                                , Element.Border.rounded 3
                                ]
                                [ Element.text "A problem has occurred"
                                , Element.paragraph [ Ui.Style.monospace ] [ Element.text msg ]
                                , runCodeButton " again"
                                ]

                        Ok data ->
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing 10
                                ]
                                [ Element.paragraph []
                                    [ Element.text "Returned data from the above module. Click on values to select, name and use them in your app." ]
                                , ApiExplorer.Api.viewTree
                                    (Just (\newSelection -> ( CodeEditor.Msg.Set, { transformation | outputSelection = newSelection } )))
                                    transformation.outputSelection
                                    data
                                ]
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ results
        , selectionTable
        ]
