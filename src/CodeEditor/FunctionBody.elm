module CodeEditor.FunctionBody exposing (view)

import CodeEditor.Help
import CodeEditor.Model
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Help
import Model.Model
import Ui.CodeEditor
import Ui.Style


view : Model.Model.UserModel -> CodeEditor.Model.Transformation -> Element.Element CodeEditor.Model.Transformation
view userModel transformation =
    let
        editor =
            Ui.CodeEditor.view
                [ Ui.CodeEditor.value transformation.code
                , Ui.CodeEditor.mode "javascript"
                , Ui.CodeEditor.onChange (\code_ -> { transformation | code = code_ })
                ]
                |> Element.html
                |> Element.el [ Element.width Element.fill ]
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ editor
        , executionPreview userModel transformation
        ]


executionPreview userModel transformation =
    let
        execution =
            Element.column
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ Element.Input.button
                    [ Element.paddingXY 10 5
                    , Element.Background.color
                        (if transformation.debugOpen then
                            Ui.Style.highlightColor

                         else
                            Element.rgba 0 0 0 0.05
                        )
                    , Element.Border.rounded 3
                    ]
                    { label = Element.text "Debug"
                    , onPress =
                        Just
                            { transformation
                                | debugOpen = not transformation.debugOpen
                            }
                    }
                , case transformation.debugOpen of
                    True ->
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing 10
                            ]
                            [ Element.text "This is how we call the function:"
                            , render
                            ]

                    False ->
                        Element.none
                ]

        moduleName =
            transformation.name |> Help.toJsVariableName

        { parameterValues, parameterNames } =
            CodeEditor.Help.getArgs userModel transformation

        render =
            Element.el
                [ Ui.Style.monospace
                , Element.Font.color Ui.Style.highlightColorSolid
                , Element.Background.color (Element.rgba 0 0 0 0.05)
                , Element.Border.rounded 3
                , Element.padding 10
                , Element.width Element.fill
                ]
                (Element.text executionPreviewString)

        executionPreviewString : String
        executionPreviewString =
            "import "
                ++ moduleName
                ++ "\n\nconst result = await "
                ++ moduleName
                ++ "("
                ++ String.join ", " (parameterNames |> List.map (\name -> "await " ++ name ++ "()"))
                ++ ")"
    in
    execution
