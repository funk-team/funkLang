port module CodeEditor exposing (init, subscriptions, update, view)

-- TODO
-- - add comments seciton
-- - add use at your own risk info

import ApiExplorer.Model
import CodeEditor.FunctionBody
import CodeEditor.Help
import CodeEditor.Inputs
import CodeEditor.Model
import CodeEditor.Msg
import CodeEditor.OutputSelection
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import IntDict
import IntDict.Typed
import Json.Decode as Decode
import Json.Encode as Encode
import Model
import Model.Model
import Persistence
import Time
import Ui.Boxicons
import Ui.Component
import Ui.DesignSystem
import Ui.Style


init =
    always Cmd.none


port registerImports : List String -> Cmd msg


port runCode : ExecutionInstructions -> Cmd msg


type alias ExecutionInstructions =
    { sourceId : Int
    , code : String
    , values : List Encode.Value
    }


port gotCodeResult : (CodeEditor.Model.RawExecutionResult -> msg) -> Sub msg


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


view : Persistence.ProjectMeta -> Model.Model.UserModel -> Element.Element CodeEditor.Msg.Msg
view projectMeta userModel =
    Element.row
        [ Element.width Element.fill, Element.height Element.fill ]
        [ viewSidebar userModel.codeEditor
        , viewCurrentCodeEditor projectMeta userModel userModel.codeEditor
            |> Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingEach { edges | left = 15 }
                ]
        ]


setResult : CodeEditor.Model.ExecutionReturn -> CodeEditor.Model.Transformation -> CodeEditor.Model.Transformation
setResult result t =
    { t
        | executionState =
            case t.executionState of
                Nothing ->
                    Just result

                -- prevent race conditions for async operation
                -- timestamp at time of requesting the execution, not the time of arrival of the result dictates which is the most recent result
                Just someOtherResult ->
                    if Time.posixToMillis someOtherResult.timestamp > Time.posixToMillis result.timestamp then
                        Just someOtherResult

                    else
                        Just result
    }


update :
    CodeEditor.Msg.Msg
    -> Model.Model.UserModel
    -> ( CodeEditor.Model.Model, Cmd CodeEditor.Msg.Msg )
update msg userModel =
    let
        model =
            userModel.codeEditor
    in
    case msg of
        CodeEditor.Msg.ShowOrHideHelpPanel ->
            ( { model
                | helpOpen = not model.helpOpen
              }
            , Cmd.none
            )

        CodeEditor.Msg.RenameApi _ _ ->
            ( model, Cmd.none )

        CodeEditor.Msg.PropagateChangesFromApiEditor ->
            ( { model
                | items =
                    IntDict.Typed.map
                        CodeEditor.Model.wrapKey
                        (\transformationKey transformation ->
                            let
                                { parameterNames } =
                                    CodeEditor.Help.getArgs userModel transformation

                                code =
                                    CodeEditor.Help.injectParameters
                                        parameterNames
                                        transformation
                            in
                            { transformation | code = code }
                        )
                        model.items
              }
            , Cmd.none
            )

        CodeEditor.Msg.GotCodeResult result ->
            ( { model
                | items =
                    IntDict.Typed.update
                        CodeEditor.Model.unwrapKey
                        result.sourceId
                        (Maybe.map (setResult result))
                        model.items
              }
            , Cmd.none
            )

        CodeEditor.Msg.NoOp ->
            ( model, Cmd.none )

        CodeEditor.Msg.SelectCode key ->
            ( { model | selected = Just key }, Cmd.none )

        CodeEditor.Msg.AddCode ->
            let
                nextId =
                    IntDict.Typed.nextId CodeEditor.Model.wrapKey model.items

                transformationName =
                    "Code " ++ String.fromInt (IntDict.Typed.size model.items + 1)

                newCode : CodeEditor.Model.Transformation
                newCode =
                    { name = transformationName

                    -- view
                    , expandedArguments = []
                    , argumentsCollapsed = False
                    , codeSectionCollapsed = False
                    , debugOpen = False

                    -- API input selection
                    , arguments = []

                    -- writing the code itself
                    , code = defaultCode

                    -- running & connecting output
                    , executionState = Nothing
                    , outputSelection = ApiExplorer.Model.emptySelection
                    }

                newCodes =
                    IntDict.Typed.insert CodeEditor.Model.unwrapKey
                        nextId
                        newCode
                        model.items

                newModel =
                    { model
                        | items = newCodes
                        , selected = Just nextId
                        , helpOpen = False
                    }
            in
            ( newModel
            , runTransformation userModel nextId newCode
            )

        CodeEditor.Msg.CodeMsg key ( kind, transformation ) ->
            case kind of
                CodeEditor.Msg.Run ->
                    ( model, runTransformation userModel key transformation )

                CodeEditor.Msg.Set ->
                    let
                        hasChanged =
                            case IntDict.Typed.get CodeEditor.Model.unwrapKey key model.items of
                                Nothing ->
                                    True

                                Just { code } ->
                                    not (transformation.code == code)

                        runIfChanged =
                            if hasChanged then
                                runTransformation
                                    userModel
                                    key
                                    transformation

                            else
                                Cmd.none
                    in
                    ( { model
                        | items =
                            IntDict.Typed.insert CodeEditor.Model.unwrapKey key transformation model.items
                      }
                    , runIfChanged
                    )

        CodeEditor.Msg.RemoveCode key ->
            ( { model
                | items = IntDict.Typed.remove CodeEditor.Model.unwrapKey key model.items
              }
            , Cmd.none
            )


runTransformation : Model.Model.UserModel -> CodeEditor.Model.TransformationKey -> CodeEditor.Model.Transformation -> Cmd CodeEditor.Msg.Msg
runTransformation userModel transformationKey transformation =
    let
        compiled =
            CodeEditor.Help.getArgs
                userModel
                transformation
    in
    runCode
        { sourceId = CodeEditor.Model.unwrapKey transformationKey
        , values = compiled.parameterValues
        , code = transformation.code
        }


type alias CompiledFunction =
    { head : String
    , execution : String
    , body : String
    }


defaultCode =
    """import maxiredCanvasConfetti from 'https://cdn.skypack.dev/maxired-canvas-confetti';

function randomInRange(min, max) {
  return Math.random() * (max - min) + min;
}

export default async (""" ++ CodeEditor.Help.paramsStartDelimiter ++ " " ++ CodeEditor.Help.paramsEndDelimiter ++ """) => {
    maxiredCanvasConfetti({
      angle: randomInRange(55, 125),
      spread: randomInRange(50, 70),
      particleCount: randomInRange(50, 100),
      origin: { y: 0.6 }
    });
    return randomInRange(1, 1000)
}"""


viewSidebar : CodeEditor.Model.Model -> Element.Element CodeEditor.Msg.Msg
viewSidebar model =
    let
        -- create and help button 😂
        newCodeButton =
            Element.Input.button
                Ui.Component.buttonStyle
                { label = Element.text "Add function"
                , onPress = Just CodeEditor.Msg.AddCode
                }

        helpButton_ =
            case IntDict.Typed.isEmpty model.items of
                True ->
                    Element.none

                False ->
                    Ui.Component.helpButton model CodeEditor.Msg.ShowOrHideHelpPanel

        helpButton =
            case model.selected of
                Nothing ->
                    Element.none

                Just _ ->
                    helpButton_

        buttonRow =
            Element.row
                [ Element.spacing 10
                , Element.padding 10
                , Element.Border.widthEach { edges | bottom = 1 }
                , Element.width Element.fill
                ]
                [ newCodeButton
                , helpButton
                ]

        -- actual stuff
        transformations =
            model
                |> CodeEditor.Model.listCodes
                |> List.map (viewTransformationInSidebar model)
    in
    Element.column
        [ Element.height Element.fill
        , Element.width Element.shrink
        , Element.Border.widthEach { edges | right = 1 }
        ]
        (buttonRow :: transformations)


help helpOpenExplicit =
    Element.column
        [ Element.paddingEach { top = 20, right = 10, bottom = 0, left = 0 }
        , Element.spacing 10
        ]
        [ Ui.Component.textListWithHeader helpOpenExplicit
            "Transform data within funk using custom code"
            [ "➡ Use JavaScript to transform data from within funk"
            , "➡ Load optimized npm packages from www.skypack.dev"
            , "➡ Feed transformed data into UI components"
            , "➡ Write transformations in your text editor using the funck CLI"
            , "➡ STATUS: WIP, UX update coming late February"
            ]
            |> Element.map (always CodeEditor.Msg.ShowOrHideHelpPanel)
        ]


viewCurrentCodeEditor : Persistence.ProjectMeta -> Model.Model.UserModel -> CodeEditor.Model.Model -> Element.Element CodeEditor.Msg.Msg
viewCurrentCodeEditor projectMeta userModel model =
    let
        selectOnLeftMsg =
            Element.column [ Element.padding 20, Element.spacing 20 ]
                [ Element.text "Select one of the transformations on the left or create new transformation."
                ]
    in
    case ( model.selected, CodeEditor.Model.listCodes model == [] || model.helpOpen ) of
        ( _, True ) ->
            help (not (CodeEditor.Model.listCodes model == []) && model.helpOpen)

        ( Nothing, False ) ->
            selectOnLeftMsg

        ( Just selected, False ) ->
            case CodeEditor.Model.getItem selected model of
                Nothing ->
                    selectOnLeftMsg

                Just transformation ->
                    let
                        openIcon open =
                            Ui.Component.icon
                                (if open then
                                    Ui.Boxicons.bxChevronUp

                                 else
                                    Ui.Boxicons.bxChevronDown
                                )

                        argumentsStep =
                            Ui.Component.collapsibleStep
                                (Element.Input.button []
                                    { onPress =
                                        { transformation | argumentsCollapsed = not transformation.argumentsCollapsed }
                                            |> (CodeEditor.Msg.CodeMsg selected << Tuple.pair CodeEditor.Msg.Set)
                                            |> Just
                                    , label =
                                        Element.row
                                            [ Element.spacing 5 ]
                                            [ openIcon <| not transformation.argumentsCollapsed
                                            , Element.text "1. Select APIs with required data"
                                            ]
                                    }
                                )
                                (if transformation.argumentsCollapsed then
                                    Nothing

                                 else
                                    Just <| CodeEditor.Inputs.view projectMeta userModel selected transformation
                                )

                        writeCodeStep =
                            let
                                inlineCode code =
                                    Element.el [ Element.paddingXY 5 2, Element.Border.rounded 4, Element.Background.color (Element.rgba 0 0 0 0.07), Ui.Style.monospace ] (Element.text code)

                                usageInfo =
                                    Element.paragraph [ Element.width (Element.fill |> Element.maximum 600), Element.spacing 8 ] <|
                                        [ Element.text "The data you selected above is accessible in the below code block using object notation e.g. "
                                        , inlineCode "myApiName.userName"
                                        , Element.text "."
                                        ]

                                usageInfo3 =
                                    Element.paragraph [ Element.width (Element.fill |> Element.maximum 600), Element.spacing 8 ] <|
                                        [ Element.text "Remove the "
                                        , inlineCode CodeEditor.Help.paramsStartDelimiter
                                        , Element.text " and "
                                        , inlineCode CodeEditor.Help.paramsEndDelimiter
                                        , Element.text " annotations if you want to manually define variables for the function. Renaming the API does not yet automatically update the variable names in the code block and will break your code, we are working to improve this."
                                        ]

                                usageInfo2 =
                                    Element.paragraph [ Element.width (Element.fill |> Element.maximum 600), Element.spacing 8 ]
                                        [ Element.text "You can import millions of NPM packages from "
                                        , Element.newTabLink
                                            [ Element.Font.color Ui.Style.grey
                                            , Element.mouseOver [ Element.Font.color Ui.Style.highlightColorSolid ]
                                            ]
                                            { url = "https://www.skypack.dev", label = Element.text "https://www.skypack.dev" }
                                        , Element.text "."
                                        ]

                                body : Element.Element CodeEditor.Msg.Msg
                                body =
                                    Element.map (CodeEditor.Msg.CodeMsg selected << Tuple.pair CodeEditor.Msg.Set)
                                        (Element.column
                                            [ Element.spacing 15
                                            , Element.width Element.fill
                                            ]
                                            [ usageInfo
                                            , usageInfo2
                                            , usageInfo3
                                            , CodeEditor.FunctionBody.view userModel transformation
                                            ]
                                        )
                            in
                            Ui.Component.collapsibleStep
                                (Element.Input.button []
                                    { onPress =
                                        Just { transformation | codeSectionCollapsed = not transformation.codeSectionCollapsed }
                                    , label =
                                        Element.row
                                            [ Element.spacing 5 ]
                                            [ openIcon <| not transformation.codeSectionCollapsed
                                            , Element.text "2. Write code"
                                            ]
                                    }
                                    |> Element.map (Tuple.pair CodeEditor.Msg.Set >> CodeEditor.Msg.CodeMsg selected)
                                )
                                (if transformation.codeSectionCollapsed then
                                    Nothing

                                 else
                                    Just body
                                )
                    in
                    Element.column
                        [ Element.paddingXY 0 15
                        , Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 30
                        ]
                        [ argumentsStep
                        , writeCodeStep
                        , Ui.Component.collapsibleStep
                            (Element.text "3. Select the return value from the code block to use within your app")
                            (Just <| CodeEditor.OutputSelection.view userModel transformation)
                            |> Element.map (CodeEditor.Msg.CodeMsg selected)
                        ]


viewTransformationInSidebar : CodeEditor.Model.Model -> ( CodeEditor.Model.TransformationKey, CodeEditor.Model.Transformation ) -> Element.Element CodeEditor.Msg.Msg
viewTransformationInSidebar model ( key, transformation ) =
    let
        isSelected =
            Just key == model.selected

        transformationNameInput =
            Ui.Component.contenteditable
                { text = transformation.name
                , placeholder = "Unnamed function"
                , enabled = isSelected
                }
                |> Element.map (\newName -> CodeEditor.Msg.CodeMsg key ( CodeEditor.Msg.Set, { transformation | name = newName } ))

        label : Element.Element CodeEditor.Msg.Msg
        label =
            Element.column
                [ Element.width Element.fill
                , Element.spacing 5
                ]
                [ Element.row [ Element.width Element.fill ]
                    [ transformationNameInput
                    ]
                ]

        rowParams : Ui.DesignSystem.SimpleRowParams CodeEditor.Msg.Msg
        rowParams =
            { isSelected = isSelected
            , onRemove = Just (CodeEditor.Msg.RemoveCode key)
            , msg = CodeEditor.Msg.SelectCode key
            , label = label
            , attribs = []
            }

        transformationRow =
            Ui.DesignSystem.viewSidebarRowSimple
                rowParams
    in
    transformationRow


subscriptions : Sub CodeEditor.Msg.Msg
subscriptions =
    gotCodeResult (refineCodeResult >> CodeEditor.Msg.GotCodeResult)


refineCodeResult : CodeEditor.Model.RawExecutionResult -> CodeEditor.Model.ExecutionReturn
refineCodeResult { timestamp, sourceId, return_ } =
    { timestamp = Time.millisToPosix timestamp -- UNIX timestamp for fetching the latest version
    , sourceId = CodeEditor.Model.wrapKey sourceId -- id of the original transformation
    , return =
        case Decode.decodeValue decodeReturnResult return_ of
            Err _ ->
                Err "Could not parse result"

            Ok (Err errFromJs) ->
                Err errFromJs

            Ok (Ok data) ->
                Ok data
    }


decodeReturnResult : Decode.Decoder CodeEditor.Model.DataResult
decodeReturnResult =
    Decode.oneOf
        [ Decode.field "err" Decode.string |> Decode.map Err
        , Decode.field "ok" Decode.value |> Decode.map Ok
        ]
