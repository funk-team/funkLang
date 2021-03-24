module CodeEditor.Inputs exposing (view)

import ApiExplorer
import ApiExplorer.Api
import ApiExplorer.Model
import CodeEditor.Help
import CodeEditor.Model
import CodeEditor.Msg
import Element
import Element.Border
import Element.Events.Extra
import Element.Input
import Help
import Interface.Selection
import Model.Model
import Persistence
import Route
import Ui.Component
import Ui.Help
import Ui.Style


view : Persistence.ProjectMeta -> Model.Model.UserModel -> CodeEditor.Model.TransformationKey -> CodeEditor.Model.Transformation -> Element.Element CodeEditor.Msg.Msg
view projectMeta userModel transformationKey transformation =
    let
        apiSources =
            -- display onboarding if no api calls are available
            case userModel.apiExplorer |> ApiExplorer.Model.listApiSpecs of
                [] ->
                    [ onboarding projectMeta ]

                specs ->
                    specs
                        |> List.map (renderPossibleInput userModel transformationKey transformation)
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        apiSources


onboarding projectMeta =
    Element.column [ Element.spacing 10 ]
        [ Element.paragraph
            [ Element.width (Element.fill |> Element.maximum 600)
            ]
            [ Element.text "No data sources found. Use the API mode to connect to a server or mock data."
            ]
        , Element.link
            Ui.Component.buttonStyle
            { label = Element.text "Go to API editor"
            , url = Route.makeUrl projectMeta (Route.Editor Route.Api)
            }
        ]


{-| Add or remove and argument and if desired update the parameter names
-}
toggleArgument userModel apiCallKey transformation =
    let
        selected =
            List.member apiCallKey transformation.arguments

        -- https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjD7aaqgpfvAhUhz4UKHXOJCTkQFjACegQICRAD&url=https%3A%2F%2Fcodeburst.io%2Fparameters-arguments-in-javascript-eb1d8bd0ef04&usg=AOvVaw0FS4iCVrMnEbMh7K0NW6MT
        arguments =
            if selected then
                transformation.arguments
                    |> List.filter ((==) apiCallKey >> not)

            else
                transformation.arguments ++ [ apiCallKey ]

        withNewArgs =
            { transformation | arguments = arguments }

        { parameterNames } =
            CodeEditor.Help.getArgs userModel withNewArgs
    in
    { withNewArgs
        | code = CodeEditor.Help.injectParameters parameterNames transformation
    }


renderPossibleInput :
    Model.Model.UserModel
    -> CodeEditor.Model.TransformationKey
    -> CodeEditor.Model.Transformation
    -> ( ApiExplorer.Model.ApiCallKey, ApiExplorer.Model.ApiSpec )
    -> Element.Element CodeEditor.Msg.Msg
renderPossibleInput userModel transformationKey transformation ( apiCallKey, apiCallSpec ) =
    let
        variableName =
            apiCallSpec.name |> Help.toJsVariableName

        selected =
            List.member apiCallKey transformation.arguments

        isExpanded =
            List.member
                apiCallKey
                transformation.expandedArguments

        previewData : Element.Element CodeEditor.Msg.Msg
        previewData =
            case ApiExplorer.getAsJson apiCallSpec userModel.apiExplorer of
                Just data ->
                    ApiExplorer.Api.viewTree
                        Nothing
                        Interface.Selection.empty
                        data
                        |> Element.el
                            [ Ui.Help.noPointerEvents
                            ]
                        |> Element.el
                            ([ Ui.Style.style "cursor" "default"
                             , Element.Events.Extra.onClickStopPropagation CodeEditor.Msg.NoOp
                             , Element.width Element.fill
                             , Element.scrollbars
                             , Element.height Element.fill
                             , Element.width Element.fill
                             ]
                                ++ Ui.Style.paperShadow
                            )

                Nothing ->
                    Element.text "No data available. Try running the request again."

        renameInput : Element.Element CodeEditor.Msg.Msg
        renameInput =
            Element.row
                [ Element.spacing 5
                ]
                [ Ui.Component.contenteditable { enabled = selected, placeholder = "Api Name", text = apiCallSpec.name }
                    |> Element.map (CodeEditor.Msg.RenameApi apiCallKey)
                    |> Element.el
                        (if selected then
                            [ Element.Events.Extra.onKeydownStopPropagation CodeEditor.Msg.NoOp, Element.Events.Extra.onClickStopPropagation CodeEditor.Msg.NoOp ]

                         else
                            []
                        )
                ]

        checkbox : Element.Element CodeEditor.Msg.Msg
        checkbox =
            Element.Input.checkbox []
                { checked = selected
                , icon = Element.Input.defaultCheckbox
                , label = Element.Input.labelRight [] renameInput
                , onChange =
                    always
                        (toggleArgument userModel apiCallKey transformation
                            |> Tuple.pair CodeEditor.Msg.Set
                            >> CodeEditor.Msg.CodeMsg transformationKey
                        )
                }

        previewToggle : Element.Element CodeEditor.Msg.Msg
        previewToggle =
            let
                toggled : CodeEditor.Msg.Msg
                toggled =
                    { transformation
                        | expandedArguments = Help.toggleInList apiCallKey transformation.expandedArguments
                    }
                        |> Tuple.pair CodeEditor.Msg.Set
                        >> CodeEditor.Msg.CodeMsg transformationKey
            in
            Element.el
                (Element.Events.Extra.onClickStopPropagation toggled
                    :: Ui.Component.buttonStyle
                    ++ (if isExpanded then
                            Ui.Component.activeButtonStyles

                        else
                            []
                       )
                )
                (Element.text "Show data in API")

        preview : Element.Element CodeEditor.Msg.Msg
        preview =
            case isExpanded of
                False ->
                    Element.none

                True ->
                    previewData

        header : Element.Element CodeEditor.Msg.Msg
        header =
            Element.row
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ checkbox
                , previewToggle
                ]

        full : Element.Element CodeEditor.Msg.Msg
        full =
            Element.column
                [ Element.width Element.fill
                , Element.height (Element.shrink |> Element.maximum 500)
                , Element.Border.width 1
                , Element.Border.color
                    (if selected then
                        Ui.Style.black

                     else
                        Ui.Style.slightAccent
                    )
                , Element.padding 20
                , Element.spacing 10
                , Element.Border.rounded 3
                ]
                [ header, preview ]
    in
    Element.Input.button
        [ Element.width Element.fill
        ]
        { label = full
        , onPress =
            Just
                (toggleArgument userModel apiCallKey transformation
                    |> Tuple.pair CodeEditor.Msg.Set
                    >> CodeEditor.Msg.CodeMsg transformationKey
                )
        }
