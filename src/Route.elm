module Route exposing (..)

import Persistence
import Slug
import Spec.Element.Id
import Url
import Url.Builder
import Url.Parser exposing ((</>))


type Route
    = Home
    | ResponsifyTestingEnvironment
    | Project Persistence.ProjectMeta ProjectMode
    | Ui


type ProjectMode
    = Editor EditorMode
    | Preview Slug.Slug
    | CodeGenPreview Spec.Element.Id.Id


type EditorMode
    = Canvas
    | DesignSystem
    | Model
    | Api
    | Code
    | Deploy
    | Files


codeGenPreviewStr =
    "code-gen-preview"


matchEditorMode : String -> EditorMode
matchEditorMode stringyMode =
    case stringyMode of
        "canvas" ->
            Canvas

        "api" ->
            Api

        "design-system" ->
            DesignSystem

        "model-editor" ->
            Model

        "code-editor" ->
            Code

        "deploy" ->
            Deploy

        "files" ->
            Files

        _ ->
            Canvas


encodeEditorMode : EditorMode -> String
encodeEditorMode mode =
    case mode of
        Canvas ->
            "canvas"

        Api ->
            "api"

        DesignSystem ->
            "design-system"

        Model ->
            "model-editor"

        Code ->
            "code-editor"

        Deploy ->
            "deploy"

        Files ->
            "files"


makeUrl projectMeta projectMode =
    case projectMode of
        Editor Canvas ->
            -- canvas is the default route
            Url.Builder.absolute (encodeProjectMeta projectMeta) []

        Editor editorMode ->
            Url.Builder.absolute (encodeProjectMeta projectMeta ++ [ encodeEditorMode editorMode ]) []

        Preview slug ->
            Url.Builder.absolute (encodeProjectMeta projectMeta ++ [ "preview", Slug.toString slug ]) []

        CodeGenPreview _ ->
            Url.Builder.absolute (encodeProjectMeta projectMeta ++ [ "code-gen", "" ]) []


encodeProjectMeta : Persistence.ProjectMeta -> List String
encodeProjectMeta { projectId, projectName } =
    [ projectId
    , projectName
    ]


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Ui (Url.Parser.s "ui")
        , Url.Parser.map ResponsifyTestingEnvironment (Url.Parser.s "responsify-tests")
        , projectParser |> Url.Parser.map (\meta -> Project meta (Editor Canvas))
        , (projectParser </> Url.Parser.string)
            |> Url.Parser.map
                (\projectMeta subPath ->
                    matchEditorMode subPath
                        |> Editor
                        |> Project projectMeta
                )
        , (projectParser </> Url.Parser.string </> slugParser)
            |> Url.Parser.map
                (\projectMeta subPath slug ->
                    Project projectMeta <|
                        case subPath of
                            "preview" ->
                                Preview slug

                            _ ->
                                Editor Canvas
                )
        , (projectParser </> Url.Parser.string </> idParser)
            |> Url.Parser.map
                (\projectMeta subPath elementId ->
                    Project projectMeta <|
                        case subPath of
                            "code-gen" ->
                                CodeGenPreview elementId

                            _ ->
                                Editor Canvas
                )
        ]


slugParser =
    Url.Parser.custom "SLUG" Slug.parse


projectParser : Url.Parser.Parser (Persistence.ProjectMeta -> a) a
projectParser =
    Url.Parser.map
        (\projectId projectName -> { projectId = projectId, projectName = projectName })
        (projectIdParser </> Url.Parser.string)


idParser : Url.Parser.Parser (Spec.Element.Id.Id -> a) a
idParser =
    Url.Parser.custom "ELEMENT_ID" String.toInt
        |> Url.Parser.map Spec.Element.Id.idFromInt


projectIdParser : Url.Parser.Parser (String -> a) a
projectIdParser =
    Url.Parser.custom "PROJECT_ID"
        (\id ->
            case String.length id of
                32 ->
                    Just id

                _ ->
                    Nothing
        )


parse : Url.Url -> Route
parse =
    Url.Parser.parse routeParser
        >> Maybe.withDefault Home


getProjectData : Url.Url -> Maybe Persistence.ProjectMeta
getProjectData url =
    case parse url of
        Project p _ ->
            Just p

        _ ->
            Nothing
