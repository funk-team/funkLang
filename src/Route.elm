module Route exposing (..)

{-| The router has two modes

  - single-project mode for the core functionality of funk
  - multi-project mode for funk with project management

-}

import Model.Product
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


makeUrl productMode projectMeta projectMode =
    let
        metaPath =
            case productMode of
                Model.Product.Core ->
                    []

                Model.Product.Enterprise ->
                    encodeProjectMeta projectMeta
    in
    case projectMode of
        Editor Canvas ->
            -- canvas is the default route
            Url.Builder.absolute metaPath []

        Editor editorMode ->
            Url.Builder.absolute (metaPath ++ [ encodeEditorMode editorMode ]) []

        Preview slug ->
            Url.Builder.absolute (metaPath ++ [ "preview", Slug.toString slug ]) []

        CodeGenPreview _ ->
            Url.Builder.absolute (metaPath ++ [ "code-gen", "" ]) []


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


defaultProjectMeta : Persistence.ProjectMeta
defaultProjectMeta =
    { projectId = "default"
    , projectName = "default project"
    }


singleProjectParser : Url.Parser.Parser (Route -> a) a
singleProjectParser =
    Url.Parser.oneOf
        [ Url.Parser.string
            |> Url.Parser.map
                (\subPath ->
                    matchEditorMode subPath
                        |> Editor
                        |> Project defaultProjectMeta
                )
        , (Url.Parser.string </> slugParser)
            |> Url.Parser.map
                (\subPath slug ->
                    Project defaultProjectMeta <|
                        case subPath of
                            "preview" ->
                                Preview slug

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


parse : Model.Product.Mode -> Url.Url -> Route
parse mode url =
    case mode of
        Model.Product.Core ->
            Url.Parser.parse singleProjectParser url
                |> Maybe.withDefault (Project defaultProjectMeta (Editor Canvas))

        Model.Product.Enterprise ->
            Url.Parser.parse routeParser url
                |> Maybe.withDefault Home


getProjectData : Model.Product.Mode -> Url.Url -> Maybe Persistence.ProjectMeta
getProjectData mode url =
    case parse mode url of
        Project p _ ->
            Just p

        _ ->
            Nothing
