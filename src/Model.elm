module Model exposing (..)

import Action
import ApiExplorer
import ApiExplorer.Api
import ApiExplorer.Model
import Authentication
import Browser.Navigation
import Canvas.AttributesPanel.Content.Tabs
import Canvas.AttributesPanel.Tabs
import Canvas.Camera
import Canvas.Selection
import Canvas.Tool.Model
import Canvas.Tool.Responsify.Model
import Clipboard.Model
import CodeEditor.Model
import DeployEditor
import DesignSystem
import Dict
import Dict.Any
import Dict.Extra
import FileSystem.CliConnection
import Google.Fonts
import Interface
import Interface.JsonTree.Model
import Interface.Selection
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Model.Model exposing (..)
import ModelEditor.Model
import Preview.Model
import Projects
import Random
import RemoteData
import ResponsifyTestingEnvironment.Model
import Route
import ScrollTo
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Model
import SupportSection
import Time
import UndoList
import Url



-- API / DATA STUFF


isSelected : UserModel -> Spec.Element.Model.Element a -> Bool
isSelected userModel element =
    Canvas.Selection.isSelectedElementId userModel.selection element.shared.id


mapPending : (UserModel -> UserModel) -> Model -> Model
mapPending fn model =
    mapUndoable
        (\m ->
            { m
                | pending = Just <| fn (m.pending |> Maybe.withDefault m.timeline.present)
            }
        )
        model


mapUndoable : (UndoableUserModel -> UndoableUserModel) -> Model -> Model
mapUndoable fn model =
    { model | project = RemoteData.map fn model.project }


latest : Model -> UserModel
latest model =
    let
        project =
            model.project
                |> RemoteData.toMaybe
                |> Maybe.withDefault defaultUndoableUserModel
    in
    project
        |> .pending
        |> Maybe.withDefault project.timeline.present


encodeSeed _ =
    Encode.null


initModel :
    Flags
    -> Url.Url
    -> Browser.Navigation.Key
    -> Maybe SavedState
    -> Model
initModel flags url key savedState =
    let
        internals : WithInternals {}
        internals =
            { seed = Random.initialSeed flags.seed
            , mode =
                case flags.mode of
                    "enterprise" ->
                        Model.Model.Enterprise

                    "core" ->
                        Model.Model.Core

                    _ ->
                        Model.Model.Core
            , dom = flags.dom
            , responsifyTests = ResponsifyTestingEnvironment.Model.init
            , clipboard = Clipboard.Model.init flags.storage
            , socialSection = SupportSection.init
            , scrollTo = ScrollTo.init
            , projects = Projects.initModel
            , key = key
            , url = url
            , pressedKeys = []
            , fs = []
            , authentication = Authentication.Undetermined
            , cliConnection = FileSystem.CliConnection.init
            , now = Time.millisToPosix flags.now
            , googleFonts = Google.Fonts.initFonts
            }
    in
    mapPending initRuntimeModel <|
        case savedState of
            Just loaded ->
                withInternals loaded internals

            Nothing ->
                withInternals defaultSavedState internals


initRuntimeModel : UserModel -> UserModel
initRuntimeModel =
    \userModel ->
        { userModel
            | runtimeModel =
                userModel.modelEditor.fields
                    |> Dict.Extra.filterMap
                        (\key field ->
                            case field.kind of
                                ModelEditor.Model.WithDefault instance ->
                                    Just instance

                                _ ->
                                    Nothing
                        )
        }


defaultSavedState : SavedState
defaultSavedState =
    { project = RemoteData.NotAsked
    }


injectUpdatedSpec : SavedState -> Model -> Model
injectUpdatedSpec savedState oldState =
    let
        project =
            savedState.project
                |> RemoteData.toMaybe
                |> Maybe.withDefault defaultUndoableUserModel

        l =
            latest oldState

        present =
            project.timeline.present

        timeline =
            project.timeline

        project_ =
            { present
                | selection = l.selection
                , camera = l.camera
            }

        timeline_ =
            { timeline | present = project_ }
    in
    { oldState
        | project = RemoteData.Success { project | timeline = timeline_ }
    }


withInternals : SavedState -> WithInternals flags -> Model
withInternals { project } { mode, socialSection, projects, dom, clipboard, cliConnection, seed, url, key, pressedKeys, authentication, fs, scrollTo, responsifyTests, now, googleFonts } =
    { project = project
    , mode = mode
    , dom = dom
    , clipboard = clipboard
    , socialSection = socialSection
    , pressedKeys = pressedKeys
    , seed = seed
    , url = url
    , key = key
    , fs = fs
    , authentication = authentication
    , cliConnection = cliConnection
    , scrollTo = scrollTo
    , projects = projects
    , now = now
    , googleFonts = googleFonts
    , responsifyTests = responsifyTests
    }


defaultUndoableUserModel : UndoableUserModel
defaultUndoableUserModel =
    { timeline = UndoList.fresh (initUserModel freshSpec)
    , pending = Nothing
    }


encodeTimeline : Timeline -> Encode.Value
encodeTimeline =
    .present >> encodeUserModel


decodeTimeline : Decode.Decoder Timeline
decodeTimeline =
    decodeUserModel |> Decode.map UndoList.fresh


decodeUserModel =
    Spec.Model.decodeSpec
        |> Decode.map initUserModel


encodeUserModel : Spec.Model.WithSpec a -> Encode.Value
encodeUserModel { modelEditor, codeEditor, itemsOnCanvas, apiExplorer, actions, elementStyles, dataConnections, designSystem, deployEditor } =
    Spec.Model.encodeSpec
        { itemsOnCanvas = itemsOnCanvas
        , apiExplorer = apiExplorer
        , codeEditor = codeEditor
        , actions = actions
        , elementStyles = elementStyles
        , dataConnections = dataConnections
        , designSystem = designSystem
        , deployEditor = deployEditor
        , modelEditor = modelEditor
        }


freshSpec : Spec.Model.Spec
freshSpec =
    { itemsOnCanvas = defaultScreens
    , modelEditor = ModelEditor.Model.init
    , codeEditor = CodeEditor.Model.init
    , apiExplorer = ApiExplorer.Model.init
    , elementStyles = Spec.Element.Id.emptyDict
    , dataConnections = Spec.Element.Id.emptyDict
    , actions = Spec.Element.Id.emptyDict
    , designSystem = DesignSystem.init () |> Tuple.first
    , deployEditor = DeployEditor.init () |> Tuple.first
    }


defaultScreens : Spec.Element.Model.Screens
defaultScreens =
    []


initUserModel : Spec.Model.Spec -> UserModel
initUserModel { itemsOnCanvas, apiExplorer, elementStyles, dataConnections, actions, designSystem, codeEditor, modelEditor, deployEditor } =
    { itemsOnCanvas = itemsOnCanvas
    , apiExplorer = apiExplorer
    , codeEditor = codeEditor
    , elementStyles = elementStyles
    , dataConnections = dataConnections
    , actions = actions
    , designSystem = designSystem
    , deployEditor = deployEditor
    , modelEditor = modelEditor
    , attributesPanelTab = defaultEditorModel.attributesPanelTab
    , attributesPanelContentTab = defaultEditorModel.attributesPanelContentTab
    , camera = defaultEditorModel.camera
    , editingLabelOn = defaultEditorModel.editingLabelOn
    , selection = defaultEditorModel.selection
    , expanded = defaultEditorModel.expanded
    , tool = defaultEditorModel.tool
    , previousTool = defaultEditorModel.tool
    , hudVisibility = True
    , drawingDropDownVisibility = False
    , evolveState = defaultEditorModel.evolveState
    , runtimeModel = defaultEditorModel.runtimeModel
    , specSaved = defaultEditorModel.specSaved
    , responsifyTests = ResponsifyTestingEnvironment.Model.init
    }


{-|

    import Canvas.AttributesPanel.Tabs
    import Canvas.AttributesPanel.Content.Tabs
    import Model.Model

    defaultEditorModel.attributesPanelTab
    --> Canvas.AttributesPanel.Tabs.Layout

    defaultEditorModel.attributesPanelContentTab
    --> Canvas.AttributesPanel.Content.Tabs.Text

-}
defaultEditorModel : EditorModel {}
defaultEditorModel =
    { attributesPanelTab = Canvas.AttributesPanel.Tabs.Layout
    , responsifyTests = ResponsifyTestingEnvironment.Model.init
    , attributesPanelContentTab = Canvas.AttributesPanel.Content.Tabs.Text
    , camera = Canvas.Camera.defaultCamera
    , editingLabelOn = Nothing
    , selection = Nothing
    , expanded = []
    , tool = Canvas.Tool.Model.init
    , previousTool = Canvas.Tool.Model.init
    , hudVisibility = True
    , drawingDropDownVisibility = False
    , evolveState = Canvas.Tool.Responsify.Model.init
    , runtimeModel = Preview.Model.init
    , specSaved = True
    }


encodeKeys _ =
    Encode.null


decodeKeys =
    Decode.succeed []


decodeSavedState =
    Decode.map
        (RemoteData.Success >> Model.Model.SavedState)
        decodeUndoableUserModel


encodeSavedState a =
    encodeUndoableUserModel a.project


decodeUndoableUserModel =
    Decode.map
        (UndoableUserModel Nothing)
        decodeTimeline


encodeUndoableUserModel a =
    encodeTimeline a.timeline


modeToTabLabel mode =
    case mode of
        Route.Canvas ->
            "Canvas"

        Route.DesignSystem ->
            "Design"

        Route.Code ->
            "Code"

        Route.Api ->
            "API"

        Route.Model ->
            "Model"

        Route.Deploy ->
            "Deploy"

        Route.Files ->
            "Files"



-- RUNTIME HELPERS


{-| Drill down into request params to find the refined type. For example: E-Mail
-}
getKindForParamTarget :
    ApiExplorer.Model.ApiSpec
    -> ( Interface.JsonTree.Model.KeyPath, Maybe Interface.Selection.InterpolationVariableKey )
    -> Maybe { variable : Maybe ApiExplorer.Api.ParamVariable, refinedType : Interface.Selection.RefinedType }
getKindForParamTarget apiCall ( keyPath, variableKey ) =
    case Dict.Any.get keyPath apiCall.requestBodyParams of
        Just { kind, name } ->
            case ( kind, variableKey ) of
                ( Interface.Selection.Single (Just (Interface.Selection.Text interpolationSettings)), Just key ) ->
                    let
                        allSettings =
                            ApiExplorer.Api.findAllSlots
                                apiCall
                                keyPath
                                interpolationSettings
                                |> List.filterMap
                                    (\( k, settings ) ->
                                        case k of
                                            Nothing ->
                                                Nothing

                                            Just k_ ->
                                                Just ( k_, settings )
                                    )
                    in
                    case List.Extra.find (\( maybeKey, _ ) -> maybeKey == key) allSettings of
                        Just ( foundKey, variableSettings ) ->
                            Just
                                { refinedType = Interface.Selection.Text interpolationSettings
                                , variable =
                                    Just
                                        { refinedType = variableSettings.kind
                                        , name = variableSettings.variableName
                                        , key = foundKey
                                        }
                                }

                        Nothing ->
                            Nothing

                ( Interface.Selection.Single (Just refinedType), _ ) ->
                    Just { refinedType = refinedType, variable = Nothing }

                _ ->
                    Nothing

        _ ->
            Nothing


paramForField key userModel =
    let
        params =
            allApiParams userModel

        thisParam =
            List.Extra.find (\parameter -> parameter.fieldKey == key) params
    in
    thisParam


allApiParams userModel =
    userModel.actions
        |> Spec.Element.Id.dictToList
        |> List.concatMap
            (\( elementId, { onClick } ) ->
                case onClick of
                    Just (Action.MakeApiCall apiCallParams) ->
                        apiCallParams
                            |> collectParams userModel

                    _ ->
                        []
            )


{-| Collect the params for an action in order to validate them or send them to an API
-}
collectParams : Model.Model.UserModel -> Action.ApiCallParams -> ApiExplorer.Api.ApiParams
collectParams userModel callParams =
    let
        help : ApiExplorer.Model.ApiSpec -> ApiExplorer.Api.ApiParams
        help apiCall =
            Dict.Any.toList callParams.modelToCallMap
                |> List.filterMap
                    (\( ( keyPath, variableKey ), modelKey ) ->
                        case ( Dict.get modelKey userModel.runtimeModel, Dict.get modelKey userModel.modelEditor.fields ) of
                            ( Just value, Just fieldSpec ) ->
                                case getKindForParamTarget apiCall ( keyPath, variableKey ) of
                                    Just { variable, refinedType } ->
                                        case refinedType of
                                            Interface.Selection.Text textOptions ->
                                                case ( variable, Interface.Selection.interpolationSettingsToList textOptions ) of
                                                    -- at least one interpolation target for variable
                                                    ( Just _, _ ) ->
                                                        Just
                                                            { keyPath = keyPath
                                                            , variable = variable
                                                            , data = value
                                                            , refinedType = refinedType
                                                            , fieldName = fieldSpec.name
                                                            , fieldKey = modelKey
                                                            }

                                                    -- no interpolation targets but also no variable
                                                    ( Nothing, [] ) ->
                                                        Just
                                                            { keyPath = keyPath
                                                            , variable = variable
                                                            , data = value
                                                            , refinedType = refinedType
                                                            , fieldName = fieldSpec.name
                                                            , fieldKey = modelKey
                                                            }

                                                    ( Nothing, _ ) ->
                                                        Nothing

                                            _ ->
                                                Just
                                                    { keyPath = keyPath
                                                    , variable = variable
                                                    , data = value
                                                    , refinedType = refinedType
                                                    , fieldName = fieldSpec.name
                                                    , fieldKey = modelKey
                                                    }

                                    _ ->
                                        Nothing

                            -- could not find param
                            _ ->
                                Nothing
                    )
    in
    case getCall callParams userModel of
        Nothing ->
            []

        Just apiCall ->
            help apiCall


getCall : Action.ApiCallParams -> Model.Model.UserModel -> Maybe ApiExplorer.Model.ApiSpec
getCall { apiCallKey } userModel =
    case apiCallKey of
        Nothing ->
            Nothing

        Just key ->
            case ApiExplorer.Model.getApiSpec key userModel.apiExplorer of
                Nothing ->
                    Nothing

                Just apiCall ->
                    Just apiCall
