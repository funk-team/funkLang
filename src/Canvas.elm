port module Canvas exposing (init, rootUpdate, subscriptions, update)

{-| The infinite canavs (or infinite scene) provides an environment for the user
to work with screens in their application in a visual way.

A camera enables zooming and panning over all the (user-created) content.
src: <https://stackblitz.com/edit/multi-touch-trackpad-gesture?file=index.js>

Tools provide means to create and modify the elements on the screen.

@@TODO The editable overlay is a thing we can just wrap in a tool

Each tool can

  - enhance elements
  - place things on the canvas (can be panned over)
  - can place things absolutely as HUD
      - elements on the HUD should NEVER be derived by pannable elements because this
        will cause glitches. An element could be panned while the new position relative to the scene is not read yet.
  - upstream mutations
      - tree / layout
      - style
      - data connection

The infinite canvas provides mouse events.
@@TODO: disable all mouse events and explicitly enable them?
Reason:

  - the number of pointerEventsNone for pass-through elements is high
  - enabling pointer events can go with the functions that also attach event listeners

-}

import ApiExplorer
import ApiExplorer.Msg
import Authentication
import Browser
import Browser.Navigation
import Canvas.AttributesPanel.Content
import Canvas.AttributesPanel.Tabs
import Canvas.Camera
import Canvas.Camera.Move
import Canvas.Msg
import Canvas.Selection
import Canvas.Tool
import Canvas.Tool.Model
import Canvas.Tool.Responsify
import Canvas.Tool.Responsify.Fixtures
import Canvas.Tool.Transform
import Canvas.Tool.Transform.Model
import Clipboard
import Clipboard.Msg
import CodeEditor
import CodeEditor.Msg
import DeployEditor
import DeployEditor.Deploy
import DeployEditor.Msg
import DesignSystem
import DesignSystem.Color
import FileSystem
import Json.Decode as Decode
import Keyboard
import List.Extra
import Model
import Model.Model
import ModelEditor
import Persistence
import Persistence.Repository
import Preview
import Preview.Msg
import Random
import RemoteData
import Route
import Runtime
import Shortcut
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Model
import Spec.Mutation
import SupportSection
import Ui.ColorPicker.Advanced
import Ui.ColorPicker.Gradient
import UndoList
import Url


port windowBlurred : (() -> msg) -> Sub msg


initModel : Model.Model.Flags -> Url.Url -> Browser.Navigation.Key -> Model.Model.Model
initModel flags url key =
    -- Persistence.loadModel Model.decodeSavedState flags.savedSpec
    Model.initModel flags url key Nothing
        |> setSeed flags


setSeed flags m =
    { m | seed = Random.initialSeed flags.seed }


positionCameraCmd model =
    let
        userModel =
            Model.latest model
    in
    Canvas.Camera.Move.englobingCameraCmd
        userModel.itemsOnCanvas


init flags url key =
    let
        model =
            initModel flags url key

        userModel =
            Model.latest model

        cmd =
            Cmd.batch
                [ designSystemCmd
                , CodeEditor.init userModel.codeEditor
                ]

        designSystemCmd =
            DesignSystem.init ()
                |> Tuple.second
                |> Cmd.map (Canvas.Msg.DesignSystemMsg >> Canvas.Msg.EditorMsg)
    in
    ( model, cmd )


port shiftUp : (() -> msg) -> Sub msg


port metaUp : (() -> msg) -> Sub msg


subscriptions : Model.Model.Model -> Sub Canvas.Msg.RootMsg
subscriptions model =
    let
        -- when the user does a key combination of the system
        -- like cmd+shift+5 to record the screen
        -- the modifier keys get stuck
        unstickKeysSub =
            [ case List.any ((==) Keyboard.Meta) model.pressedKeys || List.any ((==) Keyboard.Control) model.pressedKeys of
                True ->
                    metaUp (always Canvas.Msg.ClearMeta) |> Just

                False ->
                    Nothing
            , case List.any ((==) Keyboard.Shift) model.pressedKeys of
                True ->
                    shiftUp (always Canvas.Msg.ClearShift) |> Just

                False ->
                    Nothing
            ]
                |> List.filterMap identity
                |> Sub.batch
    in
    Sub.batch
        [ unstickKeysSub
        , Persistence.Repository.projectSavedSub model
        , windowBlurred (always Canvas.Msg.WindowBlurred)
        , Keyboard.subscriptions
            |> Sub.map Canvas.Msg.KeyMsg
        , Persistence.specUpdated Canvas.Msg.SpecUpdated
            |> Sub.map Canvas.Msg.EditorMsg
        , FileSystem.subs
            |> Sub.map Canvas.Msg.FileSystemMsg
        , SupportSection.subscriptions
            |> Sub.map Canvas.Msg.SupportSection
        , DeployEditor.Deploy.subscriptions
            |> Sub.map (DeployEditor.Msg.GotDeployEditorMsg >> Canvas.Msg.DeployEditorMsg >> Canvas.Msg.EditorMsg)
        , CodeEditor.subscriptions
            |> Sub.map (Canvas.Msg.CodeEditorMsg >> Canvas.Msg.EditorMsg)
        , Clipboard.subscriptions
            |> Sub.map Canvas.Msg.ClipboardMsg
        ]


{-| Save when the model editing state is not pending anymore
-}
persistenceHook :
    Bool
    -> ( Model.Model.Model, Cmd msg )
    -> ( Model.Model.Model, Cmd msg )
persistenceHook shouldSave ( model, cmd ) =
    case shouldSave of
        True ->
            let
                ( newModel, saveCmd ) =
                    case ( Route.getProjectData model.url, model.project ) of
                        ( Just projectMeta, RemoteData.Success p ) ->
                            let
                                saveCmd_ =
                                    Persistence.saveModel
                                        Model.encodeSavedState
                                        projectMeta
                                        { project = p }

                                newModel_ =
                                    Model.mapPending (\state -> { state | specSaved = False }) model
                            in
                            ( newModel_, saveCmd_ )

                        _ ->
                            ( model, Cmd.none )
            in
            ( newModel
            , Cmd.batch [ cmd, saveCmd ]
            )

        False ->
            ( model, cmd )


{-| The undo history should only commit things that are 'completed'.
There is no point in making an undo step for every pixel moved in a drag&drop resize operation.

Neither should Undo & Redo be tracked.

-}
type ResultType
    = Pending
    | Done
    | Undo
    | Redo


{-| Our update function returns the userModel, the 'spec' and some intel for the goblins that run this operation.
-}
type alias Return =
    { userModel : Model.Model.UserModel
    , resultType : ResultType
    , seed : Random.Seed
    , cmd : Cmd Canvas.Msg.Msg
    }


scrollingSpeed =
    50


rootUpdate :
    Canvas.Msg.RootMsg
    -> Model.Model.Model
    -> ( Model.Model.Model, Cmd Canvas.Msg.RootMsg )
rootUpdate msg model =
    let
        init_ model_ =
            let
                m =
                    model_
                        |> Model.mapPending Model.initRuntimeModel
            in
            ( m, m |> positionCameraCmd |> Cmd.map Canvas.Msg.EditorMsg )
    in
    case msg of
        Canvas.Msg.ClearMeta ->
            ( { model
                | pressedKeys =
                    model.pressedKeys
                        |> List.filter ((/=) Keyboard.Control)
                        |> List.filter ((/=) Keyboard.Meta)
              }
            , Cmd.none
            )

        Canvas.Msg.ClearShift ->
            ( { model
                | pressedKeys =
                    List.filter
                        ((/=) Keyboard.Shift)
                        model.pressedKeys
              }
            , Cmd.none
            )

        Canvas.Msg.RepoInitialized Nothing ->
            init_ { model | project = RemoteData.Success Model.defaultUndoableUserModel }

        Canvas.Msg.RepoInitialized (Just encodedSpec) ->
            case Persistence.loadModel Spec.Model.decodeSpec encodedSpec of
                Err e ->
                    init_
                        { model
                            | project = RemoteData.Failure (Decode.errorToString e)
                        }

                Ok spec ->
                    { model
                        | project =
                            RemoteData.Success
                                { timeline = UndoList.fresh (Model.initUserModel spec)
                                , pending = Nothing
                                }
                    }
                        |> init_

        Canvas.Msg.RepoPushed _ ->
            ( Model.mapPending (\state -> { state | specSaved = True }) model
            , Cmd.none
            )

        Canvas.Msg.ClipboardMsg clipboardMsg ->
            let
                ( model_, cmd ) =
                    Clipboard.update clipboardMsg model

                cmds =
                    Cmd.map Canvas.Msg.ClipboardMsg cmd
            in
            case clipboardMsg of
                Clipboard.Msg.NoOp ->
                    ( model_, cmds )

                _ ->
                    let
                        afterUndoHook =
                            Model.mapUndoable
                                (\undoable -> { undoable | pending = Nothing, timeline = UndoList.new (Model.latest model_) undoable.timeline })
                                model_
                    in
                    ( afterUndoHook, cmds )
                        |> persistenceHook True

        Canvas.Msg.EditorMsg m ->
            update m model
                |> Tuple.mapSecond (Cmd.map Canvas.Msg.EditorMsg)

        Canvas.Msg.SupportSection msg_ ->
            let
                ( socialSection, cmd ) =
                    SupportSection.update msg_ model.socialSection
            in
            ( { model | socialSection = socialSection }, Cmd.map Canvas.Msg.SupportSection cmd )

        Canvas.Msg.FileSystemMsg fsMsg ->
            FileSystem.update fsMsg model
                |> Tuple.mapSecond (Cmd.map Canvas.Msg.FileSystemMsg)

        Canvas.Msg.WriteStylesButtonClicked ->
            ( model
            , Canvas.Tool.Responsify.Fixtures.write
                Canvas.Msg.StylesWritten
                (Model.latest model)
            )

        Canvas.Msg.StylesWritten _ ->
            ( model, Cmd.none )

        Canvas.Msg.AuthStateChanged auth ->
            ( { model | authentication = auth }, Cmd.none )

        Canvas.Msg.LogoutRequested ->
            ( model, Authentication.logout () )

        Canvas.Msg.WindowBlurred ->
            ( { model | pressedKeys = [] }, Cmd.none )

        Canvas.Msg.InvestorDemoMsg msg_ ->
            ( model, Cmd.none )

        -- if we get an editor msg for our keyboard msg we apply it
        Canvas.Msg.KeyMsg { keyMsg, tagName, contentEditable } ->
            let
                ( pressedKeysAfterShortcut, editorMsg ) =
                    case ( tagName, contentEditable ) of
                        ( "INPUT", _ ) ->
                            ( Keyboard.update keyMsg model.pressedKeys, Nothing )

                        ( "TEXTAREA", _ ) ->
                            ( Keyboard.update keyMsg model.pressedKeys, Nothing )

                        ( _, Just "true" ) ->
                            ( Keyboard.update keyMsg model.pressedKeys, Nothing )

                        _ ->
                            Shortcut.update model.pressedKeys keyMsg

                model_ =
                    { model | pressedKeys = pressedKeysAfterShortcut }
            in
            case editorMsg of
                Nothing ->
                    ( model_, Cmd.none )

                Just msg_ ->
                    rootUpdate msg_ model_

        Canvas.Msg.PreviewMsg previewMsg ->
            let
                previewUpdate =
                    Preview.update previewMsg model.url model.scrollTo

                { cmd, scrollTo } =
                    Model.latest model
                        |> previewUpdate

                model_ =
                    Model.mapPending (previewUpdate >> .userModel) model
            in
            ( { model_ | scrollTo = scrollTo }
            , cmd |> Cmd.map Canvas.Msg.PreviewMsg
            )

        Canvas.Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Route.parse url of
                        Route.Project _ (Route.Preview _) ->
                            ( model
                            , Runtime.handleInternalUrl
                                { url = url
                                , key = model.key
                                , msg = Preview.Msg.ScrollToMsg >> Canvas.Msg.PreviewMsg
                                , scrollTo = model.scrollTo
                                }
                            )

                        _ ->
                            ( model
                            , Browser.Navigation.pushUrl model.key (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        Canvas.Msg.UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        Canvas.Msg.RunCmd cmd ->
            ( model, cmd )


toggle : Spec.Element.Id.Id -> List Spec.Element.Id.Id -> List Spec.Element.Id.Id
toggle id list =
    if List.member id list then
        List.Extra.remove id list

    else
        id :: list


selectionHook : Model.Model.UserModel -> Return -> Return
selectionHook { selection } return =
    if selection == return.userModel.selection then
        return

    else
        let
            userModel =
                return.userModel

            updatedUserModel =
                { userModel | attributesPanelContentTab = attributesPanelContentTab }

            attributesPanelContentTab =
                case return.userModel.selection of
                    Just selection_ ->
                        Spec.Element.Id.getFromDict (Canvas.Selection.getTargetId selection_) userModel.dataConnections
                            |> Maybe.andThen Canvas.AttributesPanel.Content.dataConnectionTabEquivalent
                            |> Maybe.withDefault return.userModel.attributesPanelContentTab

                    Nothing ->
                        return.userModel.attributesPanelContentTab
        in
        { return | userModel = updatedUserModel }


update :
    Canvas.Msg.Msg
    -> Model.Model.Model
    -> ( Model.Model.Model, Cmd Canvas.Msg.Msg )
update msg model =
    let
        userModel =
            Model.latest model

        return : Return
        return =
            makeReturn msg model

        -- @@TODO - what should be tracked by the API editor
        saveSeed : Random.Seed -> Model.Model.Model -> Model.Model.Model
        saveSeed s m =
            { m | seed = s }

        addCmd : Cmd Canvas.Msg.Msg -> Model.Model.Model -> ( Model.Model.Model, Cmd Canvas.Msg.Msg )
        addCmd cmd m =
            ( m, cmd )

        withUpdatedHistory : Model.Model.UndoableUserModel -> Model.Model.UndoableUserModel
        withUpdatedHistory undoable =
            case return.resultType of
                Pending ->
                    { undoable | pending = Just return.userModel }

                Done ->
                    { undoable | pending = Nothing, timeline = UndoList.new return.userModel undoable.timeline }

                Undo ->
                    { undoable
                        | pending = Nothing
                        , timeline = UndoList.undo undoable.timeline
                    }

                Redo ->
                    { undoable | pending = Nothing, timeline = UndoList.redo undoable.timeline }
    in
    Model.mapUndoable
        withUpdatedHistory
        model
        |> saveSeed return.seed
        |> addCmd return.cmd
        |> persistenceHook (return.resultType /= Pending)


makeReturn msg model =
    let
        userModel =
            Model.latest model
    in
    selectionHook userModel <|
        case msg of
            Canvas.Msg.SpecUpdated encodedSpec ->
                case Persistence.loadModel Model.decodeSavedState encodedSpec of
                    Err _ ->
                        Return
                            userModel
                            Pending
                            model.seed
                            Cmd.none

                    Ok spec ->
                        Return
                            (Model.injectUpdatedSpec spec model |> Model.latest)
                            Done
                            model.seed
                            Cmd.none

            Canvas.Msg.DesignSystemMsg msg_ ->
                let
                    ( designSystem, cmd ) =
                        DesignSystem.update msg_ userModel.designSystem
                in
                Return
                    { userModel | designSystem = designSystem }
                    Done
                    --@TODO could be optimised Pending vs Done
                    -- need a bunch of changes to DesignSystem.update for this to happen
                    model.seed
                    (Cmd.map Canvas.Msg.DesignSystemMsg cmd)

            Canvas.Msg.DeployEditorMsg msg_ ->
                let
                    ( deployEditor, cmd ) =
                        DeployEditor.update
                            msg_
                            model.url
                            userModel.deployEditor
                in
                Return
                    { userModel | deployEditor = deployEditor }
                    Done
                    model.seed
                    (Cmd.map Canvas.Msg.DeployEditorMsg cmd)

            Canvas.Msg.ModelEditorMsg msg_ ->
                let
                    ( modelEditor, cmd ) =
                        ModelEditor.update msg_ userModel.modelEditor

                    newModel =
                        { userModel | modelEditor = modelEditor }
                            |> Model.initRuntimeModel
                in
                Return
                    newModel
                    Done
                    model.seed
                    (Cmd.map Canvas.Msg.ModelEditorMsg cmd)

            Canvas.Msg.ToggleExpand id ->
                Return
                    { userModel
                        | expanded = toggle id userModel.expanded
                    }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.StartLabelEdit id ->
                Return
                    { userModel
                        | editingLabelOn = Just id
                    }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.FinishLabelEdit ->
                Return
                    { userModel
                        | editingLabelOn = Nothing
                    }
                    Done
                    model.seed
                    Cmd.none

            Canvas.Msg.SetLabel selection newLabel ->
                let
                    mutation =
                        Spec.Mutation.SetLabel selection newLabel

                    model_ =
                        Spec.Mutation.apply
                            mutation
                            userModel
                in
                Return
                    { model_
                        | selection = Just selection
                    }
                    Done
                    model.seed
                    Cmd.none

            Canvas.Msg.Select selection ->
                Return
                    { userModel
                        | selection = selection
                    }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.ClearButtonClicked ->
                Return { userModel | itemsOnCanvas = [] }
                    Done
                    model.seed
                    Cmd.none

            Canvas.Msg.DeleteKeyPressed ->
                processDeleteKeyPressed model userModel

            Canvas.Msg.CameraMoved mouseEvent ->
                Return
                    { userModel
                        | camera = Canvas.Camera.updateCamera model.pressedKeys mouseEvent userModel.camera
                    }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.UpdateCamera newCamera ->
                Return { userModel | camera = newCamera }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.ResetCamera level ->
                Return userModel
                    Pending
                    model.seed
                    (positionCameraCmd model)

            Canvas.Msg.ToolMsg msg_ ->
                let
                    -- get the tool result as of the current moment
                    { tool, mutation, seed, selection } =
                        Canvas.Tool.update
                            model
                            userModel
                            model.seed
                            (Model.latest model).camera
                            userModel.selection
                            msg_
                            userModel.tool

                    toolAfterAutoSwitch =
                        case ( tool, mutation ) of
                            ( Canvas.Tool.Model.Draw { sticky }, Just _ ) ->
                                if sticky then
                                    tool

                                else
                                    Canvas.Tool.Model.Transform Canvas.Tool.Transform.Model.NotTransforming

                            _ ->
                                tool

                    -- if the tool emitted a mutation, commit it
                    ( userModel_, isPending_ ) =
                        case mutation of
                            Nothing ->
                                ( userModel, Pending )

                            Just m ->
                                ( Spec.Mutation.apply m userModel, Done )

                    -- if the present tool is the transform tool, don't update the previous tool
                    previousTool_ =
                        case tool of
                            Canvas.Tool.Model.Transform _ ->
                                userModel.previousTool

                            _ ->
                                userModel.tool
                in
                Return
                    { userModel_
                        | tool = toolAfterAutoSwitch
                        , selection = selection
                        , previousTool = previousTool_
                    }
                    isPending_
                    seed
                    Cmd.none

            Canvas.Msg.ChangeTool tool ->
                Return
                    { userModel
                        | tool = tool
                        , previousTool = userModel.tool
                        , drawingDropDownVisibility = False
                    }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.ClickToolDropDown ->
                Return { userModel | drawingDropDownVisibility = not userModel.drawingDropDownVisibility }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.ChangeHudVisibility newHudVisibility ->
                Return { userModel | hudVisibility = newHudVisibility }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.UndoClicked ->
                Return userModel Undo model.seed Cmd.none

            Canvas.Msg.RedoClicked ->
                Return userModel Redo model.seed Cmd.none

            Canvas.Msg.ApiExplorerMsg msg_ ->
                let
                    trackResult =
                        Done

                    ( apiExplorer, cmd ) =
                        ApiExplorer.update msg_ userModel.apiExplorer

                    userModel__ : Model.Model.UserModel
                    userModel__ =
                        { userModel
                            | apiExplorer = apiExplorer
                        }
                            |> (\userModel_ -> { userModel_ | codeEditor = CodeEditor.update CodeEditor.Msg.PropagateChangesFromApiEditor userModel_ |> Tuple.first })
                in
                Return
                    userModel__
                    trackResult
                    model.seed
                    (Cmd.map Canvas.Msg.ApiExplorerMsg cmd)

            Canvas.Msg.CodeEditorMsg msg_ ->
                let
                    ( userModel_, msg__ ) =
                        case msg_ of
                            CodeEditor.Msg.RenameApi id name ->
                                ( ApiExplorer.update
                                    (ApiExplorer.Msg.SetApiSpecName id name)
                                    userModel.apiExplorer
                                    |> (\( apiExplorer, _ ) -> { userModel | apiExplorer = apiExplorer })
                                , CodeEditor.Msg.PropagateChangesFromApiEditor
                                )

                            _ ->
                                ( userModel, msg_ )

                    ( codeEditor, cmd ) =
                        CodeEditor.update msg__ userModel_
                in
                Return
                    { userModel_ | codeEditor = codeEditor }
                    Done
                    model.seed
                    (Cmd.map Canvas.Msg.CodeEditorMsg cmd)

            Canvas.Msg.AttributesPanelTabChanged tab ->
                Return
                    { userModel | attributesPanelTab = tab }
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.AttributesPanelContentSubTabChanged tab ->
                Return
                    { userModel | attributesPanelContentTab = tab }
                    Pending
                    model.seed
                    Cmd.none

            -- DATA OPERATIONS
            Canvas.Msg.MakeDataConnection elementId connection ->
                Return
                    { userModel
                        | dataConnections =
                            Spec.Element.Id.insertIntoDict
                                elementId
                                connection
                                userModel.dataConnections
                    }
                    Done
                    model.seed
                    Cmd.none

            Canvas.Msg.RemoveDataConnection elementId ->
                Return
                    { userModel | dataConnections = Spec.Element.Id.removeFromDict elementId userModel.dataConnections }
                    Done
                    model.seed
                    Cmd.none

            Canvas.Msg.GotMutation mutation ->
                Return
                    (Spec.Mutation.apply mutation userModel)
                    Done
                    model.seed
                    Cmd.none

            -- STYLE OPERATIONS
            Canvas.Msg.SetStyleOverrides id styles swatchUpdate ->
                Return
                    ({ userModel
                        | designSystem =
                            case swatchUpdate of
                                Nothing ->
                                    userModel.designSystem

                                Just swatchUpdate_ ->
                                    let
                                        designSystem =
                                            userModel.designSystem
                                    in
                                    { designSystem
                                        | colorEditor =
                                            DesignSystem.Color.updateSwatch swatchUpdate_ userModel.designSystem.colorEditor
                                    }
                     }
                        |> Spec.Mutation.setStyles id (always styles)
                    )
                    Done
                    model.seed
                    Cmd.none

            -- STYLE OPERATIONS
            Canvas.Msg.SetImageCropMode id imageCropMode ->
                Return
                    (userModel
                        |> Spec.Mutation.setStyles id
                            (\styles_ ->
                                { styles_
                                    | imageCropMode = imageCropMode
                                }
                            )
                    )
                    Done
                    model.seed
                    Cmd.none

            Canvas.Msg.ResponsifyToolMsg eMsg ->
                let
                    { newState, mutation, newSeed, newSelection, cmd } =
                        Canvas.Tool.Responsify.update
                            userModel
                            { selection = userModel.selection
                            , camera = userModel.camera
                            , msg = eMsg
                            , state = userModel.evolveState
                            , seed = model.seed
                            }

                    -- if the tool emitted a mutation, commit it
                    ( userModel_, isPending_ ) =
                        case mutation of
                            Nothing ->
                                ( userModel, Pending )

                            Just m ->
                                ( Spec.Mutation.apply m userModel, Done )
                in
                Return
                    { userModel_
                        | evolveState = newState
                        , selection = newSelection
                    }
                    isPending_
                    newSeed
                    (Cmd.map Canvas.Msg.ResponsifyToolMsg cmd)

            -- CODE OPERATIONS
            Canvas.Msg.SetCode _ _ ->
                Return
                    userModel
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.NoOp ->
                Return
                    userModel
                    Pending
                    model.seed
                    Cmd.none

            Canvas.Msg.ResponsifySidebarBtnClicked ( selectionItem, element ) layoutData ->
                let
                    eMsg =
                        Canvas.Tool.Responsify.SmartLayoutButtonClicked ( selectionItem, element ) layoutData

                    { newState, mutation, newSeed, newSelection } =
                        Canvas.Tool.Responsify.update
                            userModel
                            { selection = userModel.selection
                            , camera = userModel.camera
                            , msg = eMsg
                            , state = userModel.evolveState
                            , seed = model.seed
                            }

                    -- if the tool emitted a mutation, commit it
                    ( userModel_, isPending_ ) =
                        case mutation of
                            Nothing ->
                                ( userModel, Pending )

                            Just m ->
                                ( Spec.Mutation.apply m userModel, Done )
                in
                Return
                    { userModel_
                        | evolveState = newState
                        , selection = newSelection
                        , attributesPanelTab = Canvas.AttributesPanel.Tabs.Layout
                    }
                    isPending_
                    newSeed
                    Cmd.none

            -- User-Defined actions
            Canvas.Msg.SetActions elementId action ->
                Return
                    { userModel
                        | actions =
                            userModel.actions
                                |> Spec.Element.Id.insertIntoDict
                                    elementId
                                    action
                    }
                    Done
                    model.seed
                    Cmd.none


processDeleteKeyPressed : Model.Model.Model -> Model.Model.UserModel -> Return
processDeleteKeyPressed model userModel =
    case userModel.selection of
        Nothing ->
            Return userModel
                Pending
                model.seed
                Cmd.none

        Just selection ->
            let
                id =
                    Canvas.Selection.getTargetId selection

                maybeStyle =
                    Spec.Element.Id.getFromDict
                        id
                        userModel.elementStyles

                deleteElementMutation =
                    let
                        mutation =
                            Spec.Mutation.Delete selection

                        model_ =
                            Spec.Mutation.apply
                                mutation
                                { userModel | selection = Canvas.Selection.removeOne selection }
                    in
                    Return model_
                        Done
                        model.seed
                        Cmd.none
            in
            -- if the background color picker is selected, we bind the keyboard shortcuts to deleting swatches
            case ( maybeStyle, Maybe.andThen .background maybeStyle ) of
                ( Just overrides, Just backgroundColor ) ->
                    case backgroundColor of
                        Ui.ColorPicker.Gradient.GradientBackground gradientState ->
                            case gradientState.visibility of
                                Ui.ColorPicker.Advanced.Open _ ->
                                    makeReturn
                                        (Canvas.Msg.SetStyleOverrides
                                            id
                                            { overrides
                                                | background =
                                                    gradientState
                                                        |> Ui.ColorPicker.Gradient.removeColorPoint
                                                        |> Ui.ColorPicker.Gradient.GradientBackground
                                                        |> Just
                                            }
                                            Nothing
                                        )
                                        model

                                Ui.ColorPicker.Advanced.Closed ->
                                    deleteElementMutation

                        _ ->
                            deleteElementMutation

                _ ->
                    deleteElementMutation
