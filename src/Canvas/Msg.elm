module Canvas.Msg exposing (..)

import Action
import ApiExplorer.Msg
import Authentication
import Browser
import Canvas.AttributesPanel.Content.Tabs
import Canvas.AttributesPanel.Tabs
import Canvas.Camera
import Canvas.Camera.Model
import Canvas.Selection
import Canvas.Tool.Model
import Canvas.Tool.Msg
import Canvas.Tool.Responsify
import Canvas.Tool.Responsify.Model
import Clipboard.Msg
import CodeEditor.Msg
import DeployEditor.Msg
import DesignSystem.Msg
import FileSystem
import Http
import InvestorDemo
import Keyboard
import ModelEditor.Msg
import Preview.Msg
import Spec.DataConnection
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Style
import Spec.Mutation
import SupportSection.Msg
import Ui.ColorPicker.Advanced
import Url


type Msg
    = CameraMoved Canvas.Camera.MouseEvent
    | UpdateCamera Canvas.Camera.Model.Model
    | ResetCamera Float
    | ChangeTool Canvas.Tool.Model.Tool
    | ClickToolDropDown
    | ToolMsg Canvas.Tool.Msg.Msg
    | ChangeHudVisibility Bool
    | ClearButtonClicked
    | DeleteKeyPressed
    | UndoClicked
    | RedoClicked
    | ApiExplorerMsg ApiExplorer.Msg.Msg
    | SetLabel Canvas.Selection.SelectionItem String
    | Select (Maybe Canvas.Selection.SelectionItem)
    | ToggleExpand Spec.Element.Id.Id
    | StartLabelEdit Spec.Element.Id.Id
    | FinishLabelEdit
      -- Actions
    | SetActions Spec.Element.Id.Id Action.ActionsForElement
      -- AttributesPanel
    | AttributesPanelTabChanged Canvas.AttributesPanel.Tabs.Tab
    | AttributesPanelContentSubTabChanged Canvas.AttributesPanel.Content.Tabs.ContentTab
      -- DATA
    | MakeDataConnection Spec.Element.Id.Id Spec.DataConnection.DataConnection
    | RemoveDataConnection Spec.Element.Id.Id
      -- STYLING
    | SetStyleOverrides Spec.Element.Id.Id Spec.Element.Style.Style (Maybe Ui.ColorPicker.Advanced.SwatchUpdate)
    | SetImageCropMode Spec.Element.Id.Id Spec.Element.Style.ImageCropMode
      -- DESIGN SYSTEM
    | DesignSystemMsg DesignSystem.Msg.Msg
      -- DEPLOY
    | DeployEditorMsg DeployEditor.Msg.Msg
      -- MODEL EDITOR
    | ModelEditorMsg ModelEditor.Msg.Msg
      -- LAYOUT
    | GotMutation Spec.Mutation.Mutation
    | ResponsifyToolMsg Canvas.Tool.Responsify.Msg
      -- CODE
    | SetCode Spec.Element.Id.Id String
    | CodeEditorMsg CodeEditor.Msg.Msg
    | NoOp
    | SpecUpdated String
    | ResponsifySidebarBtnClicked ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement ) Canvas.Tool.Responsify.Model.LayoutData


type RootMsg
    = WriteStylesButtonClicked
    | RepoInitialized (Maybe String)
    | RepoPushed String
    | StylesWritten (Result Http.Error ())
    | EditorMsg Msg
    | KeyMsg (Keyboard.KeyMsgData Keyboard.Msg)
    | InvestorDemoMsg InvestorDemo.Msg
    | WindowBlurred
      -- Preview
    | PreviewMsg Preview.Msg.Msg
      -- Authentication
    | AuthStateChanged Authentication.State
    | LogoutRequested
      -- CLI and local FS
    | FileSystemMsg FileSystem.Msg
    | SupportSection SupportSection.Msg.Msg
      -- NAVIGATION
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- Cmd
    | RunCmd (Cmd RootMsg)
      -- CLIPBOARD
    | ClipboardMsg Clipboard.Msg.Msg
      -- UNSTICK KEYS
    | ClearShift
    | ClearMeta


type ScenarioManagerMsg
    = AddScenarioButtonClicked
    | ScenarioSelected (Maybe Int)
