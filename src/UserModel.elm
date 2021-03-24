module UserModel exposing (..)

import Canvas.AttributesPanel.Content.Tabs
import Canvas.AttributesPanel.Tabs
import Canvas.Camera
import Canvas.Camera.Model
import Canvas.Selection
import Canvas.Tool
import Canvas.Tool.Model
import Canvas.Tool.Responsify.Model
import Preview.Model


type alias UserModel =
    EditorModel Spec.Model.Spec


type alias EditorModel spec =
    { spec
        | attributesPanelTab : Canvas.AttributesPanel.Tabs.Tab
        , attributesPanelContentTab : Canvas.AttributesPanel.Content.Tabs.ContentTab
        , camera : Canvas.Camera.Model.Model
        , expanded : Canvas.Selection.ExpansionSet
        , tool : Canvas.Tool.Model.Tool
        , previousTool : Canvas.Tool.Model.Tool
        , hudVisibility : Bool
        , drawingDropDownVisibility : Bool
        , selection : Canvas.Selection.Selection
        , editingLabelOn : Maybe Spec.Element.Id.Id
        , evolveState : Canvas.Tool.Responsify.Model.State
        , mode : Mode
        , runtimeModel : Preview.Model.Model
    }
