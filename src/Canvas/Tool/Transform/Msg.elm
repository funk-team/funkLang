module Canvas.Tool.Transform.Msg exposing (Msg(..))

import Canvas.Selection
import Canvas.Tool.Transform.Model


type Msg
    = Select Canvas.Selection.SelectionItem
    | SceneClicked
    | MouseDownOnElement Canvas.Tool.Transform.Model.MouseDownDataOnElement
    | MouseDownOnScreen Canvas.Tool.Transform.Model.MouseDownDataOnScreen
    | MouseDrag Canvas.Tool.Transform.Model.TransformTarget
    | MouseUp
