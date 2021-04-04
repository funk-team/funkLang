module Canvas.Tool.AugmentationParams exposing (..)

import Canvas.Camera.Model
import Canvas.Selection
import Interface.Model
import Keyboard
import Spec.Element.Model


type alias AugmentationParams =
    { camera : Canvas.Camera.Model.Model
    , selectionItem : Canvas.Selection.SelectionItem
    , selection : Canvas.Selection.Selection
    , element : Spec.Element.Model.EitherElement
    , scope : Interface.Model.ScopeData
    , pressedKeys : List Keyboard.Key
    }
