module Model.Model exposing (..)

import Authentication
import Browser.Navigation
import Canvas.AttributesPanel.Content.Tabs
import Canvas.AttributesPanel.Tabs
import Canvas.Camera.Model
import Canvas.Selection
import Canvas.Tool.Model
import Canvas.Tool.Responsify.Model
import Clipboard.Model
import Dom
import FileSystem.CliConnection
import FileSystem.Model
import Google.Fonts
import Json.Decode as Decode
import Keyboard
import Model.Product
import Preview.Model
import Projects.Model
import Random
import RemoteData
import ResponsifyTestingEnvironment.Model
import ScrollTo
import Spec.Element.Id
import Spec.Model
import SupportSection.Model
import Time
import UndoList
import Url


type alias Flags =
    { storage : Decode.Value
    , dom : Dom.Dom
    , seed : Int
    , now : Int
    , mode : String
    }


type alias Model =
    WithInternals SavedState



-- WithInternals


type alias WithInternals model =
    { model
        | seed : Random.Seed
        , mode : Model.Product.Mode
        , dom : Dom.Dom
        , url : Url.Url
        , key : Browser.Navigation.Key
        , pressedKeys : Keys
        , authentication : Authentication.State
        , cliConnection : FileSystem.CliConnection.State
        , fs : FileSystem.Model.Files
        , socialSection : SupportSection.Model.Model
        , scrollTo : ScrollTo.State
        , clipboard : Clipboard.Model.Model
        , projects : Projects.Model.Model
        , now : Time.Posix
        , googleFonts : Google.Fonts.Fonts
        , responsifyTests : ResponsifyTestingEnvironment.Model.Model
    }


type alias Keys =
    List Keyboard.Key



-- SavedState


type alias SavedState =
    { project : RemoteData.RemoteData String UndoableUserModel
    }


type alias UndoableUserModel =
    { pending : Maybe UserModel
    , timeline : Timeline
    }


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
        , responsifyTests : ResponsifyTestingEnvironment.Model.Model
        , editingLabelOn : Maybe Spec.Element.Id.Id
        , evolveState : Canvas.Tool.Responsify.Model.State
        , runtimeModel : Preview.Model.Model
        , specSaved : Bool
    }


type alias Timeline =
    UndoList.UndoList UserModel
