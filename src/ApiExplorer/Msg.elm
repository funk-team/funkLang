module ApiExplorer.Msg exposing (..)

import ApiExplorer.Api.UrlParser
import ApiExplorer.Menus
import ApiExplorer.Model
import Interface.JsonTree.Model
import Interface.Selection


type Msg
    = -- shared
      SelectApiSpec ApiExplorer.Model.ApiCallKey
    | RemoveApiSpec ApiExplorer.Model.ApiCallKey
    | SetApiSpecName ApiExplorer.Model.ApiCallKey String
    | SetApiSpec ApiExplorer.Model.ApiCallKey ApiExplorer.Model.ApiSpec
      -- unspecified
    | AddApiSpec
      -- ApiSpec specific
    | ApiSpecEdit_ ApiExplorer.Model.ApiCallKey ApiSpecEdit
    | SwitchMenu ApiExplorer.Menus.Menu
    | ShowOrHideHelpPanel


type ApiSpecEdit
    = SetRequestMethod ApiExplorer.Model.RequestMethod
    | ClearResponse
    | UrlChanged ApiExplorer.Api.UrlParser.Url
    | MockFileUploaded String
    | GotResponse ApiExplorer.Model.Request
    | UpdateRequestBody String
    | UpdateHeaderKey Int String
    | UpdateHeaderVal Int String
    | RemoveHeaderKeyVal Int
    | AddHeaderKeyVal
    | ParseUrlButtonClicked
    | MakeRequestButtonClicked
      -- tree data operations
    | SetResponseDataSelection Interface.Selection.Selection
    | SetRequestParameterSelection Interface.Selection.Selection
    | SelectData Interface.JsonTree.Model.Selection
    | SetDataTreeViewState Interface.JsonTree.Model.State
    | EditDataSelection Interface.JsonTree.Model.KeyPathList EditOperation
      -- CURL
    | UpdateCurl ApiExplorer.Model.CurlModel
    | ApplyCurlButtonClicked ApiExplorer.Model.CurlParseResult


type EditOperation
    = Deselect
    | SetName String
    | SetRefinedType Interface.Selection.RefinedType
