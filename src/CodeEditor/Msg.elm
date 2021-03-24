module CodeEditor.Msg exposing (..)

import ApiExplorer.Model
import CodeEditor.Model


type Msg
    = CodeMsg CodeEditor.Model.TransformationKey ( CodeMsgDetail, CodeEditor.Model.Transformation )
    | AddCode
    | RemoveCode CodeEditor.Model.TransformationKey
    | SelectCode CodeEditor.Model.TransformationKey
    | NoOp
    | GotCodeResult CodeEditor.Model.ExecutionReturn
    | ShowOrHideHelpPanel
    | RenameApi ApiExplorer.Model.ApiCallKey String
    | PropagateChangesFromApiEditor


type CodeMsgDetail
    = Run
    | Set
