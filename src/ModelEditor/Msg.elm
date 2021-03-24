module ModelEditor.Msg exposing (Msg(..))

import ModelEditor.Model


type Msg
    = SelectField Int
    | SetField Int ModelEditor.Model.Field
    | RemoveField Int
    | RemoveAssociation Int Int
    | AddField
    | ShowOrHideHelpPanel
