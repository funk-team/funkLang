module Clipboard.Msg exposing (..)

import Clipboard.Model


type Msg
    = ClipboardChanged Clipboard.Model.Contents
    | NoOp
    | Copy
    | Cut
    | Paste Bool
