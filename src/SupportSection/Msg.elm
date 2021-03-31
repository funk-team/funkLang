module SupportSection.Msg exposing (..)

import RemoteData
import SupportSection.Model exposing (..)


type Msg
    = OpenModalButtonClicked WhichModal
    | CloseModalButtonClicked
    | ToggleExpanded
    | SubmitButtonClicked
    | MessageChanged String
    | EmailChanged String
    | GotSessionInfo SessionInfo
    | FeedbackResponse (RemoteData.WebData ())
