module SupportSection.Model exposing (..)

import RemoteData


type alias Model =
    { sessionInfo : SessionInfo
    , message : String
    , validation : Validation
    , request : RemoteData.WebData ()
    , isExpanded : Bool
    , openModal : WhichModal
    , email : String
    }


type Validation
    = NotAsked
    | Invalid String


type WhichModal
    = Feedback
    | Docs
    | Community
    | RoadMap
    | Changelog
    | AllClosed


type alias SessionInfo =
    { email : String
    , logRocketUrl : String
    }
