module Dynamic.Types exposing (..)

import DataBinding
import Database.Types
import Dict exposing (Dict)
import Dynamic.Data.Types
import Dynamic.Msg
import Screen



-- SPEC


type alias Spec =
    { screens : Screen.Screens

    -- , model : Model
    , workflows : List Workflow
    , database : Database.Types.Model
    , styleSettings : StyleSettings
    , store : DataBinding.Store
    }



-- META


type alias Summary =
    { name : String
    , model : Model
    , events : EventSummary
    , subScreens : List Int
    , implicitUpdates : Update
    , bindableElements : Dict Int Dynamic.Data.Types.NamedType
    }


type alias EventSummary =
    { clickEmitters : List Int
    , changeEmitters : List Int
    }



-- UPDATE


type alias Update =
    List UpdateMap



-- map a msg onto an update


type alias UpdateMap =
    { msg : Dynamic.Msg.AtomMsgFunnel
    , operation : Operation
    }


type FunnelResult
    = ClickMatch
    | ChangeMatch Dynamic.Data.Types.Instance
    | NoMatch


type alias Target =
    Int


type Operation
    = AddToCollection Screen.Id Target
    | UpdateField Target
    | ClearViewModel Screen.Id



-- MODEL


type alias Model =
    Dict Int Dynamic.Data.Types.NamedInstance


type alias StyleSettings =
    { customCss : String
    , useDefaultStyles : Bool
    , dimensions : ( Int, Int )
    }


type alias Trigger =
    Dynamic.Msg.AtomMsgFunnel


type alias Workflow =
    { name : String
    , trigger : Maybe Trigger
    , steps : List Operation
    }
