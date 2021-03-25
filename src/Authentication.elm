port module Authentication exposing (..)

import Json.Decode as Decode


type State
    = Undetermined
    | Anonymous
    | LoggedIn UserInfo
    | OpenCoreUser


type alias UserInfo =
    { name : String
    , email : String
    , id : String
    , showOnboarding : Bool
    , onBoardingScreen : Int
    }


port authStateChanged : (Maybe Decode.Value -> msg) -> Sub msg


port logout : () -> Cmd msg
