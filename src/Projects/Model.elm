module Projects.Model exposing (..)

import Bounce
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData
import Time


type alias Projects =
    RemoteData.WebData (List Project)


type alias NewProject =
    RemoteData.WebData Project


type alias Model =
    { projects : Projects
    , newProject : NewProject
    , deletePending : Maybe ( String, RemoteData.WebData String )
    , bounce : Bounce.Bounce
    }


type alias Project =
    { id : String
    , name : String
    , repo : String
    , updatedAt : Time.Posix
    , pendingChanges : RemoteData.WebData ()
    }


encodeTimestamp : Time.Posix -> Encode.Value
encodeTimestamp =
    Iso8601.fromTime >> Encode.string


decodeTimeStamp =
    Decode.string
        |> Decode.andThen
            (\stamp ->
                case Iso8601.toTime stamp of
                    Err _ ->
                        Decode.fail "Could not decode timestamp"

                    Ok val ->
                        Decode.succeed val
            )


decodeProject =
    Decode.map5
        Project
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "repo" Decode.string)
        (Decode.field "updated_at" decodeTimeStamp)
        (Decode.succeed (RemoteData.Success ()))


encodeProject a =
    Encode.object
        [ ( "id", Encode.string a.id )
        , ( "name", Encode.string a.name )
        , ( "repo", Encode.string a.repo )
        , ( "updated_at", encodeTimestamp a.updatedAt )
        ]
