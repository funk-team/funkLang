port module Persistence exposing (..)

{-| Save stuff to localstorage
-}

import Json.Decode as Decode
import Json.Encode as Encode



-- [generator-start]


type alias ProjectMeta =
    { projectName : String
    , projectId : String
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeProjectMeta =
    Decode.map2
        ProjectMeta
        (Decode.field "projectName" Decode.string)
        (Decode.field "projectId" Decode.string)


encodeProjectMeta a =
    Encode.object
        [ ( "projectName", Encode.string a.projectName )
        , ( "projectId", Encode.string a.projectId )
        ]



-- [generator-end]


saveModel :
    (a -> Encode.Value)
    -> ProjectMeta
    -> a
    -> Cmd msg
saveModel encode projectMeta data =
    encode data
        |> Encode.encode 4
        |> Tuple.pair projectMeta
        |> save


loadModel :
    Decode.Decoder a
    -> String
    -> Result Decode.Error a
loadModel decoder storage =
    Decode.decodeString decoder storage


type alias Key =
    String


type alias LocalStorage =
    Decode.Value


type alias EncodedModel =
    String


port save : ( ProjectMeta, EncodedModel ) -> Cmd msg


port specUpdated : (String -> msg) -> Sub msg


port previewSpecUpdated : (( ProjectMeta, String ) -> msg) -> Sub msg


port checkout : ProjectMeta -> Cmd msg


port checkoutCompleted : (String -> msg) -> Sub msg
