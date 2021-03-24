module ApiExplorer.Menus exposing (Menu(..), decodeMenu, encodeMenu, menus, toString)

import Json.Decode as Decode
import Json.Encode as Encode
import ZipList


type Menu
    = APIs
    | Sites
    | Uploads


toString menu =
    case menu of
        APIs ->
            "APIs"

        Sites ->
            "Sites"

        Uploads ->
            "Uploads"



-- if you want to change menu's order, change here:


menus selectedMenu =
    let
        menuZipList =
            ZipList.new
                APIs
                [ Sites
                , Uploads
                ]
    in
    menuZipList
        |> ZipList.goToFirst ((==) selectedMenu)
        |> Maybe.withDefault menuZipList



-- encode and decode
-- useless to keep in spec
-- this will be the default menu to open


decodeMenu =
    APIs
        |> Decode.succeed


encodeMenu _ =
    Encode.null
