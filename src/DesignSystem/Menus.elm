module DesignSystem.Menus exposing (Menu(..), decodeMenu, encodeMenu, menus, toString)

import Json.Decode as Decode
import Json.Encode as Encode


type Menu
    = Typography
    | Colors
    | Icons
    | Shadows


toString menu =
    case menu of
        Typography ->
            "Typography"

        Colors ->
            "Colors"

        Icons ->
            "Icons"

        Shadows ->
            "Shadows"



-- if you want to change menu's order, change here:


menus =
    [ Typography
    , Icons
    , Colors
    , Shadows
    ]



-- encode and decode
-- useless to keep in spec
-- @@TODO: validate
-- this will be the default menu to open


decodeMenu =
    Typography
        |> Decode.succeed


encodeMenu _ =
    Encode.null
