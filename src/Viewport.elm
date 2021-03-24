module Viewport exposing
    ( Device
    , Kind(..)
    , Preset(..)
    , correspondingPreset
    , decodeDevice
    , decodeKind
    , encodeDevice
    , encodeKind
    , groupedPresets
    , kindToString
    , kinds
    , presetToDevice
    , presetToString
    , presets
    )

{-| Viewports and types plus some examples
-}

import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Rectangle



-- [decgen-start]
---- DATA STRUCTURES ----


type alias Device =
    { name : String
    , width : Int
    , height : Int
    , kind : Kind
    }


type Kind
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


type Preset
    = MacBook
    | MacBookPro
    | IPhone11ProMax
    | IPhoneSE
    | GooglePixel2
    | IPadMini
    | IPadPro11
    | IPadPro129
    | Standard1080p
    | Standard1440p
    | Standard4k



---- USEFULL ----


presetToString preset =
    presetToDevice preset
        |> .name


kindToString kind =
    case kind of
        Phone ->
            "Mobile"

        Tablet ->
            "Tablet"

        Desktop ->
            "Laptop"

        BigDesktop ->
            "Desktop"


{-| Finds the device that correspond to the rectangle size if there is one
-}
correspondingPreset : Rectangle.Rectangle -> Maybe Preset
correspondingPreset rect =
    let
        doesCorrespond preset =
            let
                device =
                    presetToDevice preset

                deviceHeight =
                    toFloat device.height

                deviceWidth =
                    toFloat device.width

                rectHeight =
                    Rectangle.height rect

                rectWidth =
                    Rectangle.width rect

                similarHeight =
                    (rectHeight <= (deviceHeight + 0.05))
                        && (rectHeight >= (deviceHeight - 0.05))

                similarWidth =
                    (rectWidth <= (deviceWidth + 0.05))
                        && (rectWidth >= (deviceWidth - 0.05))
            in
            similarHeight && similarWidth
    in
    presets |> List.Extra.find doesCorrespond


presetToDevice preset =
    getCorrespondingDevice preset



---- CONSTANTS ----


kinds =
    [ Phone, Tablet, Desktop, BigDesktop ]


presets : List Preset
presets =
    groupedPresets
        |> List.map Tuple.second
        |> List.concat


groupedPresets =
    [ ( Phone, allPhones )
    , ( Tablet, allTablets )
    , ( Desktop, allDesktops )
    , ( BigDesktop, allBigDesktops )
    ]


allPhones =
    [ IPhone11ProMax
    , IPhoneSE
    , GooglePixel2
    ]


allTablets =
    [ IPadMini
    , IPadPro11
    , IPadPro129
    ]


allDesktops =
    [ MacBook
    , MacBookPro
    ]


allBigDesktops =
    [ Standard1080p
    , Standard1440p
    , Standard4k
    ]



---- DEVICES ----


desktop =
    Device "Desktop" 1440 1024 Desktop


macBook =
    Device "MacBook" 1152 700 Desktop


macBookPro =
    Device "MacBook Pro" 1440 900 Desktop


surfaceBook =
    Device "Surface Book" 1500 1000 Desktop


iMac =
    Device "iMac" 1280 720 Desktop


iPhone11ProMax =
    Device "iPhone 11 Pro Max" 414 896 Phone


iPhone11ProX =
    Device "iPhone 11 Pro / X" 375 812 Phone


iPhone8Plus =
    Device "iPhone 8 Plus" 414 735 Phone


iPhone8 =
    Device "iPhone 8" 375 667 Phone


iPhoneSE =
    Device "iPhone SE" 320 568 Phone


googlePixel2 =
    Device "Google Pixel 2" 411 731 Phone


googlePixel2XL =
    Device "Google Pixel 2 XL" 411 823 Phone


android =
    Device "Android" 360 640 Phone


iPadMini =
    Device "iPad mini" 768 1024 Tablet


iPadPro11 =
    Device "iPad Pro 11\"" 834 1194 Tablet


iPadPro129 =
    Device "iPad Pro 12.9\"" 1024 1366 Tablet


surfacePro3 =
    Device "Surface Pro 3" 1440 990 Tablet


surfacePro4 =
    Device "Surface Pro 4" 1368 912 Tablet


standard1080p =
    Device "Standard 1080p" 1920 1080 BigDesktop


standard1440p =
    Device "Standard 1440p" 2560 1440 BigDesktop


standard4k =
    Device "Standard 4k" 3840 2160 BigDesktop



---- CORRESPOND ----


getCorrespondingDevice preset =
    case preset of
        MacBook ->
            macBook

        MacBookPro ->
            macBookPro

        IPhone11ProMax ->
            iPhone11ProMax

        IPhoneSE ->
            iPhoneSE

        GooglePixel2 ->
            googlePixel2

        IPadMini ->
            iPadMini

        IPadPro11 ->
            iPadPro11

        IPadPro129 ->
            iPadPro129

        Standard1440p ->
            standard1440p

        Standard4k ->
            standard4k

        Standard1080p ->
            standard1080p



-- [decgen-generated-start] -- DO NOT MODIFY or remove this line


decodeKind =
    let
        recover x =
            case x of
                "Phone" ->
                    Decode.succeed Phone

                "Tablet" ->
                    Decode.succeed Tablet

                "Desktop" ->
                    Decode.succeed Desktop

                "BigDesktop" ->
                    Decode.succeed BigDesktop

                other ->
                    Decode.fail <| "Unknown constructor for type Kind: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeDevice =
    Decode.map4
        Device
        (Decode.field "name" Decode.string)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "kind" decodeKind)


encodeKind a =
    case a of
        Phone ->
            Encode.string "Phone"

        Tablet ->
            Encode.string "Tablet"

        Desktop ->
            Encode.string "Desktop"

        BigDesktop ->
            Encode.string "BigDesktop"


encodeDevice a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "width", Encode.int a.width )
        , ( "height", Encode.int a.height )
        , ( "kind", encodeKind a.kind )
        ]



-- [decgen-end]
