module DeployEditor.Domain exposing (..)

import DeployEditor.Deploy
import Element
import Json.Decode as Decode
import Json.Encode as Encode


type Msg
    = Update Model


update : Msg -> DeployEditor.Deploy.Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update m ->
            ( m, Cmd.none )


encodePickerState _ =
    Encode.null


encodeSelection _ =
    Encode.null



-- [generator-start]


type alias Model =
    { status : String
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.succeed init


encodeModel a =
    Encode.null


encodeSwatch a =
    Encode.null



-- [generator-end]renderSwatch : Swatch -> Element.Element Swatch


init : Model
init =
    { status = ""
    }


view : Element.Element Msg
view =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 12
        ]
        [ Element.el [] (Element.text "Hello domain")
        ]
