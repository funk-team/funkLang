module Ui.CodeEditor exposing
    ( Attribute
    , LinterMessage
    , Position
    , Severity(..)
    , linterMessages
    , mode
    , onChange
    , readOnly
    , tabSize
    , value
    , view
    , vim
    )

{-| <https://gist.github.com/lukewestby/dec1cca157faea7aac7ffe5da3280324>
-}

import Html exposing (Html)
import Html.Attributes exposing (property)
import Html.Events as Events exposing (on)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


{-| Wrap the attributes that are allowed so that a user can't
insert arbitrary stuff from Html.Attributes or Html.Events
-}
type Attribute msg
    = Attr (Html.Attribute msg)


unattr : Attribute msg -> Html.Attribute msg
unattr (Attr a) =
    a


{-| The current value of the editor
-}
value : String -> Attribute msg
value =
    Encode.string >> property "value" >> Attr


{-| The language mode
-}
mode : String -> Attribute msg
mode =
    Encode.string >> property "mode" >> Attr


{-| Whether to run the editor in vim mode
-}
vim : Bool -> Attribute msg
vim value_ =
    Attr <| property "vimMode" <| Encode.bool value_


{-| How many spaces to a tab
-}
tabSize : Int -> Attribute msg
tabSize =
    Encode.int >> property "tabSize" >> Attr


{-| Whether the editor's text can be changed. It would be cool if this was
set automatically by the presence or absence of a change listener
-}
readOnly : Attribute msg
readOnly =
    Attr <| property "readOnly" <| Encode.bool True


{-| A list of hardcoded linter messages so that Elm error messages can be displayed
from the outside.
-}
linterMessages : List LinterMessage -> Attribute msg
linterMessages messages =
    Attr <| property "linterMessages" <| Encode.list linterMessageEncoder messages


{-| Listen for changes in the editor's value

The event is called change\_ because events emitted from within the custom element caused text in the editor to disapper

-}
onChange : (String -> msg) -> Attribute msg
onChange tagger =
    Attr <| on "change_" (Decode.map tagger Events.targetValue)


{-| Render a code editor
-}
view : List (Attribute msg) -> Html msg
view attributes =
    Html.node "code-editor" (List.map unattr attributes) []


{-| Level of a linter message
-}
type Severity
    = Error
    | Warning


{-| Position of a linter message span's start and end
-}
type alias Position =
    { line : Int
    , column : Int
    }


{-| Linter message
-}
type alias LinterMessage =
    { severity : Severity
    , message : String
    , from : Position
    , to : Position
    }


linterMessageEncoder : LinterMessage -> Value
linterMessageEncoder linterMessage =
    Encode.object
        [ ( "severity", severityEncoder linterMessage.severity )
        , ( "message", Encode.string linterMessage.message )
        , ( "from", positionEncoder linterMessage.from )
        , ( "to", positionEncoder linterMessage.to )
        ]


severityEncoder : Severity -> Value
severityEncoder severity =
    case severity of
        Error ->
            Encode.string "error"

        Warning ->
            Encode.string "warning"


positionEncoder : Position -> Value
positionEncoder position =
    Encode.object
        [ ( "line", Encode.int position.line )
        , ( "ch", Encode.int position.column )
        , ( "sticky", Encode.null )
        ]
