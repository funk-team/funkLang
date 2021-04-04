module Shortcut exposing (..)

import Canvas.Msg
import Canvas.Tool.Draw.Model
import Canvas.Tool.Model
import Canvas.Tool.Transform.Model
import Clipboard.Msg
import Element
import Keyboard
import List.Extra
import Maybe.Extra
import Ui.Boxicons
import Ui.Component
import Ui.Help


type alias Shortcut =
    { keys : List Keyboard.Key
    , msg : Canvas.Msg.RootMsg
    }



-- turn shortcuts into functions
-- check sticky in Canvas


shortcuts : List Shortcut
shortcuts =
    [ { keys = [ Keyboard.Character "E" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing
                    , sticky = False
                    , mode = Canvas.Tool.Draw.Model.Box
                    }
      }
    , { keys = [ Keyboard.Shift, Keyboard.Character "E" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = True, mode = Canvas.Tool.Draw.Model.Box }
      }

    -- , { keys = [ Keyboard.Character "S" ]
    --   , msg =
    --         Canvas.Msg.ChangeTool <|
    --             Canvas.Tool.Model.Cut
    --                 Canvas.Tool.Cut.NotHovering
    --   }
    , { keys = [ Keyboard.Character "B" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = False, mode = Canvas.Tool.Draw.Model.Button }
      }
    , { keys = [ Keyboard.Shift, Keyboard.Character "B" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = True, mode = Canvas.Tool.Draw.Model.Button }
      }
    , { keys = [ Keyboard.Character "I" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = False, mode = Canvas.Tool.Draw.Model.TextInput }
      }
    , { keys = [ Keyboard.Shift, Keyboard.Character "I" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = True, mode = Canvas.Tool.Draw.Model.TextInput }
      }
    , { keys = [ Keyboard.Character "T" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = False, mode = Canvas.Tool.Draw.Model.Text }
      }
    , { keys = [ Keyboard.Shift, Keyboard.Character "T" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Draw
                    { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = True, mode = Canvas.Tool.Draw.Model.Text }
      }
    , { keys = [ Keyboard.Character "V" ]
      , msg =
            Canvas.Msg.ChangeTool <|
                Canvas.Tool.Model.Transform
                    Canvas.Tool.Transform.Model.NotTransforming
      }
    , { keys = [ Keyboard.Backspace ]
      , msg = Canvas.Msg.DeleteKeyPressed
      }

    -- windows and linux with ctrl
    , { keys = [ Keyboard.Character "Z", Keyboard.Control ]
      , msg = Canvas.Msg.UndoClicked
      }
    , { keys = [ Keyboard.Character "Z", Keyboard.Control, Keyboard.Shift ]
      , msg = Canvas.Msg.RedoClicked
      }
    , { keys = [ Keyboard.Character "Y", Keyboard.Control ]
      , msg = Canvas.Msg.RedoClicked
      }
    , { keys = [ Keyboard.Character "0", Keyboard.Control ]
      , msg = Canvas.Msg.ResetCamera 1
      }

    -- OSX with meta
    , { keys = [ Keyboard.Character "Z", Keyboard.Meta ]
      , msg = Canvas.Msg.UndoClicked
      }
    , { keys = [ Keyboard.Character "Z", Keyboard.Meta, Keyboard.Shift ]
      , msg = Canvas.Msg.RedoClicked
      }
    , { keys = [ Keyboard.Character "Y", Keyboard.Meta ]
      , msg = Canvas.Msg.RedoClicked
      }
    , { keys = [ Keyboard.Character "0", Keyboard.Meta ]
      , msg = Canvas.Msg.ResetCamera 1
      }
    ]
        |> List.map (\{ msg, keys } -> { keys = keys, msg = Canvas.Msg.EditorMsg msg })
        |> (++) clipboardHotkeys


clipboardHotkeys : List Shortcut
clipboardHotkeys =
    [ { keys = [ Keyboard.Character "C", Keyboard.Meta ]
      , msg = Canvas.Msg.ClipboardMsg Clipboard.Msg.Copy
      }
    , { keys = [ Keyboard.Character "X", Keyboard.Meta ]
      , msg = Canvas.Msg.ClipboardMsg Clipboard.Msg.Cut
      }
    , { keys = [ Keyboard.Character "V", Keyboard.Meta ]
      , msg = Canvas.Msg.ClipboardMsg (Clipboard.Msg.Paste False)
      }
    , { keys = [ Keyboard.Character "V", Keyboard.Meta, Keyboard.Shift ]
      , msg = Canvas.Msg.ClipboardMsg (Clipboard.Msg.Paste True)
      }

    -- windows
    , { keys = [ Keyboard.Character "C", Keyboard.Control ]
      , msg = Canvas.Msg.ClipboardMsg Clipboard.Msg.Copy
      }
    , { keys = [ Keyboard.Character "X", Keyboard.Control ]
      , msg = Canvas.Msg.ClipboardMsg Clipboard.Msg.Cut
      }
    , { keys = [ Keyboard.Character "V", Keyboard.Control ]
      , msg = Canvas.Msg.ClipboardMsg (Clipboard.Msg.Paste False)
      }
    , { keys = [ Keyboard.Character "V", Keyboard.Control, Keyboard.Shift ]
      , msg = Canvas.Msg.ClipboardMsg (Clipboard.Msg.Paste True)
      }
    ]


viewDebugger : List Keyboard.Key -> Element.Attribute msg
viewDebugger keys =
    List.map viewKey keys
        |> Element.row
            [ Element.alignBottom
            , Element.alignRight
            , Element.spacing 5
            , Element.padding 5
            , Element.alpha 0.5
            , Ui.Help.noPointerEvents
            ]
        |> Element.inFront


viewKey : Keyboard.Key -> Element.Element msg
viewKey key =
    case key of
        Keyboard.Shift ->
            Ui.Component.icon Ui.Boxicons.bxUpvote

        Keyboard.Control ->
            Ui.Component.icon Ui.Boxicons.bxCommand

        Keyboard.Meta ->
            Ui.Component.icon Ui.Boxicons.bxCommand

        Keyboard.Character c ->
            Element.text c

        _ ->
            Element.none


update : List Keyboard.Key -> Keyboard.Msg -> ( List Keyboard.Key, Maybe Canvas.Msg.RootMsg )
update pressedKeys msg =
    let
        ( newPressedKeys, change ) =
            Keyboard.updateWithKeyChange Keyboard.anyKeyUpper msg pressedKeys
    in
    case change of
        Just (Keyboard.KeyDown key) ->
            case resolveMatchingShortcut newPressedKeys of
                Just msg_ ->
                    ( clearNonModifiers newPressedKeys, Just msg_ )

                Nothing ->
                    ( newPressedKeys, Nothing )

        Just (Keyboard.KeyUp key) ->
            case isMod key of
                True ->
                    ( clearNonModifiers newPressedKeys, Nothing )

                False ->
                    ( newPressedKeys, Nothing )

        Nothing ->
            ( newPressedKeys, Nothing )


{-| Major complexity: For some reason the letter keys do not trigger a keyup event when
pressed in combination with a modifier. This means that for example with CMD+Z 'Z' gets stuck.

For this reason we clear out non-modifiers from our pressed keys whenever a keyboard combo matches.

-}
clearNonModifiers : List Keyboard.Key -> List Keyboard.Key
clearNonModifiers =
    List.filter isMod


isMod : Keyboard.Key -> Bool
isMod key =
    case key of
        Keyboard.Meta ->
            True

        Keyboard.Shift ->
            True

        Keyboard.Control ->
            True

        _ ->
            False


resolveMatchingShortcut : List Keyboard.Key -> Maybe Canvas.Msg.RootMsg
resolveMatchingShortcut pressed =
    shortcuts
        |> List.Extra.find (matchWithPressed pressed)
        |> Maybe.map .msg


matchWithPressed : List Keyboard.Key -> Shortcut -> Bool
matchWithPressed pressedKeys shortcut =
    let
        requiredKeysArePressed : Bool
        requiredKeysArePressed =
            shortcut.keys
                |> List.all (\requiredKey -> List.member requiredKey pressedKeys)

        notPressingAnyOtherKeys : Bool
        notPressingAnyOtherKeys =
            List.length shortcut.keys == List.length pressedKeys
    in
    requiredKeysArePressed && notPressingAnyOtherKeys


listMemberWith : (a -> Bool) -> List a -> Bool
listMemberWith fn =
    List.Extra.find fn
        >> Maybe.Extra.isJust
