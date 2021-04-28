port module CodeEditor.CustomElements exposing (main)

import Browser
import Element
import Ui.CodeEditor

port registerCustomElement : Model -> Cmd msg

type Msg = SetModel Model

main : Program () Model Msg
main = Browser.element
    { init = always (Model "my-element" initCode, Cmd.none)
    , view = view_
    , subscriptions = always Sub.none
    , update = update
    }

type alias Model =
    {name : String, code : String}

view_ model = Element.layout [] (view model)

view : Model -> Element.Element Msg
view model =
    let
        editor =
            Ui.CodeEditor.view
                [ Ui.CodeEditor.value model.code
                , Ui.CodeEditor.mode "javascript"
                , Ui.CodeEditor.onChange (\code_ -> SetModel { model | code = Debug.log "new code" code_ })
                ]
                |> Element.html
                |> Element.el [ Element.width Element.fill ]
    in
        Element.column
            [ Element.width Element.fill, Element.spacing 10 ]
            [ editor
            -- , executionPreview userModel transformation
            ]


update msg _ =
    case msg of
        SetModel model ->
            (model, registerCustomElement model)



initCode = """
class PopUpInfo extends HTMLElement {
  constructor() {
    // Always call super first in constructor
    super();
    // write element functionality in here
    this.innerHTML = 'hi'

  }
}
"""
