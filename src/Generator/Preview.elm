module Generator.Preview exposing (..)

import Browser.Navigation
import Canvas.Msg
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Elm.Pretty
import Elm.Syntax.Expression
import Generated.View
import Generator
import Http
import Json.Encode as Encode
import List.Extra
import Spec.Element.Id
import Model
import Model.Model
import Pretty
import Route
import Ui.Style
import Url



-- Browser.Navigation.replaceUrl navKey urlStr


prettify : Elm.Syntax.Expression.Expression -> String
prettify =
    Elm.Pretty.prettyExpression >> Pretty.pretty 120


view : ( Url.Url, Browser.Navigation.Key ) -> Spec.Element.Id.Id -> Model.Model.UserModel -> Element.Element (Cmd Canvas.Msg.RootMsg)
view ( url, navKey ) screenId userModel =
    let
        screenToString =
            Generator.renderScreen userModel
                >> prettify

        maybeScreenCode =
            Generated.View.viewScreens
                |> List.head

        codePreview =
            userModel.itemsOnCanvas
                |> List.Extra.find (\{shared} -> shared.id == screenId)
                |> Maybe.map screenToString
                |> Maybe.withDefault "Screen not found so I could not generate the code."
                |> Element.text
                |> List.singleton
                |> Element.paragraph [ Ui.Style.monospace ]

        screen =
            case maybeScreenCode of
                Nothing ->
                    Element.text ("No screen code generated")

                Just rendered ->
                    Element.el
                        [ Element.padding 10
                        , Element.Background.color Ui.Style.slightAccent
                        , Element.height Element.fill
                        , Element.width Element.fill
                        ]
                    <|
                        Element.el
                            [ Element.Background.color Ui.Style.white
                            , Element.height Element.fill
                            , Element.width Element.fill
                            ]
                            rendered

        headMenu =
            Element.row
                [ Element.Border.widthEach { edges | bottom = 1 }
                , Element.Border.color (Element.rgb 0 0 0)
                , Element.Background.color (Element.rgb255 0 0 0)
                , Element.width Element.fill
                , Element.height Element.shrink
                ]
                [ headMenuButton "Run Generator" (runCodeGen userModel) True
                ]

        headMenuButton : String -> msg -> Bool -> Element.Element msg
        headMenuButton text onClick isHighlighted =
            let
                isHighlightedStyles =
                    if isHighlighted then
                        [ Element.Border.color Ui.Style.highlightColor
                        , Element.padding 8
                        , Element.Border.rounded 3
                        , Element.Background.color Ui.Style.highlightColorSolid
                        , Element.mouseOver <|
                            [ Element.Background.color Ui.Style.white, Element.Font.color Ui.Style.black ]
                        , Element.pointer
                        ]

                    else
                        [ Element.padding 8
                        , Element.Border.rounded 3
                        , Element.Font.color Ui.Style.highlightColorSolid
                        , Element.mouseOver <|
                            [ Element.Background.color Ui.Style.white ]
                        , Element.pointer
                        ]
            in
            Element.Input.button
                isHighlightedStyles
                { label = Element.text text, onPress = Just onClick }
                |> Element.el [ Element.paddingXY 10 5 ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ headMenu
        , codePreview
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbars
            ]
            screen
        ]


runCodeGen : Model.Model.UserModel -> Cmd Canvas.Msg.RootMsg
runCodeGen userModel =
    let
        generatedCode =
            Generator.renderFiles userModel

        encodeRecord { name, content } =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "content", Encode.string content )
                ]

        json =
            Encode.list
                encodeRecord
                generatedCode
    in
    Http.post
        { url = "/api/insert-generated-code"
        , body = Http.jsonBody json
        , expect =
            Http.expectWhatever
                (always <| Canvas.Msg.RunCmd Cmd.none)
        }


goToScreen : ( Url.Url, Browser.Navigation.Key ) -> Int -> Cmd msg
goToScreen ( url, navKey ) index =
    let
        urlStr =
            toScreenPreviewPath url index
    in
    Browser.Navigation.replaceUrl navKey urlStr



toScreenPreviewPath url index =
    let
        protocol =
            case url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        host =
            url.host

        port_ =
            url.port_
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        screen =
            String.fromInt index
    in
    protocol ++ host ++ ":" ++ port_ ++ "/" ++ Route.codeGenPreviewStr ++ "/" ++ screen


edges =
    { top = 0, bottom = 0, right = 0, left = 0 }
