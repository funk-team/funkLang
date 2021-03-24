module Projects.WelcomeModal exposing (onBoarding, onBoardingButton)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Projects.Msg
import SupportSection
import Ui.Component
import Ui.Style


onBoarding : Int -> Element.Attribute Projects.Msg.Msg
onBoarding screenInt =
    let
        miniCloseButton =
            Element.el [ Element.alignRight ]
                (Element.Input.button
                    [ Element.Background.color Ui.Style.black
                    , Element.Border.rounded 20
                    , Element.Font.color Ui.Style.white
                    , Element.Font.center
                    , Element.Font.size 14
                    , Element.width (Element.px 35)
                    , Element.height (Element.px 30)
                    , Element.moveUp 15
                    , Element.moveRight 15
                    ]
                    { onPress = Just Projects.Msg.ToggleOnBoardingScreen, label = Element.text "X" }
                )

        modal =
            Element.column
                [ Element.width <| Element.px 800
                , Element.centerX
                , Element.Border.rounded 5
                , Element.Background.color Ui.Style.white
                , Element.moveDown 20
                ]
                [ miniCloseButton
                , Element.column [ Element.width Element.fill, Element.paddingXY 20 0 ]
                    [ Element.paragraph
                        [ Element.Font.center
                        , Element.Background.color Ui.Style.lightGrey
                        , Element.padding 20
                        , Element.Border.rounded 5
                        , Element.Font.size 22
                        , Element.Font.bold
                        ]
                        [ modalTitle screenInt
                        ]
                    , Element.column
                        [ Element.padding 20
                        , Element.Border.rounded 5
                        , Element.width Element.fill
                        , Element.height Element.shrink
                        , Element.scrollbarY
                        , Element.spacing 20
                        ]
                        [ modalText screenInt
                        , modalContent screenInt
                        , modalButton screenInt
                        ]
                    ]
                ]

        wrappedModal =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color Ui.Style.transparentBlack
                , Ui.Style.style "backdrop-filter" "blur(5px)"
                ]
                modal
    in
    Element.inFront
        wrappedModal


modalTitle screen =
    case screen of
        1 ->
            Element.text "Welcome to funkLang!"

        2 ->
            Element.text "Build Responsive WebApps"

        3 ->
            Element.text "Styling and Design System"

        4 ->
            Element.text "Build like a pro, with no-code"

        5 ->
            Element.text "Write code when you need to"

        _ ->
            Element.text "Help will always be given at funkLang to those who ask for it."


modalText screen =
    case screen of
        1 ->
            textRender
                "funk has the // *bold* power // of a programming launage, the // *bold* simplicity // of a low-code tool and the // *bold* design freedom // of Figma, Sketch or Canva."

        2 ->
            textRender
                "With funk you can draw a responsive WebApp from a // *bold* blank canvas // or select from a // *bold* pre-made template // (coming soon). When you draw in funk you are drawing // *bold* real HTML//."

        3 ->
            textRender
                "Styling is both // *bold* quick // and // *bold* powerful // in funk. Create // *bold* design librarys // to use across your organisation and create // *bold* advanced animations // visually (coming soon)."

        4 ->
            textRender
                "funk has // *bold* visual // state managment, data connections, conditional rendering (coming soon) and secure login (coming soon). This means you can build // *bold* anything // you want visually."

        5 ->
            textRender
                "If you do want need to write code, funk works with // *bold* open web standards//. A developer can connect to funk directly using our // *bold*  CLI // to add custom-elements directly in funk. Push the funk spec to GitHub and build like any SPA"

        _ ->
            textRender
                "If you need help you can use the // *bold* live chat // in the editor, or contact us via Slack or Twitter."


modalContent screen =
    case screen of
        6 ->
            contactScreen

        5 ->
            viewImage (String.fromInt screen)

        _ ->
            viewVideo (String.fromInt screen)


modalButton screen =
    let
        previousButton =
            if screen /= 1 then
                Element.Input.button Ui.Component.buttonStyle { onPress = Just (Projects.Msg.UpdateOnBoardingScreen (screen - 1)), label = Element.text "Previous" }

            else
                Element.none

        nextOrCloseButton =
            if screen <= 5 then
                Element.el [ Element.alignRight ]
                    (Element.Input.button Ui.Component.buttonStyle { onPress = Just (Projects.Msg.UpdateOnBoardingScreen (screen + 1)), label = Element.text "Next" })

            else
                Element.el [ Element.alignRight ]
                    (Element.Input.button Ui.Component.buttonStyle { onPress = Just Projects.Msg.ToggleOnBoardingScreen, label = Element.text "Lets go use funk!" })
    in
    Element.row [ Element.width Element.fill ]
        [ previousButton
        , nextOrCloseButton
        ]


onBoardingButton =
    Element.Input.button
        Ui.Component.buttonStyle
        { label = Element.text "Welcome Guide"
        , onPress = Just Projects.Msg.ToggleOnBoardingScreen
        }


videoCaption body =
    Element.paragraph [ Element.Font.center ] [ Element.text body ]


textRender : String -> Element.Element msg
textRender textToColor =
    let
        multiPartText =
            String.split "//" textToColor

        renderedText =
            List.map renderMultipleText multiPartText
    in
    Element.paragraph [] renderedText


renderMultipleText : String -> Element.Element msg
renderMultipleText text =
    if String.contains "*bold*" text then
        Element.paragraph [ Element.Font.heavy ] [ Element.text (String.replace "*bold*" "" text) ]

    else if String.contains "*orange*" text then
        Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text (String.replace "*orange*" "" text) ]

    else
        Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text (String.replace "*orange*" "" text) ]


contactScreen =
    Element.wrappedRow [ Element.spacing 20, Element.centerX, Element.paddingEach { top = 40, bottom = 40, left = 0, right = 0 } ]
        [ Element.el [ Element.centerX ] <| SupportSection.slack
        , Element.el [ Element.centerX ] <| SupportSection.twitter
        ]


viewImage imageNumber =
    Element.image [ Element.width Element.fill ] { src = "http://cdn.funklang.com/onboardingVideos/" ++ imageNumber ++ ".png", description = "" }


viewVideo videoNumber =
    let
        nopointerEvents =
            [ Html.Attributes.style "pointer-events" "none" ]

        player =
            Element.html <|
                Html.video
                    [ Html.Attributes.src ("http://cdn.funklang.com/onboardingVideos/" ++ videoNumber ++ ".mp4")
                    , Html.Attributes.style "width" "90%"
                    , Html.Attributes.style "height" "90%"
                    , Html.Attributes.controls False -- html renders empty string per default which means it won't get accepted by firefox
                    , Html.Attributes.autoplay True -- html renders empty string per default which means it won't get accepted by firefox
                    , Html.Attributes.loop True -- html renders empty string per default which means it won't get accepted by firefox
                    , Html.Attributes.style "display" "block"
                    , Html.Attributes.style "margin-left" "auto"
                    , Html.Attributes.style "margin-right" "auto"
                    , Html.Attributes.style "padding-top" "20px"
                    , Html.Attributes.style "padding-bottom" "20px"
                    ]
                    []
    in
    player
