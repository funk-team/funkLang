module SupportSection.Feedback exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import RemoteData
import SupportSection.Model exposing (..)
import SupportSection.Msg exposing (..)
import SupportSection.Components
import Ui.Component
import Ui.Style

submitButton model =
    case model.request of
        RemoteData.Success () ->
            Element.none

        _ ->
            Element.Input.button Ui.Component.buttonStyle { onPress = Just SubmitButtonClicked, label = Element.text "Submit Feedback" }


viewForm : String -> Model -> Element.Element Msg
viewForm label model =
    let
        validationFeedback =
            case model.validation of
                Invalid problem ->
                    Element.paragraph [] [ Element.text problem ]

                NotAsked ->
                    Element.none

        progress =
            Element.paragraph [] <|
                List.singleton <|
                    case model.request of
                        RemoteData.NotAsked ->
                            Element.none

                        RemoteData.Loading ->
                            Element.text "Sending..."

                        RemoteData.Success _ ->
                            Element.text "Your feedback has been saved. Thank you!"

                        RemoteData.Failure _ ->
                            Element.text "There was a problem submitting your feedback. Please contact david@funklang.com!"

        viewMessageText =
            case model.request of
                RemoteData.Success _ ->
                    Element.text "Sent!"

                RemoteData.Loading ->
                    Element.text model.message |> Element.el [ Element.Font.italic ]

                _ ->
                    messageArea label model.message
                        |> Element.map MessageChanged

        form =
            Element.column [ Element.spacing 20, Element.width Element.fill ]
                [ viewMessageText
                , validationFeedback
                , progress
                ]
    in
    form


viewModal : Model -> Element.Attribute Msg
viewModal model =
    let
        wrappedForm =
            Element.column
                [ Element.width <| Element.px 600
                , Element.centerX
                , Element.padding 50
                , Element.Border.rounded 5
                , Element.Background.color Ui.Style.white
                , Element.moveDown 50
                , Element.spacing 30
                ]
                [ viewForm "Feedback" model
                , buttonRow
                ]

        buttonRow =
            Element.row
                [ Element.width Element.fill ]
                [ submitButton model
                , Element.el [ Element.alignRight ] closeButton
                ]

        closeButton =
            Element.Input.button Ui.Component.buttonStyle { onPress = Just CloseModalButtonClicked, label = Element.text "Close" }

        modalInner =
            Element.column
                [ Element.width <| Element.px 700
                , Element.centerX
                , Element.padding 50
                , Element.Border.rounded 5
                , Element.Background.color Ui.Style.white
                , Element.moveDown 50
                , Element.spacing 30
                ]
                [ viewForm "Feedback" model
                , buttonRow
                , Element.paragraph [] [Element.text "...or chat with us using the LIVE CHAT BUTTON in the bottom left. You can also reach out to us using the following social channels..."]
                , Element.wrappedRow [Element.spacingXY 15 10] [SupportSection.Components.slack, SupportSection.Components.twitter, SupportSection.Components.reddit, SupportSection.Components.gitHubDiscissions]
                ]

        wrappedModal =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color Ui.Style.transparentBlack
                , Ui.Style.style "backdrop-filter" "blur(5px)"
                ]
                modalInner
    in
    Element.inFront
        (case model.openModal of
            Feedback ->
                wrappedModal

            _ ->
                Element.none
        )


messageArea : String -> String -> Element.Element String
messageArea label msg =
    Element.Input.multiline
        [ Element.width Element.fill
        , Element.height (Element.shrink |> Element.minimum 150)
        ]
        { placeholder = Nothing
        , label = Element.Input.labelAbove [ Element.Font.size 20 ] (Element.text label)
        , spellcheck = True
        , onChange = identity
        , text = msg
        }
