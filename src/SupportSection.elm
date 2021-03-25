port module SupportSection exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData
import RemoteData.Http
import SupportSection.Components
import SupportSection.Feedback
import SupportSection.Model exposing (..)
import SupportSection.Msg exposing (..)
import SupportSection.Releases
import Ui.Boxicons
import Ui.Component
import Ui.Style


init : Model
init =
    Model
        (SessionInfo "[not logged in]" "[no session tracked]")
        ""
        NotAsked
        RemoteData.NotAsked
        False
        AllClosed


sendToServer : SessionInfo -> String -> Cmd Msg
sendToServer { logRocketUrl, email } message =
    let
        body =
            Encode.object
                [ ( "logRocketUrl", Encode.string logRocketUrl )
                , ( "email", Encode.string email )
                , ( "message", Encode.string message )
                ]
    in
    RemoteData.Http.post
        "/api_server/collect-feedback"
        FeedbackResponse
        (Decode.succeed ())
        body


port gotSessionInfo : (SessionInfo -> msg) -> Sub msg


genericButton action text =
    Element.Input.button Ui.Component.buttonStyle { onPress = Just action, label = Element.text text }


closeButtonRight action =
    Element.row [ Element.width Element.fill ] [ Element.el [ Element.alignRight ] (genericButton action "Close") ]


viewModalDocs : Model -> Element.Attribute Msg
viewModalDocs model =
    let
        -- buttonRow =
        --     Element.row [ Element.width Element.fill ] [ Element.el [ Element.alignRight ] (genericButton CloseModalButtonClicked "Close") ]
        youTube =
            SupportSection.Components.buttonWithIcon Ui.Boxicons.bxlYoutube "YouTube" "https://www.youtube.com/channel/UCHVIria50ZFSNk9M40BfNeQ/featured/"

        docs =
            SupportSection.Components.buttonWithIcon Ui.Boxicons.bxsBookOpen "Offical Guide" "https://github.com/funk-team/funkLang#project-status"

        blogPosts =
            SupportSection.Components.buttonWithIcon Ui.Boxicons.bxlMediumSquare "Blog posts" "https://medium.com/@funklang/"

        modal =
            Element.column
                [ Element.width <| Element.px 700
                , Element.centerX
                , Element.padding 50
                , Element.Border.rounded 5
                , Element.Background.color Ui.Style.white
                , Element.moveDown 50
                , Element.spacing 30
                ]
                [ Element.column [ Element.spacing 20, Element.width Element.fill ]
                    [ Element.paragraph
                        [ Element.Font.center
                        , Element.Background.color Ui.Style.lightGrey
                        , Element.padding 20
                        , Element.Border.rounded 5
                        , Element.Font.size 22
                        , Element.Font.bold
                        ]
                        [ Element.text "Video tutorials, live streams, blog posts and offical documentation written by the funk team!" ]
                    , Element.wrappedRow [ Element.spacing 20, Element.centerX, Element.paddingEach { top = 30, bottom = 10, left = 0, right = 0 } ]
                        [ Element.el [ Element.centerX ] <| docs

                        -- , Element.el [ Element.centerX ] <| youTube
                        -- , Element.el [ Element.centerX ] <| blogPosts
                        ]
                    , Element.paragraph
                        [ Element.Font.center
                        , Element.Border.rounded 5
                        , Element.centerX
                        , Element.width (Element.px 400)
                        ]
                        [ Element.text "Coming soon -> video guides, blog posts and live building sessions, join our slack group for updates, or follow us on Twitter!" ]
                    , Element.row [ Element.centerX, Element.spacingXY 10 0 ] [ SupportSection.Components.slack, SupportSection.Components.twitter ]
                    ]
                , closeButtonRight CloseModalButtonClicked
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
        (case model.openModal of
            Docs ->
                wrappedModal

            _ ->
                Element.none
        )


mapText title points =
    let
        pointRender point =
            Element.paragraph [ Element.width Element.fill ] [ Element.text point ]
    in
    Element.column
        [ Element.spacingXY 0 10, Element.width Element.fill ]
        ([ Element.el [ Element.Font.bold, Element.Font.size 20 ] (Element.text title)
         ]
            ++ [ Element.column [ Element.spacingXY 0 5, Element.width Element.fill ] (List.map pointRender points) ]
        )


mapTextWithSide title titleRight points =
    let
        pointRender point =
            Element.paragraph [ Element.width Element.fill ] [ Element.text point ]
    in
    Element.column
        [ Element.spacingXY 0 10, Element.width Element.fill ]
        ([ Element.row [ Element.width Element.fill, Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 } ]
            [ Element.el [ Element.Font.bold, Element.Font.size 20 ] (Element.text title)
            , Element.el [ Element.Font.color Ui.Style.grey, Element.centerY, Element.Font.size 14, Element.alignRight ] (Element.text titleRight)
            ]
         ]
            ++ [ Element.column [ Element.spacingXY 0 5, Element.width Element.fill ] (List.map pointRender points) ]
        )


viewModalRoadMap : Model -> Element.Attribute Msg
viewModalRoadMap model =
    let
        suggest =
            SupportSection.Feedback.viewForm "Suggest a feature" model

        modal =
            Element.column
                [ Element.width <| Element.px 700
                , Element.centerX
                , Element.padding 50
                , Element.Border.rounded 5
                , Element.Background.color Ui.Style.white
                , Element.moveDown 50
                , Element.spacing 30
                ]
                [ Element.column [ Element.spacing 20, Element.width Element.fill ]
                    [ Element.paragraph
                        [ Element.Font.center
                        , Element.Background.color Ui.Style.lightGrey
                        , Element.padding 20
                        , Element.Border.rounded 5
                        , Element.Font.size 22
                        , Element.Font.bold
                        ]
                        [ Element.text "Roadmap" ]
                    , Element.column
                        [ Element.padding 20
                        , Element.Border.rounded 5
                        , Element.width Element.fill
                        , Element.spacing 20
                        , Element.scrollbarY
                        ]
                        [ mapText
                            "Coming soon.."
                            [ "➡ Multiselect"
                            , "➡ Components"
                            , "➡ Breakpoints"
                            , "➡ Improved Responsify"
                            , "➡ More styling options (e.g. hover styles)"
                            ]
                        , gitHubTrack
                        , suggest
                        , SupportSection.Feedback.submitButton model
                        ]
                    ]
                , closeButtonRight CloseModalButtonClicked
                ]

        gitHubTrack =
            SupportSection.Components.buttonWithIcon Ui.Boxicons.bxGit "Track Our Progress on GitHub" "https://github.com/funk-team/funkLang/issues/"

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
        (case model.openModal of
            RoadMap ->
                wrappedModal

            _ ->
                Element.none
        )


viewModalChangelog : Model -> Element.Attribute Msg
viewModalChangelog model =
    let
        modal =
            Element.column
                [ Element.width <| Element.px 700
                , Element.centerX
                , Element.padding 50
                , Element.Border.rounded 5
                , Element.Background.color Ui.Style.white
                , Element.moveDown 50
                , Element.spacing 30
                ]
                [ Element.column [ Element.spacing 20, Element.width Element.fill ]
                    [ Element.paragraph
                        [ Element.Font.center
                        , Element.Background.color Ui.Style.lightGrey
                        , Element.padding 20
                        , Element.Border.rounded 5
                        , Element.Font.size 22
                        , Element.Font.bold
                        ]
                        [ Element.text "Changelog" ]
                    , Element.column
                        [ Element.padding 20
                        , Element.Border.rounded 5
                        , Element.width Element.fill
                        , Element.spacing 20
                        , Element.scrollbarY
                        , Element.height (Element.px 500)
                        ]
                        (List.map (\( title, date, changes ) -> mapTextWithSide title date changes) SupportSection.Releases.all)
                    ]
                , Element.row [ Element.width Element.fill ] [ Element.el [] SupportSection.Components.gitHubRepo, Element.el [ Element.alignRight ] (closeButtonRight CloseModalButtonClicked) ]
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
        (case model.openModal of
            Changelog ->
                wrappedModal

            _ ->
                Element.none
        )


viewModalCommunity : Model -> Element.Attribute Msg
viewModalCommunity model =
    let
        -- discord =
        --     SupportSection.Components.buttonWithIcon Ui.Boxicons.bxlDiscourse "Discourse" "https://discord.com/"
        modal =
            Element.column
                [ Element.width <| Element.px 700
                , Element.centerX
                , Element.padding 50
                , Element.Border.rounded 5
                , Element.Background.color Ui.Style.white
                , Element.moveDown 50
                , Element.spacing 30
                ]
                [ Element.column [ Element.spacing 20, Element.width Element.fill ]
                    [ Element.paragraph
                        [ Element.Font.center
                        , Element.Background.color Ui.Style.lightGrey
                        , Element.padding 20
                        , Element.Border.rounded 5
                        , Element.Font.size 22
                        , Element.Font.bold
                        ]
                        [ Element.text "Connect with other funk users and get help directly from the funk team." ]
                    , Element.wrappedRow [ Element.spacing 20, Element.centerX, Element.paddingEach { top = 30, bottom = 10, left = 0, right = 0 } ]
                        [ Element.el [ Element.centerX ] <| SupportSection.Components.slack
                        , Element.el [ Element.centerX ] <| SupportSection.Components.twitter
                        , Element.el [ Element.centerX ] <| SupportSection.Components.reddit
                        , Element.el [ Element.centerX ] <| SupportSection.Components.gitHubDiscissions

                        -- , Element.el [ Element.centerX ] <| discord
                        ]
                    , Element.paragraph
                        [ Element.Font.center
                        , Element.padding 20
                        , Element.Border.rounded 5
                        , Element.centerX
                        , Element.width (Element.px 400)
                        ]
                        [ Element.el [] (Element.text "Or email us directly ")
                        , Element.el [ Ui.Style.style "user-select" "all" ] (Element.text "david@funklang.com")
                        ]
                    ]
                , closeButtonRight CloseModalButtonClicked
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
        (case model.openModal of
            Community ->
                wrappedModal

            _ ->
                Element.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitButtonClicked ->
            case validate model.message of
                Just message ->
                    ( { model | validation = Invalid message }, Cmd.none )

                Nothing ->
                    ( { model | validation = NotAsked, request = RemoteData.Loading }, sendToServer model.sessionInfo model.message )

        MessageChanged message ->
            ( { model | message = message, request = RemoteData.NotAsked }, Cmd.none )

        GotSessionInfo sessionInfo ->
            ( { model | sessionInfo = sessionInfo }, Cmd.none )

        ToggleExpanded ->
            ( { model | isExpanded = not model.isExpanded }, Cmd.none )

        OpenModalButtonClicked whichModal ->
            case whichModal of
                Feedback ->
                    ( { model
                        | openModal = whichModal
                        , message =
                            case model.request of
                                RemoteData.Success _ ->
                                    ""

                                _ ->
                                    model.message
                        , request =
                            case model.request of
                                RemoteData.Success _ ->
                                    RemoteData.NotAsked

                                _ ->
                                    model.request
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | openModal = whichModal }, Cmd.none )

        CloseModalButtonClicked ->
            ( { model | openModal = AllClosed }, Cmd.none )

        FeedbackResponse response ->
            ( { model
                | request = response
                , message = model.message
              }
            , Cmd.none
            )


validate : String -> Maybe String
validate msg =
    case String.length msg > 10 of
        False ->
            Just "🙏 Please write at least ten letters."

        _ ->
            Nothing


feedbackButtonStyle =
    [ Element.Border.color Ui.Style.highlightColor
    , Element.padding 6
    , Element.Border.rounded 3
    , Element.Background.color Ui.Style.highlightColorSolid
    , Element.mouseOver <|
        [ Element.Background.color Ui.Style.white, Element.Font.color Ui.Style.black ]
    , Element.pointer
    ]


hiddenBtnStyles =
    [ Element.Border.color Ui.Style.highlightColor
    , Element.padding 6
    , Element.Border.rounded 3
    , Element.alignRight
    , Element.Background.color Ui.Style.highlightColorSolid
    , Element.Background.gradient { angle = 180.0, steps = [ Ui.Style.black, Ui.Style.grey, Ui.Style.lightGrey ] }
    ]


btnExpand =
    Element.Input.button
        (feedbackButtonStyle
            ++ [ Element.alignRight ]
        )
        { label =
            Element.el
                [ Element.alignRight
                , Element.moveLeft 6
                , Element.height (Element.px 16)
                , Element.width (Element.px 10)
                ]
                (Ui.Component.icon Ui.Boxicons.bxsChevronRight)
        , onPress = Just ToggleExpanded
        }
        |> Element.el [ Element.paddingXY 0 5 ]


viewExpandOrCloseButton isExpanded =
    case isExpanded of
        False ->
            Element.row [ Element.paddingXY 10 0 ]
                [ Element.el
                    [ Element.inFront (Element.el [ Element.alignRight, Element.moveUp 5 ] btnExpand)
                    ]
                    (Element.el hiddenBtnStyles (Element.text "Co......"))
                ]

        True ->
            Element.Input.button
                feedbackButtonStyle
                { label = Element.el [ Element.centerY, Element.centerX, Element.moveLeft 6, Element.height (Element.px 17), Element.width (Element.px 10) ] (Ui.Component.icon Ui.Boxicons.bxsChevronLeft), onPress = Just ToggleExpanded }
                |> Element.el [ Element.paddingXY 10 5 ]


viewOpenButton =
    Element.Input.button
        feedbackButtonStyle
        { label = Element.text "Feedback", onPress = Just (OpenModalButtonClicked Feedback) }
        |> Element.el [ Element.paddingXY 10 5 ]


docsButton =
    Element.Input.button
        feedbackButtonStyle
        { label = Element.text "Docs", onPress = Just (OpenModalButtonClicked Docs) }
        |> Element.el [ Element.paddingXY 10 5 ]


communityButton isExpanded =
    let
        text =
            case isExpanded of
                True ->
                    "Community"

                False ->
                    "Co.."
    in
    Element.Input.button
        feedbackButtonStyle
        { label = Element.text text, onPress = Just (OpenModalButtonClicked Community) }
        |> Element.el [ Element.paddingXY 10 5 ]


roadMapButton =
    Element.Input.button
        feedbackButtonStyle
        { label = Element.text "Roadmap", onPress = Just (OpenModalButtonClicked RoadMap) }
        |> Element.el [ Element.paddingXY 10 5 ]


changelogButton =
    Element.Input.button
        feedbackButtonStyle
        { label = Element.text "Changelog", onPress = Just (OpenModalButtonClicked Changelog) }
        |> Element.el [ Element.paddingXY 10 5 ]


subscriptions =
    gotSessionInfo GotSessionInfo
