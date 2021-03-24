module Main exposing (..)

import Authentication
import Browser
import Browser.Navigation
import Canvas
import Canvas.Msg
import Canvas.View
import Element
import Generator.Preview
import Google.Fonts
import Header
import Html
import Html.Attributes
import Json.Decode as Decode
import Model
import Model.Model
import Msg
import Persistence
import Persistence.Repository
import Preview
import Projects
import Projects.Msg
import RemoteData
import ResponsifyTestingEnvironment
import Route
import Time
import Ui
import Url


main : Program Model.Model.Flags Model.Model.Model Msg.Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , onUrlRequest = Canvas.Msg.LinkClicked >> Msg.EditorMsg
        , onUrlChange = Canvas.Msg.UrlChanged >> Msg.EditorMsg
        , subscriptions = subscriptions
        }


init : Model.Model.Flags -> Url.Url -> Browser.Navigation.Key -> ( Model.Model.Model, Cmd Msg.Msg )
init flags url key =
    let
        ( model, canvasCmds ) =
            Canvas.init flags url key

        cmds =
            Cmd.batch
                [ canvasCmds |> Cmd.map Msg.EditorMsg
                , projectsCmd
                , Google.Fonts.request |> Cmd.map Msg.GotGoogleFonts
                ]

        projectsCmd =
            case Route.parse url of
                Route.Home ->
                    Projects.init model.authentication |> Cmd.map Msg.ProjectsMsg

                Route.ResponsifyTestingEnvironment ->
                    ResponsifyTestingEnvironment.initCmd |> Cmd.map Msg.ResponsifyTestingEnvironmentMsg

                _ ->
                    Cmd.none
    in
    ( model
    , cmds
    )


update : Msg.Msg -> Model.Model.Model -> ( Model.Model.Model, Cmd Msg.Msg )
update msg model =
    checkoutHook msg <|
        case msg of
            Msg.GotGoogleFonts googleFonts ->
                ( { model | googleFonts = googleFonts }
                , Cmd.none
                )

            Msg.NoOp ->
                ( model, Cmd.none )

            Msg.ResponsifyTestingEnvironmentMsg msg_ ->
                let
                    ( responsifyTests, cmd ) =
                        ResponsifyTestingEnvironment.update msg_ model.responsifyTests
                in
                ( { model | responsifyTests = responsifyTests }, cmd |> Cmd.map Msg.ResponsifyTestingEnvironmentMsg )

            Msg.SecondPassed now ->
                ( { model | now = now }, Cmd.none )

            Msg.EditorMsg msg_ ->
                let
                    ( canvasModel, canvasCmds ) =
                        Canvas.rootUpdate msg_ model
                            |> Tuple.mapSecond (Cmd.map Msg.EditorMsg)

                    projectsCmds =
                        case msg_ of
                            Canvas.Msg.AuthStateChanged auth ->
                                Projects.init auth
                                    |> Cmd.map Msg.ProjectsMsg

                            Canvas.Msg.UrlChanged _ ->
                                Projects.init model.authentication
                                    |> Cmd.map Msg.ProjectsMsg

                            _ ->
                                Cmd.none
                in
                ( canvasModel, Cmd.batch [ canvasCmds, projectsCmds ] )

            Msg.ProjectsMsg msg_ ->
                let
                    ( ( projects, auth ), cmd ) =
                        Projects.update
                            msg_
                            model.projects
                            model.authentication
                in
                ( { model | projects = projects, authentication = auth }
                , Cmd.map Msg.ProjectsMsg cmd
                )



-- when the user gets authed or navigates to a project, check it out
-- whenever the user is outside a project, clear it


checkoutHook msg ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            case msg of
                Msg.EditorMsg (Canvas.Msg.UrlChanged url) ->
                    case Route.parse url of
                        Route.Project _ _ ->
                            checkout model

                        Route.Home ->
                            ( { model | project = RemoteData.NotAsked }, Cmd.none )

                        Route.Ui ->
                            ( { model | project = RemoteData.NotAsked }, Cmd.none )

                        Route.ResponsifyTestingEnvironment ->
                            ( { model | project = RemoteData.NotAsked }, Cmd.none )

                Msg.EditorMsg (Canvas.Msg.AuthStateChanged _) ->
                    checkout model

                Msg.ProjectsMsg (Projects.Msg.ProjectsReceived _) ->
                    checkout model

                _ ->
                    ( model, Cmd.none )
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


checkout : Model.Model.Model -> ( Model.Model.Model, Cmd Msg.Msg )
checkout model =
    case model.project of
        RemoteData.NotAsked ->
            case Route.getProjectData model.url of
                Just projectMeta ->
                    ( { model | project = RemoteData.Loading }, Persistence.checkout projectMeta )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


decodeUserInfo : Decode.Decoder Authentication.UserInfo
decodeUserInfo =
    Decode.map5 Authentication.UserInfo
        (Decode.field "displayName" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "uid" Decode.string)
        (Decode.field "showOnboarding" Decode.bool)
        (Decode.field "onBoardingScreen" Decode.int)


handleAuthStateChange : Maybe Decode.Value -> Msg.Msg
handleAuthStateChange state =
    case state of
        Nothing ->
            Authentication.Anonymous |> Canvas.Msg.AuthStateChanged |> Msg.EditorMsg

        Just userInfo ->
            case Decode.decodeValue decodeUserInfo userInfo of
                Err _ ->
                    Msg.NoOp

                Ok decodedInfo ->
                    Authentication.LoggedIn decodedInfo
                        |> Canvas.Msg.AuthStateChanged
                        |> Msg.EditorMsg


view : Model.Model.Model -> Browser.Document Msg.Msg
view model =
    let
        doc =
            case Route.parse model.url of
                Route.ResponsifyTestingEnvironment ->
                    { body =
                        ResponsifyTestingEnvironment.view model
                            |> Element.map Msg.ResponsifyTestingEnvironmentMsg
                            |> Element.layout [ Element.width Element.fill, Element.height Element.fill ]
                            |> List.singleton
                    , title = "Responsify tests"
                    }

                Route.Home ->
                    { title = "Funk home"
                    , body =
                        Element.column [ Element.width Element.fill, Element.height Element.fill ]
                            [ Header.view model
                            , Projects.view
                                model.now
                                model.authentication
                                model.projects
                                |> Element.map Msg.ProjectsMsg
                            ]
                            |> Element.layout [ Element.width Element.fill, Element.height Element.fill ]
                            |> List.singleton
                    }

                Route.Project project (Route.Preview elementId) ->
                    let
                        baseUrl =
                            "/preview/" ++ project.projectId ++ "/" ++ project.projectName
                    in
                    Preview.view
                        (\slug -> Route.makeUrl project (Route.Preview slug))
                        elementId
                        (RemoteData.map (always (Model.latest model)) model.project)
                        |> (\{ body, title } ->
                                { title = title
                                , body =
                                    List.map
                                        (Html.map (Canvas.Msg.PreviewMsg >> Msg.EditorMsg))
                                        body
                                }
                           )

                Route.Project project (Route.Editor editorMode) ->
                    Canvas.View.view editorMode
                        project
                        (Model.mapPending Model.initRuntimeModel model)
                        |> (\{ body, title } -> { title = title, body = List.map (Html.map Msg.EditorMsg) body })

                Route.Ui ->
                    Ui.viewGallery
                        |> (\{ body, title } -> { title = title, body = List.map (Html.map (always (Canvas.Msg.NoOp |> (Canvas.Msg.EditorMsg >> Msg.EditorMsg)))) body })

                Route.Project projectMeta (Route.CodeGenPreview maybeScreenIndex) ->
                    Model.latest model
                        |> Generator.Preview.view ( model.url, model.key ) maybeScreenIndex
                        |> Element.map (Canvas.Msg.RunCmd >> Msg.EditorMsg)
                        |> Element.layout []
                        |> List.singleton
                        |> (\content -> { title = "Funk Generated Code Preview", body = content })

        regularAuthFlow =
            case model.authentication of
                Authentication.Anonymous ->
                    viewLogin

                Authentication.Undetermined ->
                    viewWaiting

                Authentication.LoggedIn userData ->
                    doc
    in
    regularAuthFlow


viewWaiting =
    let
        loading =
            Html.div
                [ Html.Attributes.style "font-size" "40px"
                , Html.Attributes.style "margin-top" "200px"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "font-family" "Arial"
                ]
                [ Html.text "Loading" ]

        subText =
            Html.div
                [ Html.Attributes.style "font-size" "22px"
                , Html.Attributes.style "padding-top" "30px"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "font-family" "Arial"
                ]
                [ Html.text "Please bear with us. We are still in beta"
                ]

        waitImage =
            Html.img
                [ Html.Attributes.src "/loading.gif"
                , Html.Attributes.style "display" "block"
                , Html.Attributes.style "margin-left" "auto"
                , Html.Attributes.style "margin-right" "auto"
                ]
                []
    in
    { body = [ loading, subText, waitImage ], title = "funkLang Beta" }


viewLogin =
    let
        terms =
            Html.div
                [ Html.Attributes.style "font-size" "12px"
                , Html.Attributes.style "padding-top" "30px"
                ]
                [ Html.text "By signing into funk you agree to our "
                , Html.a [ Html.Attributes.href "https://www.funklang.com/terms" ] [ Html.text " terms and conditions" ]
                ]

        body =
            Html.node "funk-authentication-element"
                []
                [ Html.text "funkLang Beta"
                , Html.div [ Html.Attributes.id "funk-auth-container" ] []
                , terms
                ]

        title =
            "funkLang Login"
    in
    { body = [ body ], title = title }


subscriptions model =
    let
        authSubs =
            Authentication.authStateChanged handleAuthStateChange

        routeSubs =
            case Route.parse model.url of
                Route.Home ->
                    [ Projects.subscriptions
                        |> Sub.map Msg.ProjectsMsg
                    , Time.every 1000 Msg.SecondPassed
                    ]
                        |> Sub.batch

                Route.Project _ (Route.Preview _) ->
                    Preview.subscriptions model.scrollTo
                        |> Sub.map Canvas.Msg.PreviewMsg
                        |> Sub.map Msg.EditorMsg

                Route.Project _ (Route.CodeGenPreview _) ->
                    Sub.none

                Route.Project _ _ ->
                    Canvas.subscriptions model
                        |> Sub.map Msg.EditorMsg

                Route.Ui ->
                    Sub.none

                Route.ResponsifyTestingEnvironment ->
                    Sub.none

        repoFetchedSub =
            Persistence.Repository.newProjectSub model
                |> Sub.map Msg.EditorMsg
    in
    Sub.batch [ authSubs, routeSubs, repoFetchedSub ]
