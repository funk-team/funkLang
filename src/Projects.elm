module Projects exposing (..)

import Authentication
import Bounce
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import List.Extra
import Model.Model
import Model.Product
import Projects.Api
import Projects.Model
import Projects.Msg
import Projects.WelcomeModal
import RemoteData
import Route
import Time
import Time.Distance
import Ui.BigTabs
import Ui.Boxicons
import Ui.Component
import Ui.Style
import SupportSection.Components

subscriptions =
    Sub.none


init : Authentication.State -> Cmd Projects.Msg.Msg
init auth =
    case auth of
        Authentication.LoggedIn userInfo ->
            Projects.Api.list userInfo.id

        Authentication.Anonymous ->
            Cmd.none

        Authentication.OpenCoreUser ->
            Cmd.none

        Authentication.Undetermined ->
            Cmd.none


initModel : Projects.Model.Model
initModel =
    Projects.Model.Model
        RemoteData.NotAsked
        RemoteData.NotAsked
        Nothing
        Bounce.init


update : Projects.Msg.Msg -> Projects.Model.Model -> Authentication.State -> ( ( Projects.Model.Model, Authentication.State ), Cmd Projects.Msg.Msg )
update msg model auth =
    case auth of
        Authentication.OpenCoreUser ->
            ( ( model, auth ), Cmd.none )

        Authentication.Undetermined ->
            ( ( model, auth ), Cmd.none )

        Authentication.Anonymous ->
            ( ( model, auth ), Cmd.none )

        Authentication.LoggedIn userInfo ->
            case msg of
                Projects.Msg.NoOp ->
                    ( ( model, auth ), Cmd.none )

                Projects.Msg.DeleteClicked int ->
                    ( ( { model | deletePending = Just ( int, RemoteData.NotAsked ) }, auth ), Cmd.none )

                Projects.Msg.DeleteConfirmClicked ->
                    let
                        ( deletePending, cmd ) =
                            case model.deletePending of
                                Just ( id, remoteData ) ->
                                    ( Just ( id, RemoteData.Loading )
                                    , Projects.Api.deleteProject id userInfo.id
                                    )

                                Nothing ->
                                    ( Nothing, Cmd.none )
                    in
                    ( ( { model | deletePending = deletePending }, auth ), cmd )

                Projects.Msg.ProjectDeleted id (RemoteData.Success _) ->
                    ( ( { model
                            | deletePending = Nothing
                            , projects =
                                RemoteData.map (List.filter (\project -> project.id /= id))
                                    model.projects
                        }
                      , auth
                      )
                    , Cmd.none
                    )

                Projects.Msg.ProjectDeleted _ response ->
                    ( ( { model
                            | deletePending =
                                Maybe.map
                                    (\( id, _ ) -> ( id, response ))
                                    model.deletePending
                        }
                      , auth
                      )
                    , Cmd.none
                    )

                Projects.Msg.DeleteAborted ->
                    ( ( { model | deletePending = Nothing }, auth ), Cmd.none )

                Projects.Msg.ProjectUpdated id response ->
                    case response of
                        RemoteData.Success updatedProject ->
                            ( ( { model
                                    | projects =
                                        RemoteData.map
                                            (List.Extra.updateIf (\p -> p.id == id) (always updatedProject))
                                            model.projects
                                }
                              , auth
                              )
                            , Cmd.none
                            )

                        _ ->
                            ( ( model, auth ), Cmd.none )

                Projects.Msg.Renamed id newName ->
                    ( ( { model
                            | projects =
                                RemoteData.map
                                    (List.Extra.updateIf
                                        (\project -> id == project.id)
                                        (\project ->
                                            { project
                                                | name = newName
                                                , pendingChanges = RemoteData.Loading
                                            }
                                        )
                                    )
                                    model.projects
                            , bounce = Bounce.push model.bounce
                        }
                      , auth
                      )
                    , Bounce.delay 1000 Projects.Msg.BounceMsg
                    )

                Projects.Msg.BounceMsg ->
                    let
                        bounce =
                            Bounce.pop model.bounce

                        isSteady =
                            Bounce.steady bounce

                        cmd =
                            if isSteady then
                                model.projects
                                    |> RemoteData.toMaybe
                                    |> Maybe.withDefault []
                                    |> List.filterMap
                                        (\project ->
                                            case ( project.name, project.pendingChanges ) of
                                                ( "", _ ) ->
                                                    Nothing

                                                ( atLeastOneLetter, RemoteData.Loading ) ->
                                                    Just (Projects.Api.patchProject userInfo.id project)

                                                _ ->
                                                    Nothing
                                        )
                                    |> Cmd.batch

                            else
                                Cmd.none
                    in
                    ( ( { model | bounce = bounce }, auth ), cmd )

                Projects.Msg.CreateProjectButtonClicked ->
                    ( ( { model | newProject = RemoteData.Loading }, auth ), Projects.Api.createProject userInfo.id )

                Projects.Msg.ProjectCreated (RemoteData.Success project) ->
                    ( ( { model
                            | newProject = RemoteData.NotAsked
                            , projects = RemoteData.map (\projects -> project :: projects) model.projects
                        }
                      , auth
                      )
                    , Cmd.none
                    )

                Projects.Msg.ProjectCreated project ->
                    ( ( { model
                            | newProject = project
                        }
                      , auth
                      )
                    , Cmd.none
                    )

                Projects.Msg.ProjectsReceived projects ->
                    ( ( { model | projects = projects }, auth ), Cmd.none )

                Projects.Msg.ToggleOnBoardingScreen ->
                    let
                        authModel_ =
                            case auth of
                                Authentication.LoggedIn userInfo_ ->
                                    Authentication.LoggedIn
                                        { userInfo_ | showOnboarding = not userInfo_.showOnboarding, onBoardingScreen = 1 }

                                _ ->
                                    auth
                    in
                    ( ( model, authModel_ ), Cmd.none )

                Projects.Msg.UpdateOnBoardingScreen screen ->
                    let
                        authModel_ =
                            case auth of
                                Authentication.LoggedIn userInfo_ ->
                                    Authentication.LoggedIn { userInfo_ | onBoardingScreen = screen }

                                _ ->
                                    auth
                    in
                    ( ( model, authModel_ ), Cmd.none )


leftSidebar =
    Element.column
        [ Element.Border.widthEach
            { edges | right = 1 }
        , Element.Border.color (Element.rgba 0 0 0 0.6)
        , Element.spacing 20
        , Element.width <| Element.px 201
        , Element.height <| identity <| identity <| Element.fill
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.Font.bold
            , Element.padding 10
            , Element.Background.color Ui.Style.slightAccent
            , Element.height Element.shrink
            ]
            (Element.text "Recent")
        , Element.el [ Element.centerX ] Projects.WelcomeModal.onBoardingButton
        , Element.el [ Element.centerX ] SupportSection.Components.gitHubRepo
        ]


edges =
    { bottom = 0
    , top = 0
    , left = 0
    , right = 0
    }


view : Model.Model.Model -> Element.Element Projects.Msg.Msg
view { now, authentication, mode, projects } =
    case authentication of
        Authentication.OpenCoreUser ->
            Element.none

        Authentication.Anonymous ->
            Element.none

        Authentication.Undetermined ->
            Element.none

        Authentication.LoggedIn userInfo ->
            Element.column
                [ Element.spacing 0
                , Element.width Element.fill
                , Element.height Element.fill
                , case userInfo.showOnboarding of
                    True ->
                        Projects.WelcomeModal.onBoarding userInfo.onBoardingScreen

                    False ->
                        Element.spacing 0
                ]
                [ tabs
                , mainBody now userInfo mode projects
                ]


tabs =
    Element.el
        [ Element.Border.widthEach
            { edges | bottom = 1 }
        , Element.Border.color (Element.rgba 0 0 0 0.6)
        , Element.padding 10
        , Element.spacing 0
        , Element.width <| identity <| identity <| Element.fill
        , Element.height <| Element.px 61
        ]
        (Ui.BigTabs.view
            [ { label = "PRIVATE"
              , onClick = Projects.Msg.NoOp
              , sideNote = Nothing
              , isSelected = True
              }
            , { label = "SHARED"
              , onClick = Projects.Msg.NoOp
              , sideNote = Just "coming soon"
              , isSelected = False
              }
            ]
        )


mainBody : Time.Posix -> Authentication.UserInfo -> Model.Product.Mode -> Projects.Model.Model -> Element.Element Projects.Msg.Msg
mainBody now userInfo mode model =
    let
        button =
            Element.Input.button
                Ui.Component.buttonStyle
                { label = Element.text "Create project"
                , onPress = Just Projects.Msg.CreateProjectButtonClicked
                }

        newProjectButton =
            case model.newProject of
                RemoteData.Failure f ->
                    Element.row [] [ Element.text "Something went wrong. Try again.", button ]

                RemoteData.Success _ ->
                    button

                RemoteData.NotAsked ->
                    button

                RemoteData.Loading ->
                    Element.text "Creeating new project"

        rightSection =
            Element.column
                [ Element.padding 10
                , Element.spacing 10
                , Element.width <| identity <| identity <| Element.fill
                , Element.height <| identity <| identity <| Element.fill
                ]
                [ newProjectButton
                , viewProjects now userInfo mode model
                ]
    in
    Element.row
        [ Element.spacing 0
        , Element.width <| identity <| identity <| Element.fill
        , Element.height <| identity <| identity <| Element.fill
        ]
        [ leftSidebar
        , rightSection
        ]


viewProjects : Time.Posix -> Authentication.UserInfo -> Model.Product.Mode -> Projects.Model.Model -> Element.Element Projects.Msg.Msg
viewProjects now userInfo mode { projects, deletePending } =
    let
        viewProject : Projects.Model.Project -> Element.Element Projects.Msg.Msg
        viewProject p =
            let
                viewCardIcon =
                    Ui.Component.icon
                        >> Element.el
                            [ Element.centerX
                            , Element.centerY
                            , Ui.Style.class "funk-project-card-icon"
                            ]

                cardTopStyles =
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.Border.widthEach { edges | bottom = 1 }
                    , Element.Border.color Ui.Style.slightAccent
                    , Ui.Style.class "funk-project-card"
                    , Element.inFront timestamp
                    ]

                timestamp =
                    Element.text
                        (case
                            Time.Distance.inWords
                                (p.updatedAt
                                    -- add some tolerance into the past because we update the time every second
                                    -- so without adding this tolerance, a newly created project can be up to 999ms in the future :)
                                    |> Time.posixToMillis
                                    |> (+) -1000
                                    |> Time.millisToPosix
                                )
                                now
                         of
                            "less than 5 seconds ago" ->
                                "just now"

                            otherWords ->
                                otherWords
                        )
                        |> Element.el
                            [ Element.padding 10
                            , Element.alignBottom
                            , Element.Font.color Ui.Style.grey
                            , Element.Font.size 12
                            ]

                link =
                    Element.link
                        cardTopStyles
                        { label =
                            viewCardIcon Ui.Boxicons.bxRightArrowAlt
                        , url = Route.makeUrl mode { projectId = p.id, projectName = p.name } (Route.Editor Route.Canvas)
                        }

                deleteButton =
                    Element.Input.button []
                        { onPress = Just (Projects.Msg.DeleteClicked p.id)
                        , label = Ui.Component.icon Ui.Boxicons.bxTrash
                        }

                deleteButtonOrStatus =
                    case p.pendingChanges of
                        RemoteData.NotAsked ->
                            deleteButton

                        RemoteData.Success _ ->
                            deleteButton

                        RemoteData.Loading ->
                            Ui.Component.icon Ui.Boxicons.bxRefresh

                        RemoteData.Failure _ ->
                            Ui.Component.icon Ui.Boxicons.bxSad

                confirmDeleteButton =
                    Element.Input.button [ Element.Font.color Ui.Style.highlightColorSolidImportant ]
                        { onPress = Just Projects.Msg.DeleteConfirmClicked
                        , label = Element.text "Really delete?"
                        }

                abortDeleteButton =
                    Element.Input.button [ Element.Font.color Ui.Style.highlightColorSolid ]
                        { onPress = Just Projects.Msg.DeleteAborted
                        , label = Element.text "NO!"
                        }

                renameInput =
                    Element.Input.text
                        [ Element.padding 0, Element.Border.width 0, Element.height Element.fill ]
                        { text = p.name
                        , label = Element.Input.labelHidden "Project name"
                        , placeholder = Just <| Element.Input.placeholder [] (Element.text "Project name")
                        , onChange = Projects.Msg.Renamed p.id
                        }

                result =
                    Element.column
                        [ Element.Border.rounded 3
                        , Element.Border.color Ui.Style.slightAccent
                        , Element.Border.width 1
                        , Element.spacing 0
                        , Element.width <| Element.px 200
                        , Element.height <| Element.px 200
                        , Ui.Style.shadowMedium
                        ]
                        [ link

                        -- TODO Return if project is example in API and perform rename check using this marker
                        , if p.name == "Example Project" then
                            defaultProjectActions

                          else
                            actions
                        ]

                defaultProjectActions =
                    Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 40
                        , Element.padding 10
                        , Element.spacing 10
                        ]
                        [ Element.text p.name ]

                actions =
                    Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 40
                        , Element.padding 10
                        , Element.spacing 10
                        ]
                        (case deletePending of
                            Nothing ->
                                [ renameInput
                                , deleteButtonOrStatus
                                ]

                            Just ( id, remoteData ) ->
                                if id == p.id then
                                    case remoteData of
                                        RemoteData.NotAsked ->
                                            [ confirmDeleteButton, abortDeleteButton ]

                                        RemoteData.Success _ ->
                                            [ Element.text "Deleted" ]

                                        RemoteData.Loading ->
                                            [ Element.text "Deleting" ]

                                        RemoteData.Failure f ->
                                            [ Element.text "Could not delete" ]

                                else
                                    [ renameInput
                                    , deleteButton
                                    ]
                        )
            in
            result
    in
    case projects of
        RemoteData.Success ps ->
            Element.wrappedRow
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                (ps |> List.map viewProject)

        RemoteData.Failure f ->
            Element.text "Could not fetch projects... Please contact us on slack."

        _ ->
            Element.text "Loading projects..."
