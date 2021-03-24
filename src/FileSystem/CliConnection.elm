port module FileSystem.CliConnection exposing (Msg, State(..), init, subs, update, view)

{-| Manage connections with the funk CLI
-}

import Element
import Element.Border
import Element.Input
import Ui.Component
import Ui.Style


init : State
init =
    NotAsked


maxFailures =
    5


type State
    = NotAsked
    | Connecting { attempts : Int }
    | Connected
    | Reconnecting { attempts : Int }
    | Closed


type Msg
    = ConnectClick
    | Connect
    | ReconnectError
    | ConnectError
    | CloseClick


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case ( msg, state ) of
        ( ConnectClick, _ ) ->
            ( Connecting { attempts = 0 }, cli_connection_requested () )

        ( Connect, _ ) ->
            ( Connected, Cmd.none )

        ( ReconnectError, Reconnecting { attempts } ) ->
            ( Reconnecting { attempts = attempts + 1 }, Cmd.none )

        ( ConnectError, Connecting { attempts } ) ->
            ( Connecting { attempts = attempts + 1 }, Cmd.none )

        ( CloseClick, _ ) ->
            ( Closed, cli_connection_termination_requested () )

        _ ->
            ( state, Cmd.none )


view : State -> Element.Element Msg
view state =
    let
        panelStyles =
            [ Ui.Style.shadowMedium
            , Element.spacing 5
            , Element.Border.rounded 2
            , Element.padding 10
            ]

        button =
            Element.Input.button
                Ui.Component.buttonStyle
                { onPress = Just ConnectClick
                , label = Element.text "Connect to CLI"
                }

        closeButton =
            Element.Input.button
                Ui.Component.buttonStyle
                { onPress = Just CloseClick
                , label = Element.text "Disconnect CLI"
                }

        contents =
            case state of
                NotAsked ->
                    Element.column panelStyles
                        [ Element.text "No connection right now"
                        , button
                        ]

                Closed ->
                    Element.column panelStyles
                        [ Element.text "CLI properly disconnected"
                        , button
                        ]

                Connecting { attempts } ->
                    if attempts > maxFailures then
                        Element.column panelStyles [ Element.text "Connection failed", button ]

                    else
                        Element.column panelStyles
                            [ Element.text "Connecting"
                            , Element.text ("Attempts: " ++ String.fromInt attempts)
                            ]

                Reconnecting { attempts } ->
                    if attempts > maxFailures then
                        Element.column panelStyles [ Element.text "Connection lost", button ]

                    else
                        Element.column panelStyles
                            [ Element.text "Reconnecting"
                            , Element.text ("Attempts: " ++ String.fromInt attempts)
                            ]

                Connected ->
                    Element.column panelStyles
                        [ Element.text "Cli connected"
                        , closeButton
                        ]
    in
    contents


port cli_connected : (() -> msg) -> Sub msg


port cli_connect_error_occurred : (() -> msg) -> Sub msg


port cli_connection_requested : () -> Cmd msg


port cli_connection_termination_requested : () -> Cmd msg


subs : Sub Msg
subs =
    Sub.batch
        [ cli_connected (always Connect)
        , cli_connect_error_occurred (always ConnectError)
        ]
