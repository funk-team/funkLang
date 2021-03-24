module DeployEditor exposing (Model, decodeModel, encodeModel, init, update, view)

import Cmd.Extra
import DeployEditor.Deploy
import DeployEditor.Domain
import DeployEditor.Menus
import DeployEditor.Msg
import Element
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.BigTabs
import Url



-- [generator-start]


type alias Model =
    { deploy : DeployEditor.Deploy.Model
    , menu : DeployEditor.Menus.Menu
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.map2
        Model
        (Decode.field "deploy" DeployEditor.Deploy.decodeModel)
        (Decode.field "menu" DeployEditor.Menus.decodeMenu)


encodeModel a =
    Encode.object
        [ ( "deploy", DeployEditor.Deploy.encodeModel a.deploy )
        , ( "menu", DeployEditor.Menus.encodeMenu a.menu )
        ]



-- [generator-end]


update : DeployEditor.Msg.Msg -> Url.Url -> Model -> ( Model, Cmd DeployEditor.Msg.Msg )
update msg url model =
    case msg of
        DeployEditor.Msg.SwitchMenu selectedMenu ->
            { model | menu = selectedMenu }
                |> Cmd.Extra.withNoCmd

        DeployEditor.Msg.GotDeployEditorMsg msg_ ->
            let
                ( mdl, cmd ) =
                    DeployEditor.Deploy.update
                        msg_
                        url
                        model.deploy
            in
            ( { model | deploy = mdl }
            , Cmd.map DeployEditor.Msg.GotDeployEditorMsg cmd
            )

        DeployEditor.Msg.GotDomainEditorMsg msg_ ->
            let
                ( mdl, cmd ) =
                    DeployEditor.Domain.update
                        msg_
                        model.deploy
            in
            ( model
            , Cmd.map DeployEditor.Msg.GotDomainEditorMsg cmd
            )



---- PROGRAM ----


init : () -> ( Model, Cmd DeployEditor.Msg.Msg )
init flags =
    let
        initModel : Model
        initModel =
            Model
                DeployEditor.Deploy.init
                DeployEditor.Menus.Deploy
    in
    ( initModel
    , Cmd.none
    )


view : Model -> Element.Element DeployEditor.Msg.Msg
view model =
    let
        content =
            case model.menu of
                DeployEditor.Menus.Deploy ->
                    DeployEditor.Deploy.view model.deploy
                        |> Element.map DeployEditor.Msg.GotDeployEditorMsg

                DeployEditor.Menus.Domain ->
                    DeployEditor.Domain.view
                        |> Element.map DeployEditor.Msg.GotDomainEditorMsg

        options =
            DeployEditor.Menus.menus
                |> List.map
                    (\o ->
                        { isSelected = o == model.menu
                        , onClick = DeployEditor.Menus.Deploy
                        , label = DeployEditor.Menus.toString o
                        , sideNote =
                            case o of
                                DeployEditor.Menus.Domain ->
                                    Just "coming soon"

                                DeployEditor.Menus.Deploy ->
                                    Nothing
                        }
                    )

        ui =
            Element.column
                [ Element.padding 20
                , Element.spacing 48
                ]
                [ Ui.BigTabs.view
                    options
                    |> Element.map DeployEditor.Msg.SwitchMenu
                , content
                ]
    in
    ui
