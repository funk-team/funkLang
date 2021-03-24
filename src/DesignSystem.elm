module DesignSystem exposing
    ( Model
    , decodeModel
    , encodeModel
    , init
    , update
    , view
    )

import Cmd.Extra
import DesignSystem.Color
import DesignSystem.Color.Model
import DesignSystem.IconBrowser
import DesignSystem.IconBrowser.Model
import DesignSystem.Menus
import DesignSystem.Msg
import DesignSystem.Shadow
import DesignSystem.Typography
import Element
import Google.Fonts
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.BigTabs



-- [generator-start]


type alias Model =
    { typoEditor : DesignSystem.Typography.Model
    , colorEditor : DesignSystem.Color.Model.Model
    , iconBrowser : DesignSystem.IconBrowser.Model.Model
    , shadows : DesignSystem.Shadow.Model
    , menu : DesignSystem.Menus.Menu
    }


update : DesignSystem.Msg.Msg -> Model -> ( Model, Cmd DesignSystem.Msg.Msg )
update msg model =
    case msg of
        DesignSystem.Msg.GotTypoMsg ( msg_, swatchUpdate ) ->
            let
                ( ( typographyModel, updatedColorModel ), cmd ) =
                    DesignSystem.Typography.update
                        msg_
                        model.typoEditor
                        swatchUpdate

                -- model.colorEditor
            in
            ( { model
                | typoEditor = typographyModel
                , colorEditor = updatedColorModel
              }
            , Cmd.map (\cmdMsg -> DesignSystem.Msg.GotTypoMsg ( cmdMsg, updatedColorModel )) cmd
            )

        DesignSystem.Msg.GotColorEditorMsg msg_ ->
            let
                ( mdl, cmd ) =
                    DesignSystem.Color.update
                        msg_
                        model.colorEditor
            in
            ( { model | colorEditor = mdl }
            , Cmd.map DesignSystem.Msg.GotColorEditorMsg cmd
            )

        DesignSystem.Msg.GotIconBrowserMsg msg_ ->
            let
                ( mdl, cmd ) =
                    DesignSystem.IconBrowser.update
                        msg_
                        model.iconBrowser
            in
            ( { model | iconBrowser = mdl }
            , Cmd.map DesignSystem.Msg.GotIconBrowserMsg cmd
            )

        DesignSystem.Msg.GotShadowMsg ( msg_, updatedColorModel ) ->
            let
                ( mdl, cmd ) =
                    DesignSystem.Shadow.update
                        msg_
                        model.shadows
            in
            ( { model
                | shadows = mdl
                , colorEditor = updatedColorModel
              }
            , Cmd.map (\cmd_ -> DesignSystem.Msg.GotShadowMsg ( cmd_, updatedColorModel )) cmd
            )

        DesignSystem.Msg.SwitchMenu selectedMenu ->
            let
                applyCmd =
                    if selectedMenu == DesignSystem.Menus.Icons then
                        model.iconBrowser.githubRepos
                            |> DesignSystem.IconBrowser.requestRepos
                            |> Cmd.batch
                            |> Cmd.map DesignSystem.Msg.GotIconBrowserMsg
                            |> Cmd.Extra.withCmd

                    else
                        Cmd.Extra.withNoCmd
            in
            { model | menu = selectedMenu }
                |> applyCmd



---- PROGRAM ----


init : () -> ( Model, Cmd DesignSystem.Msg.Msg )
init flags =
    let
        ( iconBrowser, iconBrowserCmd ) =
            DesignSystem.IconBrowser.init

        ( typoModel, typoCmd ) =
            DesignSystem.Typography.init

        colorSystem =
            DesignSystem.Color.init

        initModel : Model
        initModel =
            Model
                typoModel
                colorSystem
                iconBrowser
                DesignSystem.Shadow.init
                DesignSystem.Menus.Typography
    in
    ( initModel
    , [ Cmd.map DesignSystem.Msg.GotIconBrowserMsg iconBrowserCmd
      , Cmd.map (\msg -> DesignSystem.Msg.GotTypoMsg ( msg, colorSystem )) typoCmd
      ]
        |> Cmd.batch
    )


view : Google.Fonts.Fonts -> Model -> Element.Element DesignSystem.Msg.Msg
view googleFonts model =
    let
        content =
            case model.menu of
                DesignSystem.Menus.Colors ->
                    DesignSystem.Color.view model.colorEditor
                        |> Element.map DesignSystem.Msg.GotColorEditorMsg

                DesignSystem.Menus.Icons ->
                    DesignSystem.IconBrowser.view model.iconBrowser
                        |> Element.map DesignSystem.Msg.GotIconBrowserMsg

                DesignSystem.Menus.Typography ->
                    DesignSystem.Typography.view googleFonts model.typoEditor model.colorEditor
                        |> Element.map (\cmdMsg -> DesignSystem.Msg.GotTypoMsg ( cmdMsg, model.colorEditor ))

                DesignSystem.Menus.Shadows ->
                    DesignSystem.Shadow.view model.shadows model.colorEditor
                        |> Element.map DesignSystem.Msg.GotShadowMsg

        options =
            DesignSystem.Menus.menus
                |> List.map
                    (\o ->
                        { isSelected = o == model.menu
                        , onClick = o
                        , label = DesignSystem.Menus.toString o
                        , sideNote = Nothing
                        }
                    )
    in
    Element.column
        [ Element.padding 0
        , Element.spacing 10
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Ui.BigTabs.view
            options
            |> Element.map DesignSystem.Msg.SwitchMenu
            |> Element.el [ Element.padding 20 ]
        , content
        ]



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.map5
        Model
        (Decode.field "typoEditor" DesignSystem.Typography.decodeModel)
        (Decode.field "colorEditor" DesignSystem.Color.Model.decodeModel)
        (Decode.field "iconBrowser" DesignSystem.IconBrowser.Model.decodeModel)
        (Decode.field "shadows" DesignSystem.Shadow.decodeModel)
        (Decode.field "menu" DesignSystem.Menus.decodeMenu)


encodeModel a =
    Encode.object
        [ ( "typoEditor", DesignSystem.Typography.encodeModel a.typoEditor )
        , ( "colorEditor", DesignSystem.Color.Model.encodeModel a.colorEditor )
        , ( "iconBrowser", DesignSystem.IconBrowser.Model.encodeModel a.iconBrowser )
        , ( "shadows", DesignSystem.Shadow.encodeModel a.shadows )
        , ( "menu", DesignSystem.Menus.encodeMenu a.menu )
        ]



-- [generator-end]
