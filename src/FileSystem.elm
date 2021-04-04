port module FileSystem exposing (..)

{-| This module connects the funk editor with the local file system

Right now it only checks if files were changed and shows them to the user.

In the future this module will also send out modifications of the file system.

-}

import Element
import Element.Background
import Element.Border
import Element.Font
import FileSystem.CliConnection
import FileSystem.Model
import Json.Decode as Decode
import Model
import Model.Model
import Persistence
import Ui.BigTabs
import Ui.Component
import Ui.Style exposing (edges)


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
            [ { label = "PROJECT"
              , onClick = NoOp
              , sideNote = Just "coming soon"
              , isSelected = False
              }
            , { label = "LOCAL SYNC"
              , onClick = NoOp
              , sideNote = Just "coming soon"
              , isSelected = False
              }
            ]
        )


update : Msg -> Model.Model.Model -> ( Model.Model.Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CliMsg cliMsg ->
            let
                ( cliConnection, cmd ) =
                    FileSystem.CliConnection.update cliMsg model.cliConnection
            in
            ( { model | cliConnection = cliConnection }, Cmd.map CliMsg cmd )

        Changed files ->
            ( { model | fs = files }, Cmd.none )

        SpecWritten encodedSpec ->
            case Persistence.loadModel Model.decodeSavedState encodedSpec of
                Err _ ->
                    ( model, Cmd.none )

                Ok spec ->
                    ( Model.injectUpdatedSpec spec model, Cmd.none )


port fs_changed : (FileSystem.Model.Files -> msg) -> Sub msg


port fs_specWritten : (String -> msg) -> Sub msg


port fs_componentWritten : (Decode.Value -> msg) -> Sub msg


type Msg
    = Changed FileSystem.Model.Files
    | SpecWritten String
    | CliMsg FileSystem.CliConnection.Msg
    | NoOp


view : Model.Model.Model -> Element.Element Msg
view model =
    Element.column
        [ Element.width Element.fill ]
        [ tabs
        , Element.column [ Element.padding 20, Element.spacing 20, Element.width Element.fill ]
            [ Ui.Component.textListWithHeader False
                "Mirror everything you do in the funk editor to your IDE using the funk CLI"
                [ "➡ Changes you make in the visual editor will be mirrored to your local enviroment in code"
                , "➡ Insert custom elements directly into funk, create hooks to style them visually"
                , "➡ STATUS: Private Beta contact us if you want to use this feature (david@funklang.com)"
                ]
                |> Element.map (always NoOp)
            ]

        --        , FileSystem.CliConnection.view model.cliConnection
        --            |> Element.map CliMsg
        --        , files
        ]


{-| Render a little badge for every recognized custom elements.
Later we could add more badges for other types of files.
-}
viewComponent : Decode.Value -> Element.Element msg
viewComponent val =
    case Decode.decodeValue componentDecoder val of
        Ok { description, name } ->
            Element.row
                [ Element.spacing 5
                , Element.padding 5
                , Element.Background.color Ui.Style.highlightColorSolid
                , Element.Border.rounded 3
                , Element.Font.color Ui.Style.white
                ]
                [ Element.el [ Element.Font.heavy ] (Element.text name)

                --, Element.text description
                ]

        Err _ ->
            Element.text "Please make sure the custom element has the correct exports"


type alias Component =
    { name : String
    , description : String
    }


componentDecoder =
    Decode.map2 Component
        (Decode.at [ "default", "name" ] Decode.string)
        (Decode.at [ "default", "description" ] Decode.string)


subs : Sub Msg
subs =
    Sub.batch
        [ fs_changed Changed
        , fs_specWritten SpecWritten
        , FileSystem.CliConnection.subs |> Sub.map CliMsg
        ]
