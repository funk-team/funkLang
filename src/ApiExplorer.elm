module ApiExplorer exposing (..)

{-| Provide exploration, selection and connection capabilities for content from various sources
-}

import ApiExplorer.Api
import ApiExplorer.Model
import ApiExplorer.Msg
import Dict.Any
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Events.Extra
import Element.Font
import Element.Input
import Help
import Interface.Data
import Json.Encode as Encode
import Persistence
import RemoteData
import Ui.Boxicons
import Ui.Component
import Ui.Input
import Ui.RadioRow
import Ui.Style exposing (edges)


view : Persistence.ProjectMeta -> ApiExplorer.Model.Model -> Element.Element ApiExplorer.Msg.Msg
view projectMeta model =
    Element.row
        [ Element.width Element.fill, Element.height Element.fill ]
        [ viewSidebar model
        , viewCurrentApiSpecEditor projectMeta model
            |> Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingEach { edges | left = 15 }
                ]
        ]



---- SIDE BAR ----


viewSidebar : ApiExplorer.Model.Model -> Element.Element ApiExplorer.Msg.Msg
viewSidebar model =
    let
        buttonRow =
            Element.row
                [ Element.spacing 10
                , Element.padding 10
                , Element.Border.widthEach { edges | bottom = 1 }
                , Element.width Element.fill
                ]
                [ addButton
                , helpButton
                ]

        helpButton =
            case model.selectedApiSpec of
                Nothing ->
                    Element.none

                Just _ ->
                    helpButton_

        helpButton_ =
            case ApiExplorer.Model.listApiSpecs model == [] of
                True ->
                    Element.none

                False ->
                    Ui.Component.helpButton model ApiExplorer.Msg.ShowOrHideHelpPanel

        addButton =
            Element.Input.button
                buttonStyles
                { label = Element.text "New source"
                , onPress = Just ApiExplorer.Msg.AddApiSpec
                }

        buttonStyles =
            [ Element.padding 10
            , Element.Border.width 1
            , Element.Border.color Ui.Style.grey
            , Element.Border.rounded 3
            , Element.mouseOver
                [ Element.Background.color Ui.Style.black
                , Element.Font.color Ui.Style.white
                ]
            ]

        sources =
            model
                |> ApiExplorer.Model.listApiSpecs
                |> List.map (viewApiSpecSelector model.selectedApiSpec)
    in
    Element.column
        [ Element.height Element.fill
        , Element.width Element.shrink
        , Element.Border.widthEach { edges | right = 1 }
        ]
        (buttonRow
            :: sources
        )


viewApiSpecSelector : Maybe ApiExplorer.Model.ApiCallKey -> ( ApiExplorer.Model.ApiCallKey, ApiExplorer.Model.ApiSpec ) -> Element.Element ApiExplorer.Msg.Msg
viewApiSpecSelector selectedApiSpec ( key, { name } ) =
    let
        isSelected =
            Just key == selectedApiSpec

        bg =
            if isSelected then
                Ui.Style.highlightColor

            else
                Ui.Style.transparent

        deleteButton id isSelected_ =
            if isSelected_ then
                Element.el
                    [ Element.Events.Extra.onClickNoBubble (ApiExplorer.Msg.RemoveApiSpec id)
                    , Element.height (Element.px 15)
                    , Element.Font.color Ui.Style.grey
                    , Element.mouseOver [ Element.Font.color Ui.Style.black ]
                    ]
                    (Ui.Component.icon Ui.Boxicons.bxTrash)

            else
                Element.none
    in
    Element.row
        [ Element.Background.color bg
        , Element.Events.onClick <| ApiExplorer.Msg.SelectApiSpec key
        , Element.spacing 10
        , Element.padding 10
        , Element.pointer
        , Element.width (Element.minimum 200 Element.fill)
        ]
        [ Element.el
            []
            (Element.text name)
        , deleteButton key isSelected
        ]



---- BODY ----


viewCurrentApiSpecEditor : Persistence.ProjectMeta -> ApiExplorer.Model.Model -> Element.Element ApiExplorer.Msg.Msg
viewCurrentApiSpecEditor projectMeta model =
    let
        selectOnLeftMsg =
            Element.column [ Element.padding 20, Element.spacing 20 ]
                [ Element.text "Select one of the sources on the left or create new source."
                ]
    in
    case ( model.selectedApiSpec, ApiExplorer.Model.listApiSpecs model == [] || model.helpOpen ) of
        ( _, True ) ->
            Element.column [ Element.paddingEach { top = 20, right = 10, bottom = 0, left = 0 }, Element.spacing 10 ]
                [ Ui.Component.textListWithHeader model.helpOpen
                    "Connect REST APIs to your design"
                    [ "➡ Visually select data to insert into your design"
                    , "➡ Render lists and single data points"
                    , "➡ Assign URL parameters by clicking on parts of the URL"
                    , "➡ Mock data as a real APIs by uploading a JSON file"
                    , "➡ Parse cURL commands directly into funk "
                    ]
                    |> Element.map (always ApiExplorer.Msg.ShowOrHideHelpPanel)
                ]

        ( Nothing, False ) ->
            selectOnLeftMsg

        ( Just selectedApiSpecId, False ) ->
            case ApiExplorer.Model.getApiSpec selectedApiSpecId model of
                Nothing ->
                    selectOnLeftMsg

                Just source ->
                    let
                        nameInput =
                            Ui.Input.string "ApiSpec name" "Give your sauce a name" source.name
                                |> Element.map
                                    (\name -> ApiExplorer.Msg.SetApiSpec selectedApiSpecId { source | name = name })
                                |> Element.el [ Element.width (Element.px 400) ]

                        sourceDetails =
                            ApiExplorer.Api.view projectMeta source
                                |> Element.map (ApiExplorer.Msg.ApiSpecEdit_ selectedApiSpecId)
                    in
                    Element.column
                        [ Element.paddingEach { top = 15, right = 0, bottom = 0, left = 0 }
                        , Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 30
                        ]
                        [ nameInput
                        , sourceTypeSwitcher source
                            |> Element.map (ApiExplorer.Msg.SetApiSpec selectedApiSpecId)
                        , sourceDetails
                        ]


sourceTypeSwitcher : ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Model.ApiSpec
sourceTypeSwitcher source =
    let
        items =
            [ ApiExplorer.Model.Api, ApiExplorer.Model.Mock, ApiExplorer.Model.Curl ]

        toLabel kind =
            case kind of
                ApiExplorer.Model.Api ->
                    "URL"

                ApiExplorer.Model.Mock ->
                    "JSON"

                ApiExplorer.Model.Curl ->
                    "cURL"
    in
    Ui.RadioRow.view
        { items = items
        , toLabel = toLabel
        , selected = (==) source.kind
        }
        |> Element.map (\kind -> { source | kind = kind, request = RemoteData.NotAsked })


update :
    ApiExplorer.Msg.Msg
    -> ApiExplorer.Model.Model
    -> ( ApiExplorer.Model.Model, Cmd ApiExplorer.Msg.Msg )
update msg model =
    case msg of
        ApiExplorer.Msg.ShowOrHideHelpPanel ->
            ( { model | helpOpen = not model.helpOpen }, Cmd.none )

        ApiExplorer.Msg.SelectApiSpec key ->
            ( { model | selectedApiSpec = Just key }, Cmd.none )

        ApiExplorer.Msg.SwitchMenu menu ->
            ( { model | menu = menu }, Cmd.none )

        ApiExplorer.Msg.RemoveApiSpec key ->
            ( ApiExplorer.Model.removeApiSpec key model, Cmd.none )

        ApiExplorer.Msg.SetApiSpec key source ->
            ( ApiExplorer.Model.setApiSpec key source model, Cmd.none )

        ApiExplorer.Msg.SetApiSpecName key name ->
            ( ApiExplorer.Model.mapApiSpec key (\spec -> { spec | name = name }) model, Cmd.none )

        ApiExplorer.Msg.AddApiSpec ->
            ( ApiExplorer.Model.addApiSpec model, Cmd.none )

        ApiExplorer.Msg.ApiSpecEdit_ key msg_ ->
            case ApiExplorer.Model.getApiSpec key model of
                Just oldApiSpec ->
                    let
                        ( source, cmd ) =
                            ApiExplorer.Api.update msg_ oldApiSpec
                    in
                    ( ApiExplorer.Model.setApiSpec key source model
                    , Cmd.map (ApiExplorer.Msg.ApiSpecEdit_ key) cmd
                    )

                Nothing ->
                    ( model, Cmd.none )


sanitizeUrl : String -> String
sanitizeUrl url =
    if String.startsWith "https://" url then
        url

    else if String.startsWith "http://" url then
        url

    else if String.startsWith "://" url then
        "https" ++ url

    else
        "https://" ++ url


{-| Get the preselected data of an API as JSON object
-}
getAsJson : ApiExplorer.Model.ApiSpec -> ApiExplorer.Model.Model -> Maybe Encode.Value
getAsJson spec model =
    case spec.request of
        RemoteData.Success value ->
            List.filterMap
                (\( keyPath, { name } ) ->
                    case Interface.Data.getFromValue keyPath value of
                        Nothing ->
                            Nothing

                        Just oneVal ->
                            Just ( name |> Help.toJsVariableName, oneVal )
                )
                (spec.responseDataSelection
                    |> Dict.Any.toList
                )
                |> Encode.object
                |> Just

        _ ->
            Nothing
