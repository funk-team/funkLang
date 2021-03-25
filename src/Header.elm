module Header exposing (view)

import Authentication
import Canvas.Msg
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Model.Model
import Msg
import Ui.Style exposing (edges)


view : Model.Model.Model -> Element.Element Msg.Msg
view { authentication } =
    case authentication of
        Authentication.OpenCoreUser ->
            Element.el Ui.Style.headerStyles
                (Element.el profileSectionStyles (Element.text "Funk Open Core"))

        Authentication.Anonymous ->
            Element.none

        Authentication.Undetermined ->
            Element.none

        Authentication.LoggedIn userInfo ->
            Element.el Ui.Style.headerStyles
                (Element.row
                    profileSectionStyles
                    [ Element.text userInfo.email
                    , case String.uncons userInfo.name of
                        Just ( first, _ ) ->
                            Element.el
                                [ Element.width (Element.px 25)
                                , Element.height (Element.px 25)
                                , Element.Background.color Ui.Style.highlightColorSolid
                                , Element.Font.color Ui.Style.black
                                , Element.Border.rounded 25
                                ]
                                (Element.text (String.toUpper (String.fromChar first))
                                    |> Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.moveDown 2
                                        , Element.scale 0.8
                                        ]
                                )

                        Nothing ->
                            Element.none
                    , logoutButton
                    ]
                )


logoutButton =
    Element.Input.button [ Element.Font.underline ]
        { onPress = Just Canvas.Msg.LogoutRequested
        , label = Element.text "Log out"
        }
        |> Element.map Msg.EditorMsg


profileSectionStyles =
    [ Element.alignRight
    , Element.paddingEach { edges | right = 10 }
    , Element.spacing 10
    , Element.centerY
    ]
