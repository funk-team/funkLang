port module InvestorDemo exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import String
import Ui.Boxicons
import Ui.Component
import Ui.Style


port betaSignup : { password : String, name : String, relogin : Bool } -> Cmd msg


trackUserLogin inInit { password, name, authenticated } =
    if not (googVCs password) then
        betaSignup { password = password, name = name ++ "KILLED", relogin = inInit }

    else if authenticated && (password /= "3456") then
        betaSignup { password = password, name = name, relogin = inInit }

    else
        Cmd.none


type Msg
    = CloseButtonClicked
    | PasswordEntered String
    | NameEntered String
    | NextImageClicked
    | ClickNameButton
    | PreviousImageClicked


googVCs password =
    let
        goodVC =
            if String.contains "frontline-3456" password then
                False

            else
                True
    in
    goodVC


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseButtonClicked ->
            ( { model | open = not model.open, walkThroughScreen = 0 }, Cmd.none )

        ClickNameButton ->
            ( { model | nameEntered = True }, trackUserLogin False model )

        PasswordEntered password ->
            let
                auth =
                    if String.contains "3456" password && googVCs password then
                        True

                    else
                        False
            in
            ( { model | authenticated = auth, password = password }, Cmd.none )

        NameEntered name ->
            ( { model | name = name }, Cmd.none )

        NextImageClicked ->
            let
                screenNumber =
                    if model.walkThroughScreen == 11 then
                        model.walkThroughScreen

                    else
                        model.walkThroughScreen + 1
            in
            ( { model | walkThroughScreen = screenNumber }, Cmd.none )

        PreviousImageClicked ->
            let
                screenNumber =
                    if model.walkThroughScreen == 0 then
                        model.walkThroughScreen

                    else
                        model.walkThroughScreen - 1
            in
            ( { model | walkThroughScreen = screenNumber }, Cmd.none )


viewElmUi =
    view >> Element.layout []


view : Model -> Element.Element Msg
view model =
    let
        content =
            case model.authenticated of
                True ->
                    case model.nameEntered of
                        True ->
                            Element.column (wrapStyles ++ viewOpenStyles model.open)
                                [ Element.row [ Element.spacing 10 ]
                                    [ closeButton
                                    , Element.el [] (Element.text "Funk Alpha")
                                    ]
                                , passwordCorrectView
                                ]

                        False ->
                            Element.column (wrapStyles ++ viewOpenStyles model.open)
                                [ Element.row [ Element.spacing 10 ]
                                    [ Element.el [] (Element.text "Funk Alpha")
                                    ]
                                , nameEnter model
                                ]

                False ->
                    Element.column (wrapStyles ++ viewOpenStyles True)
                        [ Element.row [ Element.spacing 10 ]
                            [ Element.el [] (Element.text "Funk Alpha") ]
                        , passwordEnter model
                        ]
    in
    Element.column (wrapStyles ++ viewOpenStyles model.open)
        [ content ]


passwordCorrectView =
    let
        styles =
            [ Element.Font.color Ui.Style.white
            , Ui.Style.interFont
            , Element.width (Element.fill |> Element.maximum 1024)
            , Element.centerX
            ]

        logStyles =
            [ Element.spacingXY 0 10, Element.paddingXY 30 0 ]

        log =
            Element.column [ Element.spacingXY 0 20 ]
                [ Element.el [ Element.Font.bold ] (Element.text "Friday July 10th:")
                , Element.column logStyles
                    [ Element.text "* Image uploads"
                    , Element.text "* Add text to elements"
                    , Element.text "* Initial global styles via custom 'Design System' editor"
                    , Element.text "* Fonts via Google Fonts"
                    , Element.text "* Styles render in editor and preview"
                    , Element.text "* Bug fixes in layout editor"
                    ]
                , Element.el [ Element.Font.bold ] (Element.text "Thursday July 2nd:")
                , Element.column logStyles
                    [ Element.text "* Add live preview for screens"
                    , Element.text "* Add navigation when clicking elements"
                    , Element.text "* Improve UI"
                    ]
                , Element.el [ Element.Font.bold ] (Element.text "Friday 19th June:")
                , Element.column logStyles
                    [ Element.text "* Cluster similar rows before applying responsify"
                    , Element.text "* Ask user to confirm clusters before applying"
                    ]
                , Element.el [ Element.Font.bold ] (Element.text "Thursday 11th June:")
                , Element.column logStyles
                    [ Element.text "* Name newly drawn screens"
                    , Element.text "* Delete button"
                    , Element.text "* Imporve responsify panel layout"
                    , Element.text "* Add API editor"
                    , Element.text "* Enable editable overlay and API editor for inFlow elements"
                    , Element.text "* Guess types for API calls"
                    , Element.text "* Improve API editor layout"
                    , Element.text "* Add editable overlay"
                    , Element.text "* Enable adjustable layout after responsify"
                    , Element.text "* Enable manual style adjustment"
                    , Element.text "* Sane defaults for images from API calls"
                    , Element.text "* Fix lock decoding"
                    ]
                , Element.el [ Element.Font.bold ] (Element.text "Thursday 21nd May:")
                , Element.column logStyles
                    [ Element.text "* Responsify algo on individual elements (WIP)"
                    ]
                , Element.el [ Element.Font.bold ] (Element.text "Friday 15th May:")
                , Element.column logStyles
                    [ Element.text "* Add infinity canvas"
                    , Element.text "* Create dynamic screen layouts on first element drawn"
                    , Element.text "* Continuous drawing algo"
                    , Element.text "* Redesign sidebar and layout (WIP)"
                    , Element.text "* Add dynamic line, center and length snapping (WIP)"
                    , Element.text "* Temporarily remove non-core features (API Editor, Code Editor)"
                    ]
                ]
    in
    Element.column styles
        [ Element.column [ Element.paddingXY 0 30, Element.width (Element.fill |> Element.minimum 300), Element.spacingXY 0 5 ]
            [ Element.paragraph [ Element.paddingXY 0 20 ] [ Element.text "Funk enables freeform design, just like Figma or Sketch, but the design you create is a real responsive layout which can be directly connected to data and deployed." ]
            , Element.paragraph [ Element.paddingXY 0 20 ] [ Element.text "Funk is the worlds first Visual FrontEnd framework. We will enable millions of designers, low-tech users and developers to move beyond templates and create stunning websites." ]
            , log
            ]
        ]


walkThrough model =
    let
        styles =
            [ Element.Font.color Ui.Style.white
            , Ui.Style.interFont
            , Element.width (Element.fill |> Element.maximum 1024)
            , Element.centerX
            ]

        imageStyles =
            [ Element.width (Element.px 650)
            , Element.Border.glow Ui.Style.grey 3
            ]
    in
    Element.wrappedRow styles
        [ Element.paragraph [ Element.paddingXY 0 30, Element.width (Element.fill |> Element.minimum 300) ] (walkThroughText model.walkThroughScreen)
        , Element.row []
            [ Element.el [ Element.paddingXY 20 0 ] <| Ui.Component.buttonInvert PreviousImageClicked Ui.Boxicons.bxSkipPrevious "Back" (buttonStatus model.walkThroughScreen 0)
            , Element.image imageStyles
                { src = imgSrc model.walkThroughScreen, description = "" }
            , Element.el [ Element.paddingXY 20 0 ] <| Ui.Component.buttonInvert NextImageClicked Ui.Boxicons.bxSkipNext "Next" (buttonStatus model.walkThroughScreen 11)
            ]
        , preloadImages
        ]


preloadImages =
    List.range 0 10
        |> List.map preloadOneImage
        |> Element.column []


preloadOneImage src =
    Element.image preloadImageStyles
        { src = imgSrc src, description = "" }


preloadImageStyles =
    Ui.Style.styles
        [ ( "opacity", "0" ), ( "position", "absolute" ), ( "pointer-events", "none" ) ]


imgSrc index =
    let
        walkThroughScreenFileType =
            getWalkThroughScreenFileType index

        src =
            "walkThrough/" ++ String.fromInt index ++ walkThroughScreenFileType
    in
    src


getWalkThroughScreenFileType walkThroughScreen =
    let
        gifScreens =
            [ 2, 3, 4, 5, 6, 8, 9, 10, 11 ]
    in
    if List.member walkThroughScreen gifScreens then
        ".gif"

    else
        ".png"


buttonStatus screen disableOnScreen =
    if screen == disableOnScreen then
        True

    else
        False


walkThroughText walkThroughScreen =
    if walkThroughScreen == 0 then
        [ Element.text "Thanks for trying out our Alpha! This alpha features a visual designer and API connector. "
        , Element.el [ Element.Font.bold ] (Element.text "Let's try recreating the Frozen movie page.")
        ]

    else if walkThroughScreen == 1 then
        [ Element.text "A webpage is esentially a series of boxes (<divs>) which are arragned and styled using CSS. "
        , Element.text "Most sitebuilders like Wix and WebFlow use "
        , Element.el [ Element.Font.bold ] (Element.text "templates")
        , Element.text ". Funk allows the freeform drawing of layouts, like in a "
        , Element.el [ Element.Font.bold ] (Element.text "design tools")
        , Element.text ". This allows 1-1 coupling between design and code."
        ]

    else if walkThroughScreen == 2 then
        [ Element.text "Click the "
        , Element.el [ Element.Font.bold ] (Element.text "'Draw' ")
        , Element.text "button to start drawing a freeform wireframe of the Frozen movie page"
        ]

    else if walkThroughScreen == 3 then
        [ Element.text "Clicking "
        , Element.el [ Element.Font.bold ] (Element.text "'Responsify' ")
        , Element.text "uses a "
        , Element.el [ Element.Font.bold ] (Element.text "-complex and work in progress- ")
        , Element.text "algorithm to create a responsive design with "
        , Element.el [ Element.Font.bold ] (Element.text "no templates")
        , Element.text ". Eventually this will happen automatically as new boxes are drawn."
        ]

    else if walkThroughScreen == 4 then
        [ Element.text "Boxes must be drawn within other boxes, they can't overlap. The "
        , Element.el [ Element.Font.bold ] (Element.text " algorithm knows if you draw a design which won't be valid ")
        , Element.text "and will offer help on how to fix the problem."
        ]

    else if walkThroughScreen == 5 then
        [ Element.text "After you've drawn your wireframe you can select data from an external API using the  "
        , Element.el [ Element.Font.bold ] (Element.text "API Explorer")
        , Element.text ". Select the data you want (poster, title, overview) "
        , Element.el [ Element.Font.bold ] (Element.text "assign them a name AND type")
        , Element.text ". Next we can connect this external data directly to the design. "
        ]

    else if walkThroughScreen == 6 then
        [ Element.text "Click on the box you want to "
        , Element.el [ Element.Font.bold ] (Element.text "add data to")
        , Element.text " it should then appear in your design. We have basic styling tools and will add more options soon."
        ]

    else if walkThroughScreen == 7 then
        [ Element.text "Once you've finished it should look something like this."
        ]

    else if walkThroughScreen == 8 then
        [ Element.el [ Element.Font.bold ] (Element.text "TIP: ")
        , Element.text "The algorithm automatically creates a responsive design, we are improving this all the time. It presently uses a 'Best Guess' but we have plans to use ML to make this more accurate."
        ]

    else if walkThroughScreen == 9 then
        [ Element.el [ Element.Font.bold ] (Element.text "TIP: ")
        , Element.text "You can add custom HTML to any box and style it within Funk. Eventually you will be able to create global stlye sheets and apply them when needed and write JavaScript directly in Funk."
        ]

    else if walkThroughScreen == 10 then
        [ Element.el [ Element.Font.bold ] (Element.text "TIP: ")
        , Element.text "If you draw a layout which is not what HTML or CSS notmally 'looks like' Funk attempts to correct the design. We call this 'Precision Mode', it makes drawing layouts super easy and fun."
        ]

    else if walkThroughScreen == 11 then
        [ Element.text "We hope you enjoy using our Alpha, please get back to us with any feedback (david@funklang.com). PS click the close button to actually use the alpha!"
        ]

    else
        [ Element.text "" ]


passwordEnter model =
    let
        styles =
            [ Element.Font.color Ui.Style.black, Ui.Style.interFont ]
    in
    Element.column [ Element.spacing 10, Element.width (Element.fill |> Element.maximum 600) ]
        [ Element.Input.text styles (passwordInputAttribs model.password)
        , Element.el [ Element.paddingXY 0 30 ] (Element.text "If you do not have an access key yet, request it from david@funklang.com")
        ]


nameEnter model =
    let
        styles =
            [ Element.Font.color Ui.Style.black, Ui.Style.interFont, onEnter ClickNameButton ]
    in
    Element.column [ Element.spacing 10, Element.width (Element.fill |> Element.maximum 600) ]
        [ Element.Input.text styles (nameInputAttribs model.name)
        , Ui.Component.buttonInvert ClickNameButton
            Ui.Boxicons.bxCheck
            "Submit"
            (if String.length model.name > 2 then
                False

             else
                True
            )
        , Element.el [ Element.paddingXY 0 30 ] (Element.text "Then we can get started.. ")
        ]


passwordInputAttribs :
    String
    ->
        { onChange : String -> Msg
        , text : String
        , placeholder : Maybe (Element.Input.Placeholder Msg)
        , label : Element.Input.Label Msg
        }
passwordInputAttribs password =
    let
        placeholder =
            Element.Input.placeholder
                []
                (Element.text ".....")
    in
    { onChange = PasswordEntered
    , text = password
    , placeholder = Just placeholder
    , label = Element.Input.labelAbove [ Ui.Style.interFont ] (Element.text "Please enter your authentication key")
    }


nameInputAttribs :
    String
    ->
        { onChange : String -> Msg
        , text : String
        , placeholder : Maybe (Element.Input.Placeholder Msg)
        , label : Element.Input.Label Msg
        }
nameInputAttribs name =
    let
        placeholder =
            Element.Input.placeholder
                []
                (Element.text ".....")
    in
    { onChange = NameEntered
    , text = name
    , placeholder = Just placeholder
    , label = Element.Input.labelAbove [ Ui.Style.interFont ] (Element.text "Great, could I also have your name please")
    }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


wrapStyles =
    [ Element.Background.color Ui.Style.highlightColorSolid
    , Element.Font.color Ui.Style.white
    , Element.padding 50
    , Element.spacing 50
    , Element.height Element.fill
    , Element.width Element.fill
    , Element.scrollbarY
    ]


viewOpenStyles isOpen =
    Ui.Style.styles <|
        if isOpen then
            openStyles

        else
            closedStyles


type alias Style =
    ( String, String )


closedStyles : List Style
closedStyles =
    [ ( "transform", "rotateX(0deg) rotateY(0deg) rotateZ(0.5deg) scale(0.99) skewX(-0.5deg) skewY(-0.5deg)" )
    , ( "opacity", "0" )
    , ( "pointer-events", "none" )
    , ( "transition", "all 0.25s " ++ Ui.Style.swiftOut )
    ]


openStyles : List Style
openStyles =
    [ ( "opacity", "1" )
    , ( "transform", "none" )
    , ( "transition", "all 0.2s " ++ Ui.Style.swiftOut )
    ]


closeButton =
    Ui.Component.buttonInvert CloseButtonClicked Ui.Boxicons.bxX "Close" False


init : Model
init =
    Model
        False
        True
        ""
        0
        ""
        False



-- [decgen-start]


type alias Model =
    { authenticated : Bool
    , open : Bool
    , password : String
    , walkThroughScreen : Int
    , name : String
    , nameEntered : Bool
    }



-- [decgen-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.map6
        Model
        (Decode.field "authenticated" Decode.bool)
        (Decode.field "open" Decode.bool)
        (Decode.field "password" Decode.string)
        (Decode.field "walkThroughScreen" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "nameEntered" Decode.bool)


encodeModel a =
    Encode.object
        [ ( "authenticated", Encode.bool a.authenticated )
        , ( "open", Encode.bool a.open )
        , ( "password", Encode.string a.password )
        , ( "walkThroughScreen", Encode.int a.walkThroughScreen )
        , ( "name", Encode.string a.name )
        , ( "nameEntered", Encode.bool a.nameEntered )
        ]



-- [decgen-end]
