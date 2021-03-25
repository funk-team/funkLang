module Runtime exposing (handleInternalUrl, main)

import Browser
import Browser.Navigation
import Html
import Http
import Model
import Model.Model
import Model.Product
import Preview
import Preview.Msg
import RemoteData
import RemoteData.Http
import Route
import Runtime.Msg
import ScrollTo
import Slug
import Spec
import Spec.Element.Id
import Spec.Model
import Url
import Url.Parser


type Route
    = ScreenRoute Slug.Slug
    | HomeRoute


parse : Url.Url -> Route
parse =
    Url.Parser.parse routeParser
        >> Maybe.withDefault HomeRoute


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Runtime.Msg.Msg )
init flags url key =
    let
        model : Model
        model =
            { url = url, userModel = RemoteData.Loading, key = key, scrollTo = ScrollTo.init }

        cmd : Cmd Runtime.Msg.Msg
        cmd =
            RemoteData.Http.get "/spec.json"
                Runtime.Msg.SpecRetrieved
                Spec.Model.decodeSpec
    in
    ( model, cmd )


type alias UserModel =
    RemoteData.WebData Model.Model.UserModel


type alias Model =
    { url : Url.Url
    , userModel : UserModel
    , key : Browser.Navigation.Key
    , scrollTo : ScrollTo.State
    }


subscriptions : Model -> Sub Runtime.Msg.Msg
subscriptions =
    always Sub.none


main : Program () Model Runtime.Msg.Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , onUrlRequest = Runtime.Msg.LinkClicked
        , onUrlChange = Runtime.Msg.UrlChanged
        , subscriptions = subscriptions
        }


baseUrl =
    ""


view : Model -> Browser.Document Runtime.Msg.Msg
view model =
    case parse model.url of
        ScreenRoute id ->
            Preview.view (\slug -> "/" ++ Slug.toString slug) id model.userModel
                |> (\doc -> { title = doc.title, body = List.map (Html.map Runtime.Msg.PreviewMsg) doc.body })

        HomeRoute ->
            case model.userModel |> RemoteData.map Spec.getScreensWithUniqueSlugs of
                RemoteData.Success [] ->
                    { body = [ Html.text "Project has no screens" ], title = "Empty Project" }

                RemoteData.Success (( slug, _ ) :: _) ->
                    Preview.view (\slugToNavigateTo -> "/" ++ Slug.toString slugToNavigateTo) slug model.userModel
                        |> (\doc -> { title = doc.title, body = List.map (Html.map Runtime.Msg.PreviewMsg) doc.body })

                RemoteData.Loading ->
                    { title = "Loading", body = [ Html.text "Loading" ] }

                RemoteData.NotAsked ->
                    { title = "Loading", body = [ Html.text "Loading" ] }

                RemoteData.Failure f ->
                    case f of
                        Http.BadBody err ->
                            { title = "Error"
                            , body =
                                [ Html.text "Could not upgrade spec"
                                , Html.pre [] [ Html.text err ]
                                ]
                            }

                        _ ->
                            { title = "Problem"
                            , body =
                                [ Html.text "Something went wrong loading the spec" ]
                            }


update : Runtime.Msg.Msg -> Model -> ( Model, Cmd Runtime.Msg.Msg )
update msg model =
    case msg of
        Runtime.Msg.SpecRetrieved specResponse ->
            ( { model | userModel = RemoteData.map (Model.initUserModel >> Model.initRuntimeModel) specResponse }, Cmd.none )

        Runtime.Msg.UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        Runtime.Msg.PreviewMsg previewMsg ->
            let
                ( userModel, cmd, scrollTo ) =
                    case
                        RemoteData.map
                            -- the core vs enterprise is not relevant in runtime
                            -- because it is only used for hot code injection and will not influence the routing on production
                            (Preview.update previewMsg Model.Product.Core model.url model.scrollTo)
                            model.userModel
                    of
                        RemoteData.Success return ->
                            ( RemoteData.Success return.userModel, return.cmd, return.scrollTo )

                        _ ->
                            ( model.userModel, Cmd.none, model.scrollTo )
            in
            ( { model
                | userModel = userModel
                , scrollTo = scrollTo
              }
            , cmd |> Cmd.map Runtime.Msg.PreviewMsg
            )

        Runtime.Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , handleInternalUrl
                        { key = model.key
                        , url = url
                        , scrollTo = model.scrollTo
                        , msg = Preview.Msg.ScrollToMsg >> Runtime.Msg.PreviewMsg
                        }
                    )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )


type alias InternalUrlParams msg =
    { msg : ScrollTo.Msg -> msg
    , key : Browser.Navigation.Key
    , scrollTo : ScrollTo.State
    , url : Url.Url
    }


handleInternalUrl : InternalUrlParams msg -> Cmd msg
handleInternalUrl { key, url, scrollTo, msg } =
    let
        offset =
            case Maybe.map (String.split "offset=") url.query of
                Just [ _, offsetAsString ] ->
                    String.toInt offsetAsString
                        |> Maybe.withDefault 0

                _ ->
                    0

        parsedId =
            url.fragment
                |> Maybe.andThen String.toInt
                |> Maybe.map Spec.Element.Id.idFromInt
                |> Maybe.map Spec.Element.Id.toHtmlIdRaw

        scrollToCmd =
            case parsedId of
                Just id ->
                    scrollToWithOffsetInPercent offset id
                        |> Cmd.map msg

                Nothing ->
                    Cmd.none

        updateUrlCmd =
            Browser.Navigation.pushUrl
                key
                (Url.toString url)
    in
    Cmd.batch [ updateUrlCmd, scrollToCmd ]


scrollToWithOffsetInPercent : Int -> String -> Cmd ScrollTo.Msg
scrollToWithOffsetInPercent offset id =
    let
        f { viewport, scene } { element } =
            let
                offsetInPx =
                    (toFloat offset * 0.01) * viewport.height
            in
            { from =
                { x = viewport.x
                , y = viewport.y
                }
            , to =
                { x = viewport.x
                , y = min (element.y - offsetInPx) (scene.height - viewport.height)
                }
            }
    in
    ScrollTo.scrollToCustom f id


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map ScreenRoute Route.slugParser
        ]
