port module DeployEditor.Deploy exposing (..)

import Element
import Element.Font
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Product
import Persistence
import Route
import Slug
import Ui.Component
import Ui.Input
import Url


type Msg
    = Update Model
    | Deploy
    | AssetsCollected
    | SetSiteName String
    | DeployCompleted ( String, String )
    | DeployError String


port deploy : ( Persistence.ProjectMeta, String ) -> Cmd msg


port assetsCollected : (() -> msg) -> Sub msg


port deployCompleted : (( String, String ) -> msg) -> Sub msg


port deployError : (String -> msg) -> Sub msg


subscriptions =
    Sub.batch [ deployCompleted DeployCompleted, assetsCollected (always AssetsCollected), deployError DeployError ]


stringArrayDecoder : Decode.Decoder (List String)
stringArrayDecoder =
    Decode.list Decode.string


deployIfValid : Model.Product.Mode -> Url.Url -> Model -> Cmd Msg
deployIfValid mode url model =
    let
        siteSlug =
            model.siteName
                |> Slug.generate
                |> Maybe.map Slug.toString
    in
    case Maybe.map2 Tuple.pair (Route.getProjectData mode url) siteSlug of
        Just info ->
            deploy info

        Nothing ->
            Cmd.none


update : Msg -> Model.Product.Mode -> Url.Url -> Model -> ( Model, Cmd Msg )
update msg mode url model =
    case msg of
        Update m ->
            ( m, Cmd.none )

        Deploy ->
            ( { model | status = CollectingAssets }, deployIfValid mode url model )

        AssetsCollected ->
            ( { model | status = Deploying }, Cmd.none )

        DeployCompleted ( slug, hash ) ->
            ( { model | status = Deployed slug }, Cmd.none )

        DeployError message ->
            ( { model | status = Error message }, Cmd.none )

        SetSiteName newName ->
            ( { model | siteName = newName, status = NotAsked }, Cmd.none )


{-| This makes sure the status does not get stuck on bad states after reloading.
-}
type alias StatusNormalized =
    Status


encodeStatusNormalized =
    encodeStatus


decodeStatusNormalized =
    decodeStatus
        |> Decode.map
            (\status ->
                case status of
                    Deployed str ->
                        status

                    _ ->
                        NotAsked
            )



-- [generator-start]


type alias Model =
    { status : StatusNormalized
    , siteName : String
    }


type Status
    = NotAsked
    | CollectingAssets
    | Deploying
    | Deployed String
    | Error String



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.map2
        Model
        (Decode.field "status" decodeStatusNormalized)
        (Decode.field "siteName" Decode.string)


decodeStatus =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeStatusHelp


decodeStatusHelp constructor =
    case constructor of
        "NotAsked" ->
            Decode.succeed NotAsked

        "CollectingAssets" ->
            Decode.succeed CollectingAssets

        "Deploying" ->
            Decode.succeed Deploying

        "Deployed" ->
            Decode.map
                Deployed
                (Decode.field "A1" Decode.string)

        "Error" ->
            Decode.map
                Error
                (Decode.field "A1" Decode.string)

        other ->
            Decode.fail <| "Unknown constructor for type Status: " ++ other


encodeModel a =
    Encode.object
        [ ( "status", encodeStatusNormalized a.status )
        , ( "siteName", Encode.string a.siteName )
        ]


encodeStatus a =
    case a of
        NotAsked ->
            Encode.object
                [ ( "Constructor", Encode.string "NotAsked" )
                ]

        CollectingAssets ->
            Encode.object
                [ ( "Constructor", Encode.string "CollectingAssets" )
                ]

        Deploying ->
            Encode.object
                [ ( "Constructor", Encode.string "Deploying" )
                ]

        Deployed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Deployed" )
                , ( "A1", Encode.string a1 )
                ]

        Error a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Error" )
                , ( "A1", Encode.string a1 )
                ]



-- [generator-end]


init : Model
init =
    { status = NotAsked
    , siteName = ""
    }


validate =
    Slug.generate
        >> Maybe.map Slug.toString
        >> Maybe.andThen
            (\slug ->
                case String.length slug > 5 && String.length slug <= 50 of
                    True ->
                        Just slug

                    False ->
                        Nothing
            )


view : Model.Product.Mode -> Model -> Element.Element Msg
view mode model =
    case mode of
        Model.Product.Core ->
            Element.paragraph [ Element.width (Element.fill |> Element.maximum 500) ]
                [ Element.text "Deployment is not available in funk open core. We will soon provide the option to download a deployable bundle" ]

        Model.Product.Enterprise ->
            let
                deployButton btnText =
                    Ui.Component.buttonOnClick Deploy btnText False

                validation =
                    case validate model.siteName of
                        Just validSlug ->
                            Element.column [ Element.spacing 10 ]
                                [ Element.row [ Element.alpha 0.5 ]
                                    [ case model.status of
                                        Deployed _ ->
                                            Element.none

                                        _ ->
                                            Element.text ("https://" ++ validSlug ++ ".builtwithfunk.com")
                                    ]
                                , buttonOrStatus
                                ]

                        Nothing ->
                            Element.column [ Element.width (Element.px 500), Element.alpha 0.5, Element.spacingXY 0 10 ]
                                [ Element.text "Enter a name for your site."
                                , Element.text "It will be deployed as a subdomain at builtwithfunk.com."
                                , if String.length model.siteName < 5 && model.siteName /= "" then
                                    let
                                        toAdd =
                                            String.fromInt (5 - String.length model.siteName)
                                    in
                                    Element.el [ Element.Font.bold ] (Element.text ("Too short, add " ++ toAdd ++ " more characters"))

                                  else
                                    Element.none
                                ]

                nameInput =
                    Ui.Input.string "Site name" "..." model.siteName
                        |> Element.map SetSiteName
                        |> Element.el [ Element.width (Element.px 400) ]

                buttonOrStatus =
                    case model.status of
                        CollectingAssets ->
                            Element.text "Preparing files to upload"

                        Deploying ->
                            Element.text "Deploying.... this will take about 60 seconds"

                        NotAsked ->
                            Element.column [ Element.spacing 10 ] [ deployButton "Deploy app" ]

                        Deployed slug ->
                            let
                                url =
                                    "https://" ++ slug ++ ".builtwithfunk.com"
                            in
                            Element.column [ Element.spacing 10 ]
                                [ Element.text "Deployed."
                                , Element.newTabLink [] { label = Element.text url, url = url }
                                , deployButton "Re-Deploy"
                                ]

                        Error msg ->
                            case msg of
                                "slug in use" ->
                                    Element.column [ Element.spacing 10 ] [ Element.text <| "Domain in use, please choose another. " ]

                                _ ->
                                    Element.column [ Element.spacing 10 ] [ Element.text <| "Houston, we have a problem. Please try again or contact support.", deployButton "Deploy" ]
            in
            Element.column [ Element.spacing 20 ]
                [ nameInput
                , validation
                ]
