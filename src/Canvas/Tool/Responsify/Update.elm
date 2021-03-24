module Canvas.Tool.Responsify.Update exposing (..)

import Canvas.Tool.Responsify.Model


type Msg
    = ChangeChildElement Spec.Element.Id.Id
    | NoOp


update : Msg -> Canvas.Tool.Responsify.Model.Model -> ( Canvas.Tool.Responsify.Model.Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeChildElement eId ->
            let
                elementOpen =
                    case model.childElementOpen of
                        Nothing ->
                            Just eId

                        Just modelId ->
                            if modelId == eId then
                                Nothing

                            else
                                Just eId
            in
            ( { model | childElementOpen = elementOpen }
            , Cmd.none
            )
