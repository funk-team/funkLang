module ModelEditor.Help exposing (..)

import Canvas.AttributesPanel.Content.Tabs
import Dict
import Dynamic.Data
import IntDict
import ModelEditor.Model


addField model =
    let
        fieldName =
            "Field " ++ String.fromInt (Dict.size model.fields + 1)

        newField : ModelEditor.Model.Field
        newField =
            { associations = Dict.empty
            , kind = ModelEditor.Model.Unspecified
            , connectionWorkflowTarget = Nothing
            , activeContentTab = Canvas.AttributesPanel.Content.Tabs.Text
            , comment = ""
            , name = fieldName
            }

        nextId =
            IntDict.nextId model.fields

        newFields =
            Dict.insert
                nextId
                newField
                model.fields
    in
    { model = { model | fields = newFields }, fieldId = nextId }


addTextField : ModelEditor.Model.Model -> { model : ModelEditor.Model.Model, fieldId : Int }
addTextField model =
    let
        addResult =
            addField model

        model_ =
            addResult.model

        fields =
            Dict.update
                addResult.fieldId
                (Maybe.map (\field -> { field | kind = ModelEditor.Model.WithDefault (Dynamic.Data.StringInstance "") }))
                addResult.model.fields
    in
    { model = { model_ | fields = fields }, fieldId = addResult.fieldId }
