port module Projects.Api exposing (createProject, deleteProject, list, patchProject, subs)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Projects.Model
import Projects.Msg
import RemoteData
import RemoteData.Http


authHeaderKey : String
authHeaderKey =
    "X-Funk-User-Id"


endpoint : String
endpoint =
    "/api_server/project"


defaultConfig =
    RemoteData.Http.defaultConfig


createProject : String -> Cmd Projects.Msg.Msg
createProject userId =
    RemoteData.Http.postWithConfig
        { defaultConfig | headers = [ Http.header authHeaderKey userId ] }
        (endpoint ++ "/new")
        Projects.Msg.ProjectCreated
        Projects.Model.decodeProject
        Encode.null


deleteProject : String -> String -> Cmd Projects.Msg.Msg
deleteProject id userId =
    RemoteData.Http.deleteWithConfig
        { defaultConfig | headers = [ Http.header authHeaderKey userId ] }
        (endpoint ++ "/" ++ id)
        (Projects.Msg.ProjectDeleted id)
        Encode.null


patchProject : String -> Projects.Model.Project -> Cmd Projects.Msg.Msg
patchProject userId project =
    RemoteData.Http.patchWithConfig
        { defaultConfig | headers = [ Http.header authHeaderKey userId ] }
        (endpoint ++ "/" ++ project.id)
        (Projects.Msg.ProjectUpdated project.id)
        Projects.Model.decodeProject
        (Projects.Model.encodeProject project)


list : String -> Cmd Projects.Msg.Msg
list userId =
    RemoteData.Http.getWithConfig
        { defaultConfig | headers = [ Http.header authHeaderKey userId ] }
        "/api_server/project/list"
        Projects.Msg.ProjectsReceived
        (Decode.list Projects.Model.decodeProject)


port listProjects : () -> Cmd msg


port gotProjectsList : (Decode.Value -> msg) -> Sub msg


subs =
    gotProjectsList
