module Runtime.Msg exposing (..)

import Browser
import Preview.Msg
import RemoteData
import Spec.Model
import Url


type Msg
    = PreviewMsg Preview.Msg.Msg
    | SpecRetrieved (RemoteData.WebData Spec.Model.Spec)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
