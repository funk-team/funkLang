module SupportSection.Components exposing (..)

import Element
import Ui.Boxicons
import Ui.Component


slack =
    buttonWithIcon Ui.Boxicons.bxlSlack "Slack" "https://funklang.slack.com/join/shared_invite/zt-kpufugyf-iaOO9lc1XWcGf_mD6YnNVw#/"


twitter =
    buttonWithIcon Ui.Boxicons.bxlTwitter "Twitter" "https://twitter.com/FunkLng/"


gitHubDiscissions =
    buttonWithIcon Ui.Boxicons.bxGit "GitHub Discission" "https://github.com/funk-team/funkLang/discussions/"


gitHubRepo =
    buttonWithIcon Ui.Boxicons.bxGit "GitHub" "https://github.com/funk-team/funkLang/"


reddit =
    buttonWithIcon Ui.Boxicons.bxlReddit "Reddit" "https://www.reddit.com/r/funkLang/"


buttonWithIcon icon text action =
    Element.newTabLink Ui.Component.buttonStyle
        { url = action
        , label =
            Element.row []
                [ Ui.Component.icon icon |> Element.el [ Element.centerY, Element.moveUp 1 ]
                , Element.el [ Element.paddingEach { top = 0, bottom = 0, left = 10, right = 0 } ] (Element.text text)
                ]
        }
