
1. [funk CLI](#1-funk-cli)
5. [Status](#5-cli-status)


## 1. funk CLI

The funk CLI will be used to directly connect a developers text editor to the funk visual web editor. funk outputs the funk specification which at the moment is a JSON file. We want to improve on this and output something which is more akin to a language for hybrid visual code workflows. This way a developer will be able to write code as they do now and work in a more collaborative way with non-developers. We use our own language to ensure anything written in funk will always work with both visual and code workflows.

![funk-CLI][funk-CLI]

In addition to just syncing the funk specification you will also be able to write custom-web components in your text editor and directly insert them into the funk editor with visual hooks to style and connect data to them. This way we can ensure you will always be able to build what you want with funk, just like you do with React, WordPress or VueJs. An advantage of using web components is they work across frameworks and are typically more performant than frameworks specific components.

Needless to say this kind of browser to text editor binding is difficult to build as we need to replicate a filesystem in the browser which also works with our backend and your text editor. We have a built a POC for the CLI and plan to release a beta of it as soon as possible. If you would like to help build or test it please get in touch on Slack or Twitter.

## 5. CLI status

Issues related to the funk CLI are tagged with CLI, view them here [here](https://github.com/funk-team/funkLang/labels/CLI)


<!-- IMAGES -->
[funk-CLI]: images/cli/funk-cli.png
