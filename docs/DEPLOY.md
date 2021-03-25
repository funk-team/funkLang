1. [Hosting](#1-hosting-with-funk)
2. [Status](#5-cli-status)

## 1. Hosting with funk

Hosting with funk is easy and flexible. You can host either with us on our global CDN or with your existing hosting provider. You will be able to push the funk specification to your Git provider and compile a funk app to highly optimised HTML, CSS and JS.

![funk-code-screenshot][funk-code-screenshot]

At the moment funk renders it's entire runtime when you deploy to our test sub domain `builtwithfunk.com`. We are working on full code compilation that will make funk apps faster than the vast majority of React apps. Several low-code tools don't even both to properly output code and just render using a runtime approach this leads to very bad performance and SEO.

We don't expect the complied funk code to be human editable, the funk language will be the way developer interact with funk when not using the visual editor.

## 5. Deploy status

Issues related to the deploying with funk are tagged with Deploy, view them [here](https://github.com/funk-team/funkLang/labels/Deploy)

<!-- IMAGES -->

[funk-code-screenshot]: images/cli/funk-cli.png
