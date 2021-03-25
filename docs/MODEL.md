1. [Model editor](#1-model-editor)
2. [Data types](#2-data-types)
3. [Connecting the model](#3-connecting-the-model-to-an-element)
4. [Status](#5-code-status)

## 1. Model editor

The funk Model editor stores the state of your application. This might be what tab a user has clicked on, if they are logged in or what element they should be shown based on a prior action or external data.

![funk-model-screenshot][funk-model-screenshot]

## 2. Data types

Presently it is possible to store `strings` `ints` and `custom types` in the model. Strings and ints are for numbers and text and can be connected to an input source, like a text input box, or a value from an external API.

Custom types are a little more complex, but a lot more powerful. A custom type is a set of possible states something can be in, such as a login state (authenticated, logged in, anonymous) or what tab is selected (tab 1, tab 2, tab 3).

A custom type can hold data such as an object representing a users profile information, or the image to be displayed to a user when a specific button is pressed. In funk we call this an `association` as you are associating this type with some data.

This simple concept allows you to store complex application states while also keeping your application well organised.

When you link this state management with the Canvas you have a model-view-update application cycle where you first define a model, like it to a view and update the model based on a user action, such as a click event.

## 3. Connecting the model to an element

You can connect the funk model to the Canvas using the right hand sidebar under the Connect tab. You can tell the model to update when a user clicks an element using the Action tab. When you use Actions and the Model connections together you can create custom logic and, eventually, conditional rendering based on the application state.

![funk-model-connect][funk-model-connect]

**_We are working to add support for components which will enable the model to hold not just data but also elements._**

## 4. Code status

Issues related to the Code editor are tagged with Code-Editor, view them
[here](https://github.com/funk-team/funkLang/labels/Model-editor)

<!-- IMAGES -->

[funk-model-screenshot]: images/model/funk-model-overview.png

<!-- GIF -->

[funk-model-connect]: images/model/funk-model-connect.gif
