
1. [Design System](#1-design-system)
2. [Connecting the Design System to an element](#2-connecting-the-design-system-to-an-element)
3. [Animations, Hovere styles etc..](#3-animations-hover-styles-and-conditional-styling)
4. [Status](#4-design-system-status)


## 1. Design System

The funk Design System is a central hub for all the styles within your app. You can link styles to a specific element and modify them either in the RHS panel in the Canvas or in the Design System tab. We want styling in funk to be akin to styling in design tools like Sketch and Figma.

If you need to write custom CSS you will be able to do this too, but we hope we will be able to cover 95% of styling use cases without you having to resort to CSS.

***Presently we support a limited subset of the design options we want to. We will be adding more advances styling options soon***

![funk-design-screenshot][funk-design-screenshot]


## 2. Connecting the Design System to an element  

You can connect multiple elements to the same item in the Design System and have the same style applied to them. When you update a style in the Design system any connected elements are updated with the new style.

![funk-design-update-canvas][funk-design-update-canvas]

When you first apply a style to an element it is automatically set to a `Custom` style, you can change this style to one in the Design System using the dropdown menu. If you press the `+` button any custom styles you've set are added to the Design System so you can re-use them elsewhere.

You can change the name of any non-built in styles by clicking on their name in the dropdown menu or in the Design tab.

![funk-design-linking][funk-design-linking]


## 3. Animations, hover styles and conditional styling

We are actively working to improve and add more styling features to funk, if you want to help us build any of these features contact us, or check GitHub issues to see if we are already working on what you need to use.

## 4. Design System status

Issues related to the Design System are tagged with Design-System, view them [here](https://github.com/funk-team/funkLang/labels/Design-System)




<!-- IMAGES -->
[funk-design-screenshot]: images/design/funk-design-overview.png

<!-- GIFS -->
[funk-design-linking]: images/design/funk-design-system-linking.gif
[funk-design-update-canvas]: images/design/funk-design-update-canvas.gif
