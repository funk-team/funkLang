1. [Canvas](#1-canvas)
2. [Atomic design](#2-atomic-design)
3. [Drawing responsive HTML in funk](#3-drawing-html-in-funk)
4. [Styling in funk](#4-styling-in-funk)
5. [Actions in funk](#5-actions-in-funk)
6. [Status](#6-canvas-status)

## 1. Canvas

The funk Canvas is where you draw HTML, style elements and connect data.

![funk-editor-screenshot][funk-editor-screenshot]

Drawing in funk is quick and easy, just draw the layout you want. The drawing interaction includes snapping, copy paste and keyboard shortcuts, just like a design tool.

![drawing-html-in-funk-gif][drawing-html-in-funk-gif]

## 2. ATOMIC design

When you draw HTML in funk you need to consider how the HTML will be structured and made responsive. funk follows the ATOMIC design principle of drawing smaller things (atoms) which are then connected to bigger things (screens)

**_Atoms_**
are the basic building blocks which other things are constructed form within funk. An atom may be an input, button or slider. Atoms can be linked to colors and styles and are not just a wireframe to be filled in later.

**_Molecules_**
are collections of atoms which are grouped together to make a larger thing. For example, an input box and a button is a form. The form does not do anything yet, it is just a collection of atoms, this is a molecule.

**_Organisms_**
are 'living' groups of molecules which do something. In the above example our form molecule may now be connected to an API - it's ALIVE and is thus an organisms speaking to the outside world.

**_Templates_**
are structural elements which can have Organisms placed within them to later form a page. A close analogy is a wireframe, where you have sketched out the location of each organism but not yet put it in place.

**_Screens_**
are templates which hold multiple Organisms. They are what you deploy to the web. Screens may be simple with only a few static Organisms or more complex requiring conditional rendering to display the correct data to the correct user.

## 3. Drawing HTML in funk

Drawing HTML in funk is similar to drawing a picture in design tools like Figma or Sketch. The Canvas has commonly used shortcuts like copy and paste and funk specific shortcuts like `E` for drawing an element or `B` for drawing a button. You can draw as many `Screens` and `Elements` as you like on the canvas, zoom around using pinch to zoom or shift+scroll wheel if you use a mouse.

You can draw two things in funk, elements and screens:

1. **Screen** The first thing you draw within the canvas is a `Screen`, it can be any size and hold multiple elements.
2. **Elements** (aka a `div`) This can hold text, data, images, video, or other elements. An elemens can also have `click events` attached (via the `Acton panel` in the RHS sidebar). When you draw an element within another element it become `nested` and a child of the element it was drawn in. You can drag an element out of their parent to change it's nesting level.

### Element types

![funk-drawing-options][funk-drawing-options]

1. **Blank Element** can hold any type of content
2. **Buttons** are elements which have been pre-styles to look and behave like a button **_(TODO apply better button styles by default)_**
3. **Text Inputs** are elements in which a USER enters text. They have specific options under the **_typograph_** style panel for styling the palceholder text and the input text. **_(TODO apply better text input styles by default)_**
4. **Text** elements which can only hold text.

You can change an element type via the `Style tab` in the RHS sidebar using the `Element type` dropdown

![funk-change-element-type][funk-change-element-type]

### Drawing for the web

When you draw in funk it's important to remember everything needs to be a `ROW` or a `COLUMN`. Sometimes you will need to draw new elements to make sure everything follows this rule.

![funk-row-right-wrong][funk-row-right-wrong]

### Making sites responsive in funk

By default the layout you draw in funk are created in `Static Mode`, this means they are not responsive and are outside the `flow` of the page (if you resize the screen the `Elements` won't resize yet). You check if an element (or group of elements) are responsive by looking for the `R` button in the screen structure panel. If the button is not there it is responsive.

To make a layout responsive select the parent elements and click the `R` button in the `Screen Structure` panel. This enters you into `Responsify` mode which automatically applies 'best guess' flexbox layout settings. We are working to add CSS grid support.

![funk-responsive][funk-responsive]

funk has a simplified set of flexbox setting such as right, left and centre align. You can control the size of an element when the screen resizes by changing its `MAXIMUM` and `MINIMUM` height and width. **A lot of the complexities of CSS are abstracted away when you design in funk. We have reduced the things you need to think about to padding, spacing and alignment.**

The best way to learn how these setting work is to try them and draw different layouts. If you get stuck pop into our Slack and we will be happy to give you a hand.

![funk-spacing-padding-align][funk-spacing-padding-align]

We are working to add breakpoints for full desktop -> mobile transitions, improving the guessed `Responsify` settings, and adding CSS grid support.

**_Presently if you change the size of any element using the drag handles you will have to re-resonsify the element, we are working to improve this._**

## 4. Styling in funk

Styling in funk is simple and quick. Select an element and expand an entry in the `Style panel` such as `Background Color`. You can add an item to the global `Design system` by clicking the `+` button.

![funk-styling-text-gif][funk-styling-text-gif]

## 5. Actions in funk

In funk `Actions` are anything a user does which requires a specific response. This might be clicking a button or navigating to a different screen. Actions can also be used to update the `Model State` to keep track of what a user should be shown, or what data should be fetched from an `API`. Read more about setting up the `Model State` in the MODEL DOCS.

**_Presently you can only connect one action per element and you are limited on what this can be connected to, we are working to improve this._**

### 6. Canvas status

Issues related to the Canvas are tagged with Canvas, view them [here](https://github.com/funk-team/funkLang/labels/Canvas).

<!-- IMAGES -->

[funk-editor-screenshot]: images/funk-canvas-overview.png
[funk-drawing-options]: images/canvas/drawing-tools-funk.png
[funk-change-element-type]: images/canvas/change-element-type.png
[funk-row-right-wrong]: images/canvas/row-right-wrong.png
[funk-spacing-padding-align]: images/canvas/funk-spacing-padding-align.png
[funk-responsify-settings]: images/canvas/funk-responsify-settings.png

<!-- GIFS -->

[funk-styling-text-gif]: images/canvas/funk-styling-text.gif
[drawing-html-in-funk-gif]: images/canvas/drawing-html-in-funk.gif
[funk-responsive]: images/canvas/responsive-design-funk.gif
