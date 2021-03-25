1. [Code editor](#1-code-editor)
2. [Writing code in the funk editor](#2-writing-code-within-the-funk-editor)
3. [Writing code in your own editor](#3-writing-code-in-your-own-text-editor)
4. [Feeding data into the Editor](#4-feeding-data-into-elements)
5. [Status](#5-code-status)

## 1. Code editor

The funk Code editor can be used to transform data your app gets via an API or user input. You can either write JS within the funk editor or connect funk to your text editor directly using our CLI (to be released soon). After you've transformed data you can connect it within your app visually.

![funk-code-screenshot][funk-code-screenshot]

## 2. Writing code within the funk editor

You can write code directly in the funk editor, an example is shown below.

```
import dayjs from 'https://cdn.skypack.dev/dayjs';


export default async (/*args-start*/ listOfPosts /*args-end*/) => {
    var formatedDate = []
    for (const [key, value] of Object.entries(listOfPosts.articles)) {
        var dateObj = dayjs(value.updatedAt)
        formatedDate.push(`${dateObj.day()}-${dateObj.month()}-${dateObj.year()}`)

    }
    return formatedDate
}
```

Things to note:

1. You can use NPM packages from www.skypack.dev
2. The value returned from the `export default async` function is used within the funk editor
3. You must visually click on the returned value at the bottom of the Code editor otherwise it won't be available within funk

## 3. Writing code in your own text editor

funk was developed by developers, we know you want to write code in your own editor, we want to as well. We have an experimental CLI which will enable you to have this close workflow between code and visual environments. If you want to help us build this feature check the issue list and contact us on Slack or Twitter if you have time to help.

## 4. Feeding data into elements

At the moment you can only feed data from the Code editor into the length and width fields, we are working to allow data to be fed into any element.

## 5. Code status

Issues related to the Code editor are tagged with Code-Editor, view them [here](https://github.com/funk-team/funkLang/labels/Code-editor)

<!-- IMAGES -->

[funk-code-screenshot]: images/code/funk-code-overview.png
