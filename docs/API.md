1. [API editor](#1-api-editor)
2. [Making a GET request in funk](#2-making-a-get-request-in-funk)
3. [Connecting data within funk](#3-connecting-data-within-funk)
4. [Making a POST request](#4-making-a-POST-request-in-funk)
5. [Transforming API data](#5-Transforming-api-data)
6. [Status](#6-api-status)

## 1. API editor

The funk API editor is where you GET or POST data to an external API. You can also upload a mock data file to represent an API response if you don't have an API yet. We will also plan to support GraphQL once we have a full REST framework.

![funk-api-screenshot][funk-api-screenshot]

## 2. Making a GET request in funk

Making GET requests in funk is easy. Enter the URL of the API you want to get data from and click 'Make Get Request'. The response from the API is automatically fetched and parsed into a selectable list.

You can select either individual or entire lists. A list might be a list of posts a user has made or a list of friends they have. You can render entire lists within the Canvas too.

If you want you can also mock an API by uploading a JSON file, this is useful if you don't yet have an API to get data from. You can also copy a CURL command and it will automatically be parsed into the funk visual environment.

## 3. Connecting data from an API to the Cavnas

Once you've selected some data from an API you can directly connect it to an element within the canvas. In the RHS sidebar under the `Content` and `API` tab select the API you've just created and select it

![funk-connect-data][funk-connect-data]

## 4. Making a POST request in funk

To POST data to an API you first need to setup what data will be sent and the format in which the API expects to receive it. You can do this by entering `test JSON data`. This JSON is then parsed by funk. The editor now knows the type of data you want to POST and the structure the API wants to receive it in.

To make the API call when a user does something, like clicking a `Submit` button, create an Action in the RHS panel and connect it into the POST endpoint you just created.

![funk-post-api-1][funk-post-api-1]

**_POST requests are still work in progress, we have a couple of bugs and feature additions before this feature is completed_**

## 5. Transforming API data

In addition to directly placing data within the funk canvas you can also transform data you have received from an API using code (we will also be building a visual data transformer for common tasks). Check the CODE documentation for more details on how this [works](https://github.com/funk-team/funkLang/blob/master/docs/CODE.md).

## 6. API status

Issues related to the API editor are tagged with API-Editor, view them [here](https://github.com/funk-team/funkLang/labels/API-Editor)

<!-- IMAGES -->

[funk-api-screenshot]: images/api/funk-api-overview.png
[funk-connect-api-1]: images/api/funk-connect-api-1.png
[funk-connect-api-2]: images/api/funk-connect-api-2.png
[funk-connect-api-3]: images/api/funk-connect-api-3.png
[funk-post-api-1]: images/api/funk-post-api-1.png

<!-- GIF -->

[funk-connect-data]: images/api/funk-connect-data.gif
