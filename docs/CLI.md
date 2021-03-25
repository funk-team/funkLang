1. [funk CLI](#1-funk-cli)
2. [Status](#5-cli-status)

## 1. funk CLI

The funk CLI will be used to directly connect a developers text editor to the funk visual web editor. funk outputs the funk specification which at the moment is a JSON file. We want to improve on this and output something which is more akin to a language for hybrid visual code workflows. This way a developer will be able to write code as they do now and work in a more collaborative way with non-developers. We use our own language to ensure anything written in funk will always work with both visual and code workflows.

![funk-CLI][funk-cli]

In addition to just syncing the funk specification you will also be able to write custom-web components in your text editor and directly insert them into the funk editor with visual hooks to style and connect data to them. This way we can ensure you will always be able to build what you want with funk, just like you do with React, WordPress or VueJs. An advantage of using web components is they work across frameworks and are typically more performant than frameworks specific components.

Needless to say this kind of browser to text editor binding is difficult to build as we need to replicate a filesystem in the browser which also works with our backend and your text editor. We have a built a POC for the CLI and plan to release a beta of it as soon as possible. If you would like to help build or test it please get in touch on Slack or Twitter.

## Example funk spec

This is a work in progress outline of what we want the funk spec should look like. We are open to suggestions about how it should be formatted and look/feel.

`state.yaml`

```yaml
dataSources:
      counter:
    type: State Int
    init: 0
    action:
      Increment: @builtin/arithmetics/add(1)
      Decrement: @builtin/arithmetics/subtract(1)
  login:
    type: Auth
    endpoints:
      user:
        type: Nullable
        output:
          name: string
          id: UserId
  imdb:
    types:
      MovieId: String
    type: HttpAPI
    baseUrl: @config/env/imdbBaseApi
    endpoints:
      movies:
        realTime: false
        input:
          name: String
        output:
          list(movie in response.data.whatever.path.to.the.payload):
            name: movie.name : String
            rating: movie.rating : Int 1-5
            id: movie.id : MovieId
      movie:
        input:
          id: MovieId
        output:
          name: .name
          rating: .rating
```

`view.yaml`

```yaml
views:
  titleBar:
    els:
      logo:
        type: @builtin/image
        content:
          image: staticAssets/logo
      userPanel:
        fallbackForNull: @views/loginButton
        type: @builtin/text
        content:
          text: @dataSources/login/user.name
  movieDetails:
    requiredContext:
      @dataSources.imdb.MovieId
    els:
      coverArt:
        content:
          image: @dataSources/imdb/movie.coverArt
      rating:
        els:
          improveRatingButton:
            content:
              text: Rating +1
            action:
              @dataSources.counter.increment
  movieOverview:
    els:
      results:
        repeatOn: for movie in dataSources.imdb.movies(query: 'david')
        repeatWith:
          els:
            movieTitle:
              description: The title of a single movie in the search results list
              type: @builtin/text
              layout: ...
              styles: ...
              content:
                text: @movie.name
            linkToMovie:
              description: A link the user can click to go to the movie
              type: @builtin/button
              content:
                text: 'details'
              action:
                navigateTo: @views/movieDetails/movie.id
```

`features.yaml`

```yaml
auth:
    providers:
        google: true
        facebook: false
        email/password: true
    two-factor: true
    whitelines: david@funk.app
    only-password: 12345
full-text-search:
    indexes:
        movies: @dataSources/imdb
```

## 2. CLI status

Issues related to the funk CLI are tagged with CLI, view them here [here](https://github.com/funk-team/funkLang/labels/CLI)

<!-- IMAGES -->

[funk-cli]: images/cli/funk-cli.png
