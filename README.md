# ClockCard

![ClockCard logo](logo.svg)

ClockCard is a time tracking app that will also handle everything that is toilsome related to it.

The core features for this app are:

* Controlling time tracking for each issue separately through user-friendly stopwatch
* Straightforward time logging to external worklog systems (integration with JIRA and Google Spreadsheets)

## Technologies used

![Electron logo](elm-electron-logos.svg)

The app is powered by [Elm programming language](http://elm-lang.org/) + [Electron](https://electronjs.org/) . 
Elm itself has some limitations in terms of side-effect involving issues like saving the app state so Elm app cooperates
with some JavaScript when performing such tasks through Elm's builtin [Ports](https://guide.elm-lang.org/interop/ports.html) feature.


## Development
```bash
# Clone this repository
git clone git@github.com:vViktorPL/clockcard.git
# Go into the repository
cd clockcard
# Install dependencies
yarn install
# Compile sources and launch the app
yarn watch
```

## Important files

- `src/static/main.js` - Starts the app and creates a browser window to render HTML. This is the app's **main process**.
- `src/static/index.html` - A web page to render. This is the app's **renderer process**.
- `src/static/ports.js` - JS code which initializes and communicates with Elm app (performs some side-effects like App state saving)
- `src/static/base.css` - CSS styles for the App
- `src/elm/Main.elm` - Main App module source code

## Testing

[`elm-explorations/test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/) package is used for unit testing the main Elm app.
```bash
yarn test
```


## Build
### Mac app build
```bash
yarn build:mac
```
and Mac app will be built into `dist/ClockCard-darwin-x64` directory.

## Roadmap
Roadmap is described in [ROADMAP.md](ROADMAP.md) file