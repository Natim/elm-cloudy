# elm-cloudy

Elm tooling for [Cloudy](https://www.cloudy.fr) pilot logbooks.

| Project | Description |
| --- | --- |
| [`elm-cloudy-http-client/`](elm-cloudy-http-client/) | Published-style Elm package `Natim/elm-cloudy`: types and HTTP client for `/api/v1`. |
| [`elm-cloudy-pwa/`](elm-cloudy-pwa/) | Offline-first mobile PWA that uses the http client. Deployed to [GitHub Pages](https://natim.github.io/elm-cloudy/). |

## Quick start

**HTTP client**

```bash
cd elm-cloudy-http-client
elm make src/Cloudy.elm --output=/dev/null
cd tests && elm-test src/Tests.elm
```

**PWA (development)**

```bash
cd elm-cloudy-pwa
npm install
npm run dev
```

**PWA (production build)**

```bash
cd elm-cloudy-pwa
npm run build
```

## License

AGPL-3.0 (see [LICENSE](LICENSE)).
