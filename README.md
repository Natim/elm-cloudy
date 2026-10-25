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

## Pre-commit

Install [pre-commit](https://pre-commit.com/) once, then hooks run on each commit (same `elm-format` and `elm-analyse` steps as CI):

```bash
pip install pre-commit   # or your OS package
pre-commit install
```

Run manually on all files:

```bash
pre-commit run --all-files
```

Or only the Elm checks:

```bash
./scripts/pre-commit-elm-checks.sh
```

## License

AGPL-3.0 (see [LICENSE](LICENSE)).
