# elm-cloudy

Elm client for the [Cloudy](https://www.cloudy.fr) logbook REST API (`/api/v1`), inspired by [elm-kinto](https://github.com/Kinto/elm-kinto).

## Install

```bash
elm install Natim/elm-cloudy
```

Or add a dependency on this repository in `elm.json` while the package is unpublished.

## Layers

- **`Cloudy.Data`** — types and JSON codecs (no HTTP).
- **`Cloudy.Client`**, **`Cloudy.Request`**, **`Cloudy`** — configure a client, build requests, and `send` / `sendEmpty`.

Per logbook category, log and report records live under `Cloudy.Data.Log.*` and `Cloudy.Data.Report.*` (generated from OpenAPI).

## Quick example

```elm
import Cloudy
import Cloudy.Client as Client
import Cloudy.Request as Request

login : Cmd Msg
login =
    Client.default
        |> Request.send GotToken
        |> (\_ -> Cmd.none)
```

Typical flow:

1. `Cloudy.token client { email, password }` → `Request TokenResponse`
2. `Client.withBearer accessToken client` for authenticated calls
3. `Cloudy.listLogbooks`, `Cloudy.listLogs`, `Cloudy.createLog`, etc.
4. `Request.send` for JSON responses; `Request.sendEmpty` for `DELETE` (204)
5. Log attachments: `Cloudy.uploadLogFile client logbookId logId file` (multipart; use `Request.send`), then `Cloudy.deleteLogFile client logbookId logId fileId` (`Request.sendEmpty`)
6. Aircraft photo: `Cloudy.uploadAircraftPhoto client logbookId aircraftId file` (multipart; use `Request.send`). The newest photo becomes `photo_url`.

OpenAPI snapshot: [`vendor/cloudy-openapi.json`](vendor/cloudy-openapi.json). Regenerate Elm types:

```bash
python3 scripts/generate_elm.py
```

## Development

```bash
elm-format --validate src/
elm make src/Cloudy.elm
cd tests && elm-test
```

## License

AGPL-3.0 (same family as Cloudy).
