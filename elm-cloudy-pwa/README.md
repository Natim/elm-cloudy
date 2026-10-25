# elm-cloudy-pwa

Offline-first mobile PWA for quick Cloudy logbook entries. It compiles the HTTP client from [`../elm-cloudy-http-client/`](../elm-cloudy-http-client/) via `source-directories`.

## Development

Requires Elm **0.19.2** (see root CI).

```bash
npm install
npm run build:elm   # once, or after Elm changes
npm run dev         # Vite on http://localhost:5173
```

Default API base URL on the login screen is `https://cloudy.fr/api/v1`, hidden under Paramètres avancés. For local Django, use `http://127.0.0.1:8000/api/v1` and set `CORS_ALLOWED_ORIGINS` accordingly.

## Production build

```bash
npm run build
```

Static files land in `dist/` with base path `/elm-cloudy/` for GitHub Pages.

## Tests

```bash
npm test
```
