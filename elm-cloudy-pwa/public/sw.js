const CACHE = "elm-cloudy-pwa-v3";
const ASSETS = [
  "/elm-cloudy/",
  "/elm-cloudy/index.html",
  "/elm-cloudy/404.html",
  "/elm-cloudy/elm.js",
  "/elm-cloudy/styles.css",
  "/elm-cloudy/ports.js",
  "/elm-cloudy/manifest.webmanifest",
  "/elm-cloudy/icon.svg",
];

self.addEventListener("install", (event) => {
  event.waitUntil(caches.open(CACHE).then((cache) => cache.addAll(ASSETS)));
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches.keys().then((keys) =>
      Promise.all(keys.filter((k) => k !== CACHE).map((k) => caches.delete(k)))
    )
  );
  self.clients.claim();
});

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (url.origin !== self.location.origin || !url.pathname.startsWith("/elm-cloudy/")) {
    return;
  }
  if (event.request.method !== "GET") {
    return;
  }
  if (url.pathname.startsWith("/elm-cloudy/api")) {
    return;
  }
  event.respondWith(
    fetch(event.request)
      .then((response) => {
        if (response.ok && response.type === "basic") {
          const copy = response.clone();
          caches.open(CACHE).then((cache) => cache.put(event.request, copy));
        }
        return response;
      })
      .catch(() => caches.match(event.request))
  );
});
