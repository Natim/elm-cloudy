(function () {
  const DB_NAME = "elm-cloudy-pwa";
  const DB_VERSION = 1;
  const STORE = "state";

  function openDb() {
    return new Promise((resolve, reject) => {
      const req = indexedDB.open(DB_NAME, DB_VERSION);
      req.onerror = () => reject(req.error);
      req.onupgradeneeded = () => {
        req.result.createObjectStore(STORE);
      };
      req.onsuccess = () => resolve(req.result);
    });
  }

  function idbGet(key) {
    return openDb().then(
      (db) =>
        new Promise((resolve, reject) => {
          const tx = db.transaction(STORE, "readonly");
          const req = tx.objectStore(STORE).get(key);
          req.onerror = () => reject(req.error);
          req.onsuccess = () => resolve(req.result);
        })
    );
  }

  function idbSet(key, value) {
    return openDb().then(
      (db) =>
        new Promise((resolve, reject) => {
          const tx = db.transaction(STORE, "readwrite");
          tx.oncomplete = () => resolve();
          tx.onerror = () => reject(tx.error);
          tx.objectStore(STORE).put(value, key);
        })
    );
  }

  function defaultState() {
    return {
      session: null,
      logbooks: [],
      logs: {},
      outbox: [],
      online: navigator.onLine,
    };
  }

  window.__cloudyPortsReady = function (app) {
    app.ports.storageLoad.subscribe(function () {
      Promise.all([
        idbGet("session"),
        idbGet("logbooks"),
        idbGet("logs"),
        idbGet("outbox"),
      ])
        .then(function (parts) {
          app.ports.storageLoaded.send({
            session: parts[0] || null,
            logbooks: parts[1] || [],
            logs: parts[2] || {},
            outbox: parts[3] || [],
            online: navigator.onLine,
          });
        })
        .catch(function () {
          app.ports.storageLoaded.send(defaultState());
        });
    });

    app.ports.storageSaveSession.subscribe(function (value) {
      idbSet("session", value);
    });

    app.ports.storageSaveLogbooks.subscribe(function (value) {
      idbSet("logbooks", value);
    });

    app.ports.storageSaveLogs.subscribe(function (value) {
      idbSet("logs", value);
    });

    app.ports.storageSaveOutbox.subscribe(function (value) {
      idbSet("outbox", value);
    });

    window.addEventListener("online", function () {
      app.ports.connectivityChanged.send(true);
    });
    window.addEventListener("offline", function () {
      app.ports.connectivityChanged.send(false);
    });

    if (app.ports.nowRequest) {
      app.ports.nowRequest.subscribe(function () {
        app.ports.nowReceived.send(new Date().toISOString());
      });
    }
  };
})();
