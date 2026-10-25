#!/usr/bin/env bash
# Same Elm quality gates as .github/workflows/ci.yml (format + analyse).
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pwa="${repo_root}/elm-cloudy-pwa"
client_src="${repo_root}/elm-cloudy-http-client/src"

if [[ ! -d "${pwa}/node_modules" ]]; then
  echo "Installing npm dependencies in elm-cloudy-pwa…" >&2
  (cd "${pwa}" && npm ci)
fi

export PATH="${pwa}/node_modules/.bin:${PATH}"

elm-format --validate "${client_src}/"
elm-format --validate "${pwa}/src/"

analyse_output="$(cd "${pwa}" && elm-analyse 2>&1 || true)"
printf '%s\n' "${analyse_output}"
echo "${analyse_output}" | grep -q 'Found 0 message(s)' || exit 1
if echo "${analyse_output}" | grep -q '^  > '; then
  exit 1
fi
