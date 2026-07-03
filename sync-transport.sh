#!/usr/bin/env bash
# Rebuild the r-httpuv browser transport and refresh it inside the local wasm
# R_HOME so site/ serves the current bridge/SW/socket.
#
# Canonical path (produces r-httpuv/inst/www): build the TS transport, then
# `R CMD INSTALL` r-httpuv into the wasm R_HOME via the emscripten-forge
# toolchain. The transport assets are plain, architecture-independent JS, so for
# JS-only iteration this script takes the fast path: build + copy inst/www into
# R_HOME/library/httpuv/www, then regenerate the VFS manifest.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TRANSPORT_JS_DIR="$REPO_DIR/r-httpuv/js"
INST_WWW="$REPO_DIR/r-httpuv/inst/www"
R_HOME_WWW="$SCRIPT_DIR/R_HOME/library/httpuv/www"

echo "[sync-transport] Building r-httpuv transport ($TRANSPORT_JS_DIR)"
( cd "$TRANSPORT_JS_DIR" && npm run build )

if [ ! -d "$R_HOME_WWW" ]; then
  echo "[sync-transport] ERROR: $R_HOME_WWW does not exist." >&2
  echo "[sync-transport] Install r-httpuv into the wasm R_HOME first (R CMD INSTALL)." >&2
  exit 1
fi

echo "[sync-transport] Copying inst/www -> $R_HOME_WWW"
cp -f "$INST_WWW"/* "$R_HOME_WWW"/

echo "[sync-transport] Regenerating VFS manifest"
"$SCRIPT_DIR/create_manifest.sh"

echo "[sync-transport] Done. Rebuild Lucent (site/lucent: npm run build) if needed, then: npm run serve"
