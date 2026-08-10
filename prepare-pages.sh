#!/bin/bash
# Assemble a static publish tree for GitHub Pages under _pages/.
#
# Expects (repo root = this site directory):
#   - packages/ (empack_env_meta.json + *.tar.gz, including appended webApp)
#   - runtime/bin/Rmain.{js,wasm}
#   - lucent/dist/ (built; includes httpuv-web/sw/shiny-socket)
#
# Usage: ./prepare-pages.sh
set -euo pipefail

SITE_DIR="$(cd "$(dirname "$0")" && pwd)"
OUT="$SITE_DIR/_pages"
SW_SRC="$SITE_DIR/lucent/dist/httpuv-sw.js"

die() { echo "[prepare-pages] ERROR: $*" >&2; exit 1; }

[ -f "$SITE_DIR/packages/empack_env_meta.json" ] || die "missing packages/empack_env_meta.json (run ./pack-empack.sh)"
[ -f "$SITE_DIR/runtime/bin/Rmain.js" ] || die "missing runtime/bin/Rmain.js"
[ -f "$SITE_DIR/runtime/bin/Rmain.wasm" ] || die "missing runtime/bin/Rmain.wasm"
[ -d "$SITE_DIR/lucent/dist" ] || die "missing lucent/dist (cd lucent && npm run build)"
[ -f "$SITE_DIR/lucent/dist/runApp.js" ] || die "missing lucent/dist/runApp.js"
[ -f "$SW_SRC" ] || die "missing $SW_SRC (cd lucent && npm run build)"
[ -f "$SITE_DIR/index.html" ] || die "missing $SITE_DIR/index.html"
[ -d "$SITE_DIR/webApp" ] || die "missing $SITE_DIR/webApp"

echo "[prepare-pages] Writing $OUT"
rm -rf "$OUT"
mkdir -p "$OUT/lucent"

cp -f "$SITE_DIR/index.html" "$OUT/index.html"
cp -f "$SITE_DIR/style.css" "$OUT/style.css"
cp -f "$SITE_DIR/favicon.svg" "$OUT/favicon.svg"
# Source tree kept for reference/editing; runtime app comes from empack webApp archive.
cp -a "$SITE_DIR/webApp" "$OUT/webApp"
cp -a "$SITE_DIR/lucent/dist" "$OUT/lucent/dist"
cp -a "$SITE_DIR/packages" "$OUT/packages"
cp -a "$SITE_DIR/runtime" "$OUT/runtime"

# Root-scoped SW — GitHub Pages cannot send Service-Worker-Allowed.
cp -f "$SW_SRC" "$OUT/httpuv-sw.js"
if [ -f "$SW_SRC.map" ]; then
  cp -f "$SW_SRC.map" "$OUT/httpuv-sw.js.map"
fi

echo "[prepare-pages] Done ($(du -sh "$OUT" | cut -f1))"
