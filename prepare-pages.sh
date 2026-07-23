#!/bin/bash
# Assemble a static publish tree for GitHub Pages under _pages/.
#
# Expects (repo root = this site directory):
#   - _env-wasm/ (+ _env-wasm-manifest.json)
#   - lucent/dist/ (built)
#   - _env-wasm/.../httpuv/www/httpuv-sw.js (for the root SW copy)
#
# Usage: ./prepare-pages.sh
set -euox pipefail

SITE_DIR="$(pwd)"
OUT="$SITE_DIR/_pages"
SW_SRC="$SITE_DIR/_env-wasm/lib/R/library/httpuv/www/httpuv-sw.js"
MANIFEST="$SITE_DIR/_env-wasm-manifest.json"
APP_MANIFEST="$SITE_DIR/webApp/manifest.json"

rm -r $OUT 2>/dev/null || true

die() { echo "[prepare-pages] ERROR: $*" >&2; exit 1; }

[ -d "$SITE_DIR/_env-wasm/bin" ] || die "missing $SITE_DIR/_env-wasm (install/fetch the wasm prefix first)"
[ -f "$MANIFEST" ] || die "missing $MANIFEST (run create_manifest.py after installing the wasm prefix)"
[ -d "$SITE_DIR/lucent/dist" ] || die "missing lucent/dist (cd lucent && npm run build)"
[ -f "$SITE_DIR/lucent/dist/runApp.js" ] || die "missing lucent/dist/runApp.js"
[ -f "$SW_SRC" ] || die "missing $SW_SRC (install r-httpuv into the wasm prefix)"
[ -f "$SITE_DIR/index.html" ] || die "missing $SITE_DIR/index.html"
[ -d "$SITE_DIR/webApp" ] || die "missing $SITE_DIR/webApp"

echo "[prepare-pages] Writing $OUT"
rm -rf "$OUT"
mkdir -p "$OUT/lucent"

cp -f "$SITE_DIR/index.html" "$OUT/index.html"
cp -f "$SITE_DIR/style.css" "$OUT/style.css"
cp -f "$SITE_DIR/favicon.svg" "$OUT/favicon.svg"
cp -a "$SITE_DIR/webApp" "$OUT/webApp"
cp -a "$SITE_DIR/lucent/dist" "$OUT/lucent/dist"
cp -a "$SITE_DIR/_env-wasm" "$OUT/_env-wasm"
cp -f "$MANIFEST" "$OUT/_env-wasm-manifest.json"

# Root-scoped SW — GitHub Pages cannot send Service-Worker-Allowed.
cp -f "$SW_SRC" "$OUT/httpuv-sw.js"
if [ -f "$SW_SRC.map" ]; then
  cp -f "$SW_SRC.map" "$OUT/httpuv-sw.js.map"
fi

# Drop conda metadata only — keep etc/fonts (and the rest of etc/) for runtime.
rm -rf "$OUT/_env-wasm/conda-meta" "$OUT/_env-wasm/etc/conda" 2>/dev/null || true

echo "[prepare-pages] Done ($(du -sh "$OUT" | cut -f1))"
