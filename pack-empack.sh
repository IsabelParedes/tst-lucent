#!/bin/bash
# Pack the wasm run env + webApp for Lucent (empack → packages/, Rmain → runtime/).

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PREFIX=${PREFIX:-$SCRIPT_DIR/_prefix-wasm}
PACKAGES_DIR=$SCRIPT_DIR/packages
RUNTIME_DIR=$SCRIPT_DIR/runtime
WEBAPP_DIR=$SCRIPT_DIR/webApp

set -eux

mkdir -p "$PACKAGES_DIR" "$RUNTIME_DIR/bin" "$RUNTIME_DIR/lib/R/lib"

empack pack env --env-prefix "$PREFIX" --outdir "$PACKAGES_DIR"

empack pack dir \
  --host-dir "$WEBAPP_DIR" \
  --mount-dir /webApp \
  --outname webApp.tar.gz \
  --outdir "$PACKAGES_DIR"

empack pack append \
  --env-meta "$PACKAGES_DIR/empack_env_meta.json" \
  --tarfile "$PACKAGES_DIR/webApp.tar.gz"

# Bootstrap HTTP assets: Rmain must load before VFS populate; libR*.so are
# SIDE_MODULEs requested during that load (see r-main glue/).
cp "$PREFIX/bin/Rmain.js" "$RUNTIME_DIR/bin/Rmain.js"
cp "$PREFIX/bin/Rmain.wasm" "$RUNTIME_DIR/bin/Rmain.wasm"
cp "$PREFIX/lib/R/lib/libR.so" "$RUNTIME_DIR/lib/R/lib/libR.so"
cp "$PREFIX/lib/R/lib/libRblas.so" "$RUNTIME_DIR/lib/R/lib/libRblas.so"
cp "$PREFIX/lib/R/lib/libRlapack.so" "$RUNTIME_DIR/lib/R/lib/libRlapack.so"
