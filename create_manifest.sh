#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
R_HOME_DIR="$SCRIPT_DIR/R_HOME"
OUT="$SCRIPT_DIR/R_HOME-manifest.json"
VFS_PREFIX="/R_HOME"

python3 - "$R_HOME_DIR" "$OUT" "$VFS_PREFIX" <<'PY'
import json
import sys
from pathlib import Path

r_home_dir = Path(sys.argv[1])
out = Path(sys.argv[2])
vfs_prefix = sys.argv[3].rstrip("/")

files = sorted(
    f"{vfs_prefix}/{p.relative_to(r_home_dir).as_posix()}"
    for p in r_home_dir.rglob("*")
    if p.is_file()
)

out.write_text(json.dumps({"files": files}, separators=(",", ":")))
print(f"Wrote {len(files)} files to {out}")
PY