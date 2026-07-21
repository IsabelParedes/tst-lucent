#!/usr/bin/env python3
"""Generate a VFS manifest for the wasm conda prefix under site/.

Manifest paths are absolute VFS locations (e.g. /lib/R/library/shiny/...). The
prefix tree is mounted at the Emscripten filesystem root; host-side fetching
maps each path onto the host prefix directory (default site/_env-wasm).
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def site_dir() -> Path:
    return Path(__file__).resolve().parent


def default_prefix_dir() -> Path:
    return site_dir() / "_env-wasm"


def default_out() -> Path:
    return site_dir() / "_env-wasm-manifest.json"


# Host paths relative to the prefix root that must not appear in the VFS manifest.
MANIFEST_EXCLUDE_DIRS = ("conda-meta", "etc/conda")
MANIFEST_EXCLUDE_FILES = (".mambarc",)


def is_excluded(prefix_dir: Path, path: Path) -> bool:
    rel = path.relative_to(prefix_dir).as_posix()
    if rel in MANIFEST_EXCLUDE_FILES:
        return True
    return any(rel == name or rel.startswith(f"{name}/") for name in MANIFEST_EXCLUDE_DIRS)


def vfs_path(prefix_dir: Path, path: Path) -> str:
    rel = path.relative_to(prefix_dir).as_posix()
    return f"/{rel}"


def create_manifest(prefix_dir: Path, out: Path) -> int:
    if not prefix_dir.is_dir():
        raise SystemExit(f"ERROR: prefix dir does not exist: {prefix_dir}")

    files = sorted(
        vfs_path(prefix_dir, p)
        for p in prefix_dir.rglob("*")
        if p.is_file() and not is_excluded(prefix_dir, p)
    )

    out.write_text(json.dumps({"files": files}, indent=2) + "\n")
    print(f"Wrote {len(files)} files to {out}")
    return len(files)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate a JSON manifest of VFS paths for the wasm prefix tree.",
    )
    parser.add_argument(
        "--prefix-dir",
        type=Path,
        default=default_prefix_dir(),
        help=f"Host prefix tree to scan (default: {default_prefix_dir()})",
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=default_out(),
        help=f"Output manifest path (default: {default_out()})",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    create_manifest(args.prefix_dir, args.out)


if __name__ == "__main__":
    main()
