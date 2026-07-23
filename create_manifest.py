#!/usr/bin/env python3
"""Generate manifests for the site/ harness.

1. VFS manifest for the wasm conda prefix (`_env-wasm-manifest.json`).
   Paths are absolute VFS locations (e.g. /lib/R/library/shiny/...). The
   prefix tree is mounted at the Emscripten filesystem root; host-side fetching
   maps each path onto the host prefix directory (default site/_env-wasm).

2. App file list for Lucent (`webApp/manifest.json`).
   Paths are relative to the app directory (e.g. app.R, data/counties.rds).
   Browsers cannot enumerate directories over HTTP; Lucent fetches this list.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def site_dir() -> Path:
    return Path(__file__).resolve().parent


def default_prefix_dir() -> Path:
    return site_dir() / "_env-wasm"


def default_prefix_out() -> Path:
    return site_dir() / "_env-wasm-manifest.json"


def default_app_dir() -> Path:
    return site_dir() / "webApp"


def default_app_out() -> Path:
    return default_app_dir() / "manifest.json"


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


def write_manifest(out: Path, files: list[str]) -> int:
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps({"files": files}, indent=2) + "\n")
    print(f"Wrote {len(files)} files to {out}")
    return len(files)


def create_prefix_manifest(prefix_dir: Path, out: Path) -> int:
    if not prefix_dir.is_dir():
        raise SystemExit(f"ERROR: prefix dir does not exist: {prefix_dir}")

    files = sorted(
        vfs_path(prefix_dir, p)
        for p in prefix_dir.rglob("*")
        if p.is_file() and not is_excluded(prefix_dir, p)
    )
    return write_manifest(out, files)


def create_app_manifest(app_dir: Path, out: Path) -> int:
    if not app_dir.is_dir():
        raise SystemExit(f"ERROR: app dir does not exist: {app_dir}")

    files = sorted(
        p.relative_to(app_dir).as_posix()
        for p in app_dir.rglob("*")
        if p.is_file() and p.name != "manifest.json"
    )
    return write_manifest(out, files)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate wasm prefix and/or webApp file manifests.",
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
        default=default_prefix_out(),
        help=f"Wasm prefix manifest path (default: {default_prefix_out()})",
    )
    parser.add_argument(
        "--app-dir",
        type=Path,
        default=default_app_dir(),
        help=f"Shiny app directory to scan (default: {default_app_dir()})",
    )
    parser.add_argument(
        "--app-out",
        type=Path,
        default=default_app_out(),
        help=f"App manifest path (default: {default_app_out()})",
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--prefix-only",
        action="store_true",
        help="Only write the wasm prefix manifest",
    )
    group.add_argument(
        "--app-only",
        action="store_true",
        help="Only write the webApp manifest",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    do_prefix = not args.app_only
    do_app = not args.prefix_only
    if do_prefix:
        create_prefix_manifest(args.prefix_dir, args.out)
    if do_app:
        create_app_manifest(args.app_dir, args.app_out)


if __name__ == "__main__":
    main()
