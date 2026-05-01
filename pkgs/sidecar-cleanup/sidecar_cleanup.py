"""Find and optionally remove orphaned Darktable XMP sidecar files.

A sidecar is orphaned when no matching DNG or ARW file exists in the same
directory. Darktable names sidecars as ``photo.dng.xmp``; plain ``photo.xmp``
sidecars are also supported.
"""

import argparse
import sys
from pathlib import Path

RAW_SUFFIXES: frozenset[str] = frozenset({".dng", ".arw"})


def _raw_exists(path: Path) -> bool:
    return path.exists() or path.with_suffix(path.suffix.upper()).exists()


def find_orphaned_sidecars(directory: Path) -> list[Path]:
    """Return XMP sidecars in *directory* that have no matching raw file.

    Scans recursively into subdirectories.
    """
    orphaned: list[Path] = []
    for xmp in sorted(directory.glob("**/*.xmp")):
        companion = xmp.parent / xmp.stem
        if companion.suffix.lower() in RAW_SUFFIXES:
            if not _raw_exists(companion):
                orphaned.append(xmp)
        else:
            has_raw = any(
                _raw_exists(xmp.parent / (xmp.stem + ext)) for ext in RAW_SUFFIXES
            )
            if not has_raw:
                orphaned.append(xmp)
    return orphaned


def arguments() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description=(
            "Find orphaned Darktable XMP sidecar files. "
            "Pass --remove to delete them."
        ),
    )
    parser.add_argument(
        "directory",
        type=Path,
        help="Directory to scan for orphaned XMP sidecars",
    )
    parser.add_argument(
        "--remove",
        action="store_true",
        help="Delete orphaned sidecar files (dry-run without this flag)",
    )
    return parser.parse_args()


def main() -> None:
    """Entry point."""
    args = arguments()

    if not args.directory.is_dir():
        print(f"Error: '{args.directory}' is not a directory.")
        sys.exit(1)

    orphaned = find_orphaned_sidecars(args.directory)

    if not orphaned:
        print("No orphaned sidecar files found.")
        return

    if args.remove:
        for xmp in orphaned:
            xmp.unlink()
            print(f"Removed: {xmp}")
    else:
        for xmp in orphaned:
            print(xmp)
        count = len(orphaned)
        print(f"\n{count} orphaned sidecar(s) found. Use --remove to delete them.")


if __name__ == "__main__":
    main()
