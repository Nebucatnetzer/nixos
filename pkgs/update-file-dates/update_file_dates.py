# This code is fully AI generated.
"""A script to set a file's date from its name or a CLI flag."""

from __future__ import annotations

import argparse
import os
import re
import sys
from datetime import date
from datetime import datetime
from datetime import time
from pathlib import Path

# Min year for yyyymmdd format to be considered valid
MIN_VALID_YEAR = 1940


def find_date_in_filename(filename: str) -> date | None:
    """Search for a date in various formats in a filename."""
    patterns = [
        (r"(\d{4}-\d{2}-\d{2})", "%Y-%m-%d"),  # yyyy-mm-dd
        (r"(\d{2}-\d{2}-\d{2})", "%y-%m-%d"),  # yy-mm-dd
        (r"(\d{8})", "%Y%m%d"),  # yyyymmdd
    ]

    for pattern, date_format in patterns:
        if match := re.search(pattern, filename):
            try:
                # Special check for yyyymmdd to avoid matching random numbers
                if date_format == "%Y%m%d":
                    year = int(match.group(1)[:4])
                    current_year = datetime.now().year
                    if not (MIN_VALID_YEAR <= year <= current_year + 1):
                        continue  # Skip if year is out of a reasonable range

                # The intermediate datetime is naive, but we only need the date part.
                return datetime.strptime(  # noqa: DTZ007
                    match.group(1),
                    date_format,
                ).date()
            except ValueError:
                continue  # Try the next pattern if this one fails to parse
    return None


def get_file_mtime_date(file_path: Path) -> date | None:
    """Get the modification time of a file as a date object."""
    try:
        mtime_ts = file_path.stat().st_mtime
        return datetime.fromtimestamp(mtime_ts).date()
    except FileNotFoundError:
        print(f"[ERROR] File not found at '{file_path}'")
        return None


def handle_file_update(
    file_path: Path,
    new_date: date,
    source: str,
    *,
    is_dry_run: bool,
) -> None:
    """Perform the file modification time update."""
    if is_dry_run:
        print(
            f"DRY RUN: Would change mtime for '{file_path.name}' to "
            f"{new_date.isoformat()} (from {source})."
        )
        return

    try:
        dt_obj = datetime.combine(new_date, time.min)
        new_mtime = dt_obj.timestamp()
        current_atime = file_path.stat().st_atime  # Preserve access time
        os.utime(file_path, (current_atime, new_mtime))
        print(
            "[OK] Applied change: mtime for "
            f"'{file_path.name}' is now {new_date.isoformat()} "
            f"(from {source})."
        )
    except OSError as e:
        print(f"[ERROR] Error applying changes to '{file_path}': {e}")


def parse_arguments() -> argparse.Namespace:
    """Parse command-line arguments using argparse."""
    parser = argparse.ArgumentParser(
        description=(
            "Find a date in a filename and update the file's modification time."
        ),
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        "file_path",
        type=Path,
        help="The path to the file to process.",
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Apply the changes. Without this flag, the script runs in dry-run mode.",
    )
    parser.add_argument(
        "--date",
        type=str,
        default=None,
        help=(
            "A fallback date in yyyy-mm-dd format"
            " to use if no date is found in the filename."
        ),
    )
    return parser.parse_args()


def main() -> None:
    """Coordinate the process of finding and setting the date."""
    args = parse_arguments()
    is_dry_run = not args.apply
    file_path = args.file_path

    # Determine the date to use, from filename or fallback
    date_found = find_date_in_filename(file_path.name)
    source = "filename"
    if not date_found and args.date:
        try:
            date_found = datetime.strptime(args.date, "%Y-%m-%d").date()  # noqa: DTZ007
            source = "CLI --date"
        except ValueError:
            print(
                "[ERROR] Invalid date format for --date. Please use yyyy-mm-dd.",
                file=sys.stderr,
            )
            sys.exit(1)

    # If no date could be determined, exit
    if not date_found:
        print(
            f"[INFO] No date in filename for '{file_path.name}',"
            " and no --date fallback provided."
        )
        return

    # Check if the file's mtime should be updated
    current_mtime = get_file_mtime_date(file_path)
    if current_mtime is None:
        return  # Error already printed by the helper function

    if date_found >= current_mtime:
        print(
            f"[SKIP] Skipping '{file_path.name}':"
            f" Found date ({date_found}) is not older "
            f"than current mtime ({current_mtime})."
        )
        return

    # If all checks pass, perform the update
    handle_file_update(file_path, date_found, source, is_dry_run=is_dry_run)


if __name__ == "__main__":
    main()
