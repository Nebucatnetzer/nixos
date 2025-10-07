# This code is fully AI generated.
"""A script to set a file's date and time from its name or a CLI flag."""

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


def find_time_in_filename(filename: str) -> time | None:
    """Search for a time in various formats in a filename."""
    # Patterns look for common separators like 'T', '_', or '-'
    patterns = [
        (r"[\sT_-](\d{2}-\d{2}-\d{2})", "%H-%M-%S"),  # HH-MM-SS
        (r"[\sT_-](\d{6})", "%H%M%S"),  # HHMMSS
    ]

    for pattern, time_format in patterns:
        if match := re.search(pattern, filename):
            try:
                return datetime.strptime(
                    match.group(1), time_format
                ).time()  # noqa: DTZ007
            except ValueError:
                continue
    return None


def get_file_mtime(file_path: Path) -> datetime | None:
    """Get the modification time of a file as a datetime object."""
    try:
        mtime_ts = file_path.stat().st_mtime
        return datetime.fromtimestamp(mtime_ts)
    except FileNotFoundError:
        print(f"[ERROR] File not found at '{file_path}'")
        return None


def handle_file_update(
    file_path: Path,
    new_datetime: datetime,
    source: str,
    *,
    is_dry_run: bool,
) -> None:
    """Perform the file modification time update."""
    if is_dry_run:
        print(
            f"DRY RUN: Would change mtime for '{file_path.name}' to "
            f"{new_datetime.isoformat()} (from {source})."
        )
        return

    try:
        new_mtime = new_datetime.timestamp()
        current_atime = file_path.stat().st_atime  # Preserve access time
        os.utime(file_path, (current_atime, new_mtime))
        print(
            "[OK] Applied change: mtime for "
            f"'{file_path.name}' is now {new_datetime.isoformat()} "
            f"(from {source})."
        )
    except OSError as e:
        print(f"[ERROR] Error applying changes to '{file_path}': {e}")


def parse_arguments() -> argparse.Namespace:
    """Parse command-line arguments using argparse."""
    parser = argparse.ArgumentParser(
        description=(
            "Find a date/time in a filename and update the file's modification time."
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
            "A fallback date in yyyy-mm-dd format to use if no date is found"
            " in the filename."
        ),
    )
    parser.add_argument(
        "--time",
        type=str,
        default=None,
        help=(
            "A fallback time in HH:MM:SS format to use. "
            "If no time is found in the filename, this is used. "
            "If this is not provided, midnight (00:00:00) is used."
        ),
    )
    return parser.parse_args()


def main() -> None:
    """Coordinate the process of finding and setting the date and time."""
    args = parse_arguments()
    is_dry_run = not args.apply
    file_path = args.file_path

    # Determine the date part
    date_part = find_date_in_filename(file_path.name)
    date_source = "filename"
    if not date_part and args.date:
        try:
            date_part = datetime.strptime(args.date, "%Y-%m-%d").date()  # noqa: DTZ007
            date_source = "CLI --date"
        except ValueError:
            print(
                "[ERROR] Invalid date format for --date. Please use yyyy-mm-dd.",
                file=sys.stderr,
            )
            sys.exit(1)

    if not date_part:
        print(
            f"[INFO] No date in filename for '{file_path.name}',"
            " and no --date fallback provided."
        )
        return

    # Determine the time part
    time_part = find_time_in_filename(file_path.name)
    time_source = "filename"
    if not time_part and args.time:
        try:
            time_part = datetime.strptime(args.time, "%H:%M:%S").time()  # noqa: DTZ007
            time_source = "CLI --time"
        except ValueError:
            print(
                "[ERROR] Invalid time format for --time. Please use HH:MM:SS.",
                file=sys.stderr,
            )
            sys.exit(1)
    elif not time_part:
        time_part = time.min  # Default to midnight
        time_source = "default (midnight)"

    # Combine date and time
    datetime_found = datetime.combine(date_part, time_part)
    source = f"date from {date_source}, time from {time_source}"

    # Check if the file's mtime should be updated
    current_mtime = get_file_mtime(file_path)
    if current_mtime is None:
        return  # Error already printed by the helper function

    if datetime_found >= current_mtime:
        print(
            f"[SKIP] Skipping '{file_path.name}':"
            f" Found datetime ({datetime_found}) is not older "
            f"than current mtime ({current_mtime})."
        )
        return

    # If all checks pass, perform the update
    handle_file_update(file_path, datetime_found, source, is_dry_run=is_dry_run)


if __name__ == "__main__":
    main()
