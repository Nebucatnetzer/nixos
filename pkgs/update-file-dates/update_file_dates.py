"""A script to set a file's date from its name or a CLI flag."""

from __future__ import annotations

import os
import re
import sys
from datetime import date
from datetime import datetime
from datetime import time
from datetime import timezone
from pathlib import Path

# Min year for yyyymmdd format to be considered valid
MIN_VALID_YEAR = 1940


def find_date_in_filename(filename: str) -> date | None:
    """Search for a date in yyyy-mm-dd or yyyymmdd format in a filename."""
    # First, search for the more specific 'yyyy-mm-dd' format
    if match := re.search(r"(\d{4}-\d{2}-\d{2})", filename):
        try:
            # The intermediate datetime is naive, but we only need the date part.
            return datetime.strptime(match.group(1), "%Y-%m-%d").date()  # noqa: DTZ007
        except ValueError:
            pass

    # If not found, search for the 'yyyymmdd' format
    if match := re.search(r"(\d{8})", filename):
        try:
            year = int(match.group(1)[:4])
            current_year = datetime.now(timezone.UTC).year
            if MIN_VALID_YEAR <= year <= current_year + 1:
                return (
                    datetime.strptime(match.group(1), "%Y%m%d")
                    .replace(tzinfo=timezone.UTC)
                    .date()
                )
        except ValueError:
            pass

    return None


def update_file_mtime(
    file_path: Path,
    date_obj: date,
    source: str,
    *,
    is_dry_run: bool,
) -> None:
    """Update the file's modification time (mtime), preserving access time.

    Args:
        file_path: The path to the file.
        date_obj: The date object to set the timestamp to.
        source: A string indicating where the date was found.
        is_dry_run: If True, print the action without executing it.

    """
    dt_obj = datetime.combine(date_obj, time.min)
    new_mtime = dt_obj.timestamp()

    if is_dry_run:
        print(
            f"DRY RUN: Would change mtime for '{file_path.name}' to "
            f"{date_obj.isoformat()} (from {source}).",
        )
        return

    try:
        current_atime = file_path.stat().st_atime
        os.utime(file_path, (current_atime, new_mtime))
        print(
            f"Updated mtime for '{file_path.name}' to {date_obj.isoformat()} "
            f"(from {source}).",
        )
    except OSError as e:
        print(f"Error updating mtime for '{file_path}': {e}")


def print_no_date_found(file_path: Path) -> None:
    """Print that no date was found in the filename and no fallback was given."""
    print(
        (
            f"! No date in filename for '{file_path.name}',"
            " and no --date fallback provided.",
        ),
    )


def parse_arguments(
    args: list[str],
) -> tuple[Path, bool, date | None]:
    """Parse command-line arguments.

    Args:
        args: A list of command-line arguments (excluding script name).

    Returns:
        A tuple containing the file path, dry run flag, and an optional fallback date.

    Raises:
        ValueError: If arguments are invalid.

    """
    is_dry_run = False
    fallback_date_str: str | None = None
    file_paths: list[str] = []

    i = 0
    while i < len(args):
        arg = args[i]
        if arg == "--dry-run":
            is_dry_run = True
            i += 1
        elif arg == "--date":
            if i + 1 < len(args):
                fallback_date_str = args[i + 1]
                i += 2  # Consume both the flag and its value
            else:
                message = "--date flag requires a date argument"
                raise ValueError(message)
        else:
            file_paths.append(arg)
            i += 1

    if len(file_paths) != 1:
        message = "Exactly one file path must be provided"
        raise ValueError(message)

    file_path = Path(file_paths[0])
    fallback_date = None
    if fallback_date_str:
        try:
            fallback_date = (
                datetime.strptime(
                    fallback_date_str,
                    "%Y-%m-%d",
                )
                .replace(tzinfo=timezone.UTC)
                .date()
            )
        except ValueError:
            message = "Invalid date format for --date. Please use yyyy-mm-dd."
            raise ValueError(message) from None

    return file_path, is_dry_run, fallback_date


def main() -> None:
    """Parse args and coordinate finding and setting the date."""
    try:
        file_path, is_dry_run, fallback_date = parse_arguments(sys.argv[1:])
    except ValueError as e:
        script_name = Path(sys.argv[0]).name
        print(f"Error: {e}", file=sys.stderr)
        print(
            f"\nUsage: {script_name} [--dry-run] [--date yyyy-mm-dd] <file_path>",
            file=sys.stderr,
        )
        sys.exit(1)

    date_found = None
    source = ""

    # 1. Try to find date in the filename
    if date_from_name := find_date_in_filename(file_path.name):
        date_found = date_from_name
        source = "filename"
    # 2. If not found, use the fallback date if provided
    elif fallback_date:
        date_found = fallback_date
        source = "CLI --date"

    # 3. Update file or print message based on result
    if date_found:
        update_file_mtime(
            file_path,
            date_found,
            source,
            is_dry_run=is_dry_run,
        )
    else:
        print_no_date_found(file_path)


if __name__ == "__main__":
    main()
