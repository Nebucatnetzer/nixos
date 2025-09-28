"""A command-line script to rename a file to a timestamp format.

This script renames a target file to the 'yyyymmddThhmmss.extension' format.
By default, it uses the file's last modification time. An optional '--date'
argument allows specifying a custom date and time in ISO format.
"""

import argparse
import sys
from datetime import datetime
from datetime import UTC
from pathlib import Path


def _get_datetime_from_input(filepath: Path, date_str: str | None) -> datetime:
    """Return a datetime object from either a date string or the file's mtime.

    Args:
        filepath: The path to the file, used to get the modification time.
        date_str: An optional ISO format date string. If provided, this is parsed.
                  If None, the file's mtime is used.

    Returns:
        A datetime object for the new filename.

    Raises:
        ValueError: If the provided date string has an invalid format.

    """
    if date_str:
        try:
            # datetime.fromisoformat is flexible and handles various ISO 8601 formats
            return datetime.fromisoformat(date_str)
        except ValueError as e:
            message = (
                f"Error: Invalid date format for '{date_str}'. "
                "Please use ISO format (e.g., 'YYYY-MM-DD' or 'YYYY-MM-DDTHH:MM:SS')."
            )
            raise ValueError(message) from e

    # Get modification time as a timestamp and convert it to a datetime object
    mtime_timestamp = filepath.stat().st_mtime
    return datetime.fromtimestamp(mtime_timestamp, tz=UTC)


def rename_file(filepath: Path, date_str: str | None = None) -> None:
    """Rename a file to 'yyyymmddThhmmss.extension' based on a timestamp.

    Args:
        filepath: The path to the file to rename.
        date_str: An optional ISO format date string (e.g., 'YYYY-MM-DDTHH:MM:SS')
                  to use for the new filename. If None, the file's last
                  modification time is used instead.

    Raises:
        ValueError: If the provided date string is in an invalid format.
        FileExistsError: If a file with the new name already exists.
        PermissionError: If the script lacks permissions to rename the file.

    """
    dt_obj = _get_datetime_from_input(filepath, date_str)

    # Format the datetime object into the 'yyyymmddThhmmss' string
    new_stem = dt_obj.strftime("%Y%m%dT%H%M%S")
    new_filename = f"{new_stem}{filepath.suffix}"
    new_filepath = filepath.with_name(new_filename)

    if new_filepath.exists():
        message = f"Error: A file named '{new_filepath.name}' already exists."
        raise FileExistsError(message)

    # Perform the rename operation
    filepath.rename(new_filepath)
    print(f"Successfully renamed '{filepath.name}' to '{new_filepath.name}'")


def main() -> None:
    """Parse command-line arguments and execute the file renaming logic."""
    parser = argparse.ArgumentParser(
        description=(
            "Rename a file to a timestamp format (yyyymmddThhmmss.ext) "
            "based on its modification time or a specified date."
        ),
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        "filepath", type=Path, help="The path to the file you want to rename."
    )
    parser.add_argument(
        "--date",
        type=str,
        help=(
            "An optional date/time string in ISO format to use instead of the\n"
            "file's modification time. Examples:\n"
            "'2025-09-28'\n"
            "'2025-09-28T15:30:00'"
        ),
        default=None,
    )

    args = parser.parse_args()

    try:
        rename_file(args.filepath, args.date)
    except (ValueError, FileExistsError, PermissionError) as e:
        print(e, file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
