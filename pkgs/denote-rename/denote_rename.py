"""Rewrite a given file to the format of the Emacs extension denote.

https://protesilaos.com/emacs/denote
"""

import argparse
import datetime
import re
import sys
from pathlib import Path


def parse_existing_filename(
    filename: str,
) -> tuple[str | list[str] | None]:
    """Parse an existing filename to extract the timestamp, title, and tags.

    Args:
        filename (str): The current filename which can be in the form of:
        YYYYMMDDTHHMMSS--SOME-TITLE__RANDOM_TAGS.extension.

    Returns:
        tuple[Optional[str], str, Optional[list[str]]]: A tuple with
        extracted timestamp, title, and tags, where timestamp and tags are optional.
        The extension gets dropped.

    """
    match = re.match(r"(\d{8}T\d{6})--([^_]+?)(?:__([^\.]+?))?(?:\.(.+))?$", filename)

    if match:
        timestamp = match.group(1)
        title = match.group(2)
        tags = match.group(3).split("_") if match.group(3) else None
        return timestamp, title, tags
    # Return None for timestamp and tags if not matched
    return None, filename, None


def format_title(raw_title: str) -> str:
    """Convert {raw_title}.

    To lowercase, replace spaces with '-' and all special characters with an
    empty string.
    """
    cleaned_title = re.sub(
        r"[\[\]\{\}!@#\$%\^&\*\(\)\+\'\"?,\.\|;:~`‘’“”/=]+",  # noqa: RUF001
        "",
        raw_title,
    )
    lowercase_title = cleaned_title.lower()
    return lowercase_title.replace(" ", "-")


def format_tags(raw_tags: list[str]) -> list[str]:
    """Convert all given raw_tags.

    To lowercase, replace spaces with '-' and all special
    characters with an empty string.
    """
    formatted_tags = []
    for tag in raw_tags:
        cleaned_tag = re.sub(
            r"[\[\]\{\}!@#\$%\^&\*\(\)\+\'\"?,\.\|;:~`‘’“”/=]+",  # noqa: RUF001
            "",
            tag,
        )
        lowercase_tag = cleaned_tag.lower()
        formatted_tags.append(lowercase_tag.replace(" ", "-"))
    return formatted_tags


def rename_file_with_creation_timestamp_and_tags(
    file_path: Path,
    tags: list[str] | None = None,
) -> None:
    """Rename a file by prefixing it with its creation timestamp.

    If the timestamp is not already present and appending new or existing tags.

    The new filename format is: {timestamp}--{user-confirmed title}__{tags}.{extension}

    Args:
        file_path (Path): The path to the file to be renamed.
        tags (Optional[list[str]]): An optional list of tags to append to the file name.

    """
    # Get the filename without extension
    original_filename = file_path.stem

    # Get existing metadata
    existing_timestamp, parsed_title, existing_tags = parse_existing_filename(
        filename=original_filename,
    )

    # Use the creation timestamp if the file is not already renamed according
    # to our format.
    if not existing_timestamp:
        creation_time = file_path.stat().st_ctime
        existing_timestamp = datetime.datetime.fromtimestamp(
            creation_time,
            tz="Europe/Zurich",
        ).strftime("%Y%m%dT%H%M%S")

    formatted_title = format_title(raw_title=parsed_title)

    input_question = (
        "Use the following title? "
        f'"{formatted_title}" (press Enter to confirm or type a new title): '
    )
    input_title = input(input_question).strip()
    if input_title == "":
        updated_title = formatted_title
    else:
        updated_title = format_title(raw_title=input_title)

    if tags is None:
        tags = existing_tags if existing_tags else []
    # remove duplicates
    tags = list(set(format_tags(tags)))
    tags.sort()

    # Join the tags with underscores if there are any
    formatted_tags = "_".join(tags) if tags else ""

    # Construct the new file name
    new_filename = f"{existing_timestamp}--{updated_title}"
    if formatted_tags:
        new_filename += f"__{formatted_tags}"
    new_filename += file_path.suffix
    rename_file(file_path, new_filename)


def rename_file(file_path: Path, new_filename: str) -> None:
    """Rename the file to new name."""
    new_file_path = file_path.with_name(new_filename)
    file_path.rename(new_file_path)
    print(f"File renamed to: {new_file_path}")


def arguments() -> argparse.Namespace:
    """Initialise script arguments."""
    parser = argparse.ArgumentParser(
        description="Rename a file with a timestamp and optional tags.",
    )
    parser.add_argument(
        "file_path",
        type=Path,
        help="The path to the file to be renamed",
    )
    parser.add_argument(
        "--tags",
        nargs="*",
        default=None,
        help="A list of optional tags associated with the file",
    )
    return parser.parse_args()


# pylint: disable=missing-function-docstring
def main() -> None:
    args = arguments()

    if args.file_path.exists():
        rename_file_with_creation_timestamp_and_tags(
            file_path=args.file_path,
            tags=args.tags,
        )
    else:
        print("File does not exist.")
        sys.exit(1)


if __name__ == "__main__":
    main()
