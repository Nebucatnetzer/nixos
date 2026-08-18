import argparse
import csv
import os
import re
import sys
import tempfile
from pathlib import Path


def clean_text(text: str) -> str:
    if not text:
        return ""

    # Order matters: more specific prefixes first but if you just use one word per line
    # it will loop through them until it doesn't find a match anymore.
    prefixes = [
        r"einkauf\s+",
        r"gutschrift\s+",
        r"online\s+",
        r"sumup\s+",
        r"twint\s+",
        r"zahlung\s+",
    ]

    # Use a loop to strip prefixes in case they are nested
    # and use re.IGNORECASE for the capitalization
    cleaned = text
    changed = True
    while changed:
        original = cleaned
        for p in prefixes:
            cleaned = re.sub(f"^{p}", "", cleaned, flags=re.IGNORECASE)
        if original == cleaned:
            changed = False

    # Remove everything from the first date (DD.MM.YYYY) onwards
    cleaned = re.split(r"\d{2}\.\d{2}\.\d{4}", cleaned)[0]
    return cleaned.strip().rstrip(",")


def main() -> None:
    parser = argparse.ArgumentParser(description="Cleanup bank CSVs for Actual Budget")
    parser.add_argument("input", help="Input CSV file")
    parser.add_argument(
        "-i", "--in-place", action="store_true", help="Update the file in place"
    )
    parser.add_argument("-o", "--output", help="Output CSV file (stdout if omitted)")
    args = parser.parse_args()

    input_path = Path(args.input)
    if not input_path.exists():
        print(f"Error: File {args.input} not found", file=sys.stderr)
        sys.exit(1)

    # Create a temporary file if in-place or output is specified
    temp_file = None
    if args.in_place:
        fd, temp_path = tempfile.mkstemp(dir=input_path.parent, suffix=".tmp")
        temp_file = os.fdopen(fd, "w", encoding="utf-8", newline="")
    elif args.output:
        temp_file = Path.open(args.output, "w", encoding="utf-8", newline="")
    else:
        temp_file = sys.stdout

    try:
        with Path.open(input_path, mode="r", encoding="utf-8") as infile:
            reader = csv.reader(infile, delimiter=";")
            try:
                header = next(reader)
            except StopIteration:
                return

            try:
                text_idx = header.index("Text")
            except ValueError:
                print("Error: Could not find 'Text' column in CSV", file=sys.stderr)
                sys.exit(1)

            writer = csv.writer(temp_file, delimiter=";")
            writer.writerow(header)

            for row in reader:
                if len(row) > text_idx:
                    row[text_idx] = clean_text(row[text_idx])
                writer.writerow(row)

    finally:
        if temp_file is not sys.stdout:
            temp_file.close()

    if args.in_place:
        # Atomic rename to replace original file
        Path(temp_path).replace(input_path)


if __name__ == "__main__":
    main()
