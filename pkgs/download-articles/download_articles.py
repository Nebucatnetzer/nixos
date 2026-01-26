"""Module for batch-downloading ePub files from Push to Kindle service.

This script parses an Emacs Org-mode file for URLs, sends them to the
Fivefilters conversion API, and saves the resulting ePub files to a
specified directory using the server-provided filenames.
"""

import argparse
import re
import shutil
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Final
from urllib.error import HTTPError
from urllib.error import URLError

# Type aliases for clarity (PEP 695)
type URL = str
type FilePath = str | Path

# Constants
USER_AGENT: Final[str] = "Mozilla/5.0"


def get_urls_from_org(file_path: FilePath) -> list[URL]:
    """Extract all unique URLs from a given .org file.

    Search the file content for http/https patterns and return them as a
    list while preserving the order of discovery.
    """
    url_pattern: re.Pattern[str] = re.compile(r"https?://[^\s\]>]+")
    try:
        path = Path(file_path)
        content: str = path.read_text(encoding="utf-8")
        return list(dict.fromkeys(url_pattern.findall(content)))
    except FileNotFoundError:
        print(f"Error: File '{file_path}' not found.")
        return []


def download_epub(target_url: URL, output_dir: Path) -> None:
    """Download an ePub and save it using the server's filename.

    Send a request to the Push to Kindle API and stream the response to
    a file in the output directory. Catch specific network and
    filesystem exceptions to provide detailed error feedback.
    """
    encoded_url: str = urllib.parse.quote(target_url, safe="")
    api_url: str = (
        f"https://pushtokindle.fivefilters.org/send.php?"
        f"context=download&format=epub&url={encoded_url}&links=1"
    )

    req = urllib.request.Request(api_url, headers={"User-Agent": USER_AGENT})

    try:
        print(f"Processing: {target_url}")

        with urllib.request.urlopen(req) as response:
            content_disp: str = response.headers.get("Content-Disposition", "")
            filename_match: re.Match[str] | None = re.search(
                r'filename="(.+?)"', content_disp
            )

            filename: str = (
                filename_match.group(1)
                if filename_match
                else f"download_{hash(target_url)}.epub"
            )
            save_path: Path = output_dir / filename

            # Use shutil.copyfileobj to stream data efficiently (Avoids FURB122)
            with open(save_path, "wb") as out_file:
                shutil.copyfileobj(response, out_file)

            print(f"  -> Saved: {filename}")

    except HTTPError as e:
        print(f"  -> HTTP Error {e.code}: {e.reason} (URL: {target_url})")
    except URLError as e:
        print(f"  -> Network Error: {e.reason} (Check your connection)")
    except OSError as e:
        print(f"  -> File System Error: {e.strerror} (Path: {e.filename})")
    except Exception as e:
        print(f"  -> Unexpected Error: {type(e).__name__}: {e}")


def main() -> None:
    """Parse command line arguments and execute the download loop.

    Handle the input file reading, directory creation, and sequential
    processing of all discovered URLs.
    """
    parser = argparse.ArgumentParser(
        description="Batch Kindle ePub downloader using standard library."
    )
    parser.add_argument("--input-file", required=True, help="Path to the .org file.")
    parser.add_argument(
        "--output-directory", required=True, help="Directory to save ePubs."
    )

    args: argparse.Namespace = parser.parse_args()

    input_path: Path = Path(args.input_file).expanduser().resolve()
    output_path: Path = Path(args.output_directory).expanduser().resolve()

    output_path.mkdir(parents=True, exist_ok=True)

    urls: list[URL] = get_urls_from_org(input_path)

    if not urls:
        print("No URLs found to process.")
        return

    print(f"Found {len(urls)} URLs. Starting downloads...")
    for url in urls:
        download_epub(url, output_path)


if __name__ == "__main__":
    main()
