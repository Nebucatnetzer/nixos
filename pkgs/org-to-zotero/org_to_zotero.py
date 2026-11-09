"""Save every URL in a text file, for example an org file, into desktop Zotero.

Playwright opens Chromium with the Zotero Connector and uBlock Origin Lite. The
connector saves each page like a click on its toolbar button: translator metadata
plus a snapshot, or the PDF file. uBlock Origin Lite removes ads and, with the
right rulesets enabled, cookie banners from the snapshots. Zotero must already be
running; this script never starts it.
"""

import argparse
import copy
import io
import json
import os
import re
import shutil
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path
from typing import Any
from typing import Final

from playwright.sync_api import BrowserContext
from playwright.sync_api import Error as PlaywrightError
from playwright.sync_api import Playwright
from playwright.sync_api import Worker
from playwright.sync_api import sync_playwright

ZOTERO_URL: Final = "http://127.0.0.1:23119"
LIBRARY_API_URL: Final = f"{ZOTERO_URL}/api/users/0"
# The Zotero local API returns at most 100 items per request.
API_PAGE_SIZE: Final = 100

# Chrome Web Store IDs of the extensions, by the name of their cache directory.
EXTENSION_IDS: Final = {
    "zotero-connector": "ekhagklcjbdpajgpjgmbionohlpdbjgc",
    "ublock-origin-lite": "ddkjiahejlhfcafbddmgiahcphecmpfh",
}
EXTENSION_DOWNLOAD_URL: Final = (
    "https://clients2.google.com/service/update2/crx?response=redirect"
    "&prodversion=147.0&acceptformat=crx2,crx3"
    "&x=id%3D{extension_id}%26uc"
)
# Only the Zotero Connector's service worker has this global object.
IS_CONNECTOR_SCRIPT: Final = (
    "typeof Zotero !== 'undefined' && Boolean(Zotero.Connector_Browser)"
)

PAGE_LOAD_TIMEOUT_MS: Final = 60_000
SERVICE_WORKER_TIMEOUT_MS: Final = 30_000
# The connector looks for translators after the page load event.
TRANSLATOR_DETECTION_MS: Final = 3_000
NEW_ITEM_TIMEOUT_SECONDS: Final = 60
# The snapshot or PDF arrives in Zotero after the parent item.
ATTACHMENT_TIMEOUT_SECONDS: Final = 60
POLL_INTERVAL_SECONDS: Final = 1

# The pattern stops at "]", so it also finds the target of an org link.
URL_PATTERN: Final = re.compile(r"https?://[^\s\]>]+")
# A URL at the end of a sentence carries the sentence punctuation.
TRAILING_PUNCTUATION: Final = ".,;:!?"

# Query parameters that only track the reader. "email" holds the email address.
TRACKING_PARAMETERS: Final = frozenset(
    {"CMP", "email", "fbclid", "gclid", "mc_cid", "mc_eid", "ref"}
)
TRACKING_PARAMETER_PREFIX: Final = "utm_"
# Mailchimp puts the subscriber ID into "e". Other sites can use "e" for content.
MAILCHIMP_HOST: Final = "mailchi.mp"
MAILCHIMP_SUBSCRIBER_PARAMETER: Final = "e"

# Chromium content setting value 2 means "block".
CONTENT_SETTING_BLOCK: Final = 2
# Chromium reads these preferences at start. They turn off the translate offer and
# the notification and location prompts. "exit_type" stops the "Restore pages?"
# message after an interrupted run.
BROWSER_PREFERENCES: Final[dict[str, Any]] = {
    "translate": {"enabled": False},
    "profile": {
        "exit_type": "Normal",
        "default_content_setting_values": {
            "notifications": CONTENT_SETTING_BLOCK,
            "geolocation": CONTENT_SETTING_BLOCK,
        },
    },
}

# Runs in the connector's service worker. onZoteroButtonElementClick is the same
# code path as a click on the toolbar button.
SAVE_TAB_SCRIPT: Final = """async (pageUrl) => {
    const tabs = await chrome.tabs.query({});
    const tab = tabs.find((candidate) => candidate.url === pageUrl);
    if (!tab) {
        throw new Error("No browser tab has the URL " + pageUrl);
    }
    await Zotero.Connector_Browser.onZoteroButtonElementClick(tab);
}"""


class ZoteroError(Exception):
    """Zotero is not reachable, or it did not save an item in time."""


def is_tracking_parameter(name: str, host: str | None) -> bool:
    """Tell whether a query parameter only tracks the reader."""
    if name in TRACKING_PARAMETERS:
        return True
    if name.startswith(TRACKING_PARAMETER_PREFIX):
        return True
    return name == MAILCHIMP_SUBSCRIBER_PARAMETER and host == MAILCHIMP_HOST


def strip_tracking(url: str) -> str:
    """Remove the tracking query parameters from the URL."""
    parts = urllib.parse.urlsplit(url)
    parameters = urllib.parse.parse_qsl(parts.query, keep_blank_values=True)
    kept_parameters: list[tuple[str, str]] = []
    for name, value in parameters:
        if is_tracking_parameter(name, parts.hostname):
            continue
        kept_parameters.append((name, value))
    # Do not re-encode a query that has nothing to remove.
    if len(kept_parameters) == len(parameters):
        return url
    query = urllib.parse.urlencode(kept_parameters)
    return urllib.parse.urlunsplit(parts._replace(query=query))


def read_urls(text: str) -> list[str]:
    """Return every http(s) URL in the text once, in file order, without tracking."""
    urls: list[str] = []
    for match in URL_PATTERN.finditer(text):
        url = strip_tracking(match.group(0).rstrip(TRAILING_PUNCTUATION))
        if url not in urls:
            urls.append(url)
    return urls


def normalize_url(url: str) -> str:
    """Make URLs comparable that differ only in tracking or a trailing slash."""
    return strip_tracking(url).rstrip("/")


def check_zotero_running() -> None:
    """Raise ZoteroError if the Zotero desktop app does not answer."""
    ping_url = f"{ZOTERO_URL}/connector/ping"
    try:
        with urllib.request.urlopen(ping_url, timeout=5):  # noqa: S310 - constant local URL
            pass
    except urllib.error.URLError as error:
        message = "Zotero is not running. Start it and run this script again."
        raise ZoteroError(message) from error


def get_items(path: str, query: dict[str, str]) -> list[dict[str, Any]]:
    """Return the items of one request to the Zotero local API."""
    url = f"{LIBRARY_API_URL}/{path}?{urllib.parse.urlencode(query)}"
    try:
        with urllib.request.urlopen(url, timeout=30) as response:  # noqa: S310 - local Zotero URL
            items: list[dict[str, Any]] = json.load(response)
    except urllib.error.HTTPError as error:
        message = (
            f"The Zotero local API returned HTTP {error.code} for {url}. "
            "Enable 'Allow other applications on this computer to communicate "
            "with Zotero' in the Zotero settings under Advanced."
        )
        raise ZoteroError(message) from error
    except urllib.error.URLError as error:
        message = f"Zotero is not reachable at {ZOTERO_URL}: {error.reason}"
        raise ZoteroError(message) from error
    return items


def library_urls() -> set[str]:
    """Return the normalized URLs of all top level items in the Zotero library."""
    urls: set[str] = set()
    start = 0
    while True:
        query = {"limit": str(API_PAGE_SIZE), "start": str(start)}
        items = get_items("items/top", query)
        for item in items:
            url = item["data"].get("url")
            if url:
                urls.add(normalize_url(url))
        if len(items) < API_PAGE_SIZE:
            return urls
        start += API_PAGE_SIZE


def newest_item() -> dict[str, Any] | None:
    """Return the top level item that was added last, or None for an empty library."""
    query = {"sort": "dateAdded", "direction": "desc", "limit": "1"}
    items = get_items("items/top", query)
    if not items:
        return None
    return items[0]


def wait_for_new_item(previous_key: str | None) -> dict[str, Any]:
    """Wait until Zotero has a newest item that differs from previous_key."""
    deadline = time.monotonic() + NEW_ITEM_TIMEOUT_SECONDS
    while time.monotonic() < deadline:
        item = newest_item()
        if item is not None and item["key"] != previous_key:
            return item
        time.sleep(POLL_INTERVAL_SECONDS)
    message = f"Zotero got no new item within {NEW_ITEM_TIMEOUT_SECONDS} s."
    raise ZoteroError(message)


def is_stored_file(item: dict[str, Any]) -> bool:
    """Tell whether the item is an attachment with content, not only a link."""
    data = item["data"]
    return bool(data["itemType"] == "attachment" and data["linkMode"] != "linked_url")


def wait_for_attachment(item: dict[str, Any]) -> bool:
    """Wait until the item has a snapshot or file. Return False on timeout."""
    if is_stored_file(item):
        return True
    deadline = time.monotonic() + ATTACHMENT_TIMEOUT_SECONDS
    while time.monotonic() < deadline:
        children = get_items(f"items/{item['key']}/children", {})
        for child in children:
            if is_stored_file(child):
                return True
        time.sleep(POLL_INTERVAL_SECONDS)
    return False


def install_extension(extension_id: str, directory: Path) -> None:
    """Download an extension from the Chrome Web Store and unpack it."""
    print(f"Downloading the extension {directory.name}.")
    url = EXTENSION_DOWNLOAD_URL.format(extension_id=extension_id)
    with urllib.request.urlopen(url, timeout=60) as response:  # noqa: S310 - constant https URL
        package = response.read()
    shutil.rmtree(directory, ignore_errors=True)
    # A CRX file is a zip archive with an extra header. zipfile skips the header.
    with zipfile.ZipFile(io.BytesIO(package)) as archive:
        archive.extractall(directory)


def is_connector_worker(worker: Worker) -> bool:
    """Tell whether the service worker belongs to the Zotero Connector."""
    return bool(worker.evaluate(IS_CONNECTOR_SCRIPT))


def connector_worker(context: BrowserContext) -> Worker:
    """Return the service worker of the Zotero Connector."""
    for service_worker in context.service_workers:
        if is_connector_worker(service_worker):
            return service_worker
    started_worker: Worker = context.wait_for_event(
        "serviceworker",
        predicate=is_connector_worker,
        timeout=SERVICE_WORKER_TIMEOUT_MS,
    )
    return started_worker


def save_url(context: BrowserContext, url: str) -> None:
    """Open the URL and let the Zotero Connector save it."""
    previous_item = newest_item()
    previous_key = None
    if previous_item is not None:
        previous_key = previous_item["key"]

    page = context.new_page()
    try:
        page.goto(url, wait_until="load", timeout=PAGE_LOAD_TIMEOUT_MS)
        page.wait_for_timeout(TRANSLATOR_DETECTION_MS)
        connector_worker(context).evaluate(SAVE_TAB_SCRIPT, page.url)
        item = wait_for_new_item(previous_key)
        if not wait_for_attachment(item):
            print(f"  Warning: saved without a snapshot or file: {url}")
    finally:
        page.close()


def merge_preferences(current: dict[str, Any], wanted: dict[str, Any]) -> None:
    """Write the wanted values into current and keep all other keys."""
    for key, value in wanted.items():
        current_value = current.get(key)
        if isinstance(value, dict) and isinstance(current_value, dict):
            merge_preferences(current_value, value)
            continue
        current[key] = copy.deepcopy(value)


def write_browser_preferences(profile: Path) -> None:
    """Set BROWSER_PREFERENCES in the profile. Chromium must not be running."""
    preferences_file = profile / "Default" / "Preferences"
    preferences: dict[str, Any] = {}
    if preferences_file.exists():
        preferences = json.loads(preferences_file.read_text(encoding="utf-8"))
    merge_preferences(preferences, BROWSER_PREFERENCES)
    preferences_file.parent.mkdir(parents=True, exist_ok=True)
    preferences_file.write_text(json.dumps(preferences), encoding="utf-8")


def launch_browser(
    playwright: Playwright, extension_directories: list[Path]
) -> BrowserContext:
    """Open Chromium with the persistent profile and the extensions."""
    profile = cache_directory() / "profile"
    write_browser_preferences(profile)
    extension_list = ",".join(str(directory) for directory in extension_directories)
    return playwright.chromium.launch_persistent_context(
        profile,
        headless=False,
        args=[
            f"--disable-extensions-except={extension_list}",
            f"--load-extension={extension_list}",
        ],
    )


def log_in(extension_directories: list[Path]) -> None:
    """Open the browser so the user can log in to websites. Return when it closes."""
    print(
        "Log in to the websites you need and set up uBlock Origin Lite. "
        "Then close the browser window."
    )
    with sync_playwright() as playwright:
        context = launch_browser(playwright, extension_directories)
        # Timeout 0 waits as long as the user needs.
        context.wait_for_event("close", timeout=0)


def save_all(urls: list[str], extension_directories: list[Path]) -> list[str]:
    """Save every URL with the connector. Return the URLs that failed."""
    failed: list[str] = []
    with sync_playwright() as playwright:
        context = launch_browser(playwright, extension_directories)
        try:
            for url in urls:
                print(f"Saving {url}")
                try:
                    save_url(context, url)
                except (PlaywrightError, ZoteroError) as error:
                    print(f"  Failed: {error}", file=sys.stderr)
                    failed.append(url)
        finally:
            context.close()
    return failed


def cache_directory() -> Path:
    """Return the directory for the extensions and the browser profile."""
    xdg_cache_home = os.environ.get("XDG_CACHE_HOME")
    if xdg_cache_home:
        return Path(xdg_cache_home) / "org-to-zotero"
    return Path.home() / ".cache" / "org-to-zotero"


def parse_arguments() -> argparse.Namespace:
    """Parse the command line."""
    parser = argparse.ArgumentParser(
        description=(
            "Save every URL in a file into Zotero, with snapshots and PDFs. "
            "Zotero must be running."
        )
    )
    parser.add_argument(
        "org_file", type=Path, nargs="?", help="file to read the URLs from"
    )
    parser.add_argument(
        "--update-extensions",
        action="store_true",
        help="download the current Zotero Connector and uBlock Origin Lite first",
    )
    parser.add_argument(
        "--login",
        action="store_true",
        help="open the browser to log in to websites, then exit without saving",
    )
    arguments = parser.parse_args()
    if arguments.org_file is None and not arguments.login:
        parser.error("org_file is required unless --login is given")
    return arguments


def prepare_extensions(*, update: bool) -> list[Path]:
    """Return the extension directories. Download missing extensions."""
    extension_directories: list[Path] = []
    for name, extension_id in EXTENSION_IDS.items():
        directory = cache_directory() / name
        has_extension = (directory / "manifest.json").exists()
        if update or not has_extension:
            install_extension(extension_id, directory)
        extension_directories.append(directory)
    return extension_directories


def main() -> int:
    """Save all new URLs of the file and return the exit code."""
    arguments = parse_arguments()
    if arguments.login:
        log_in(prepare_extensions(update=arguments.update_extensions))
        return 0

    org_text = arguments.org_file.read_text(encoding="utf-8")
    urls = read_urls(org_text)

    try:
        check_zotero_running()
        known_urls = library_urls()
    except ZoteroError as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1

    new_urls: list[str] = []
    for url in urls:
        if normalize_url(url) in known_urls:
            print(f"Already in Zotero: {url}")
            continue
        new_urls.append(url)
    if not new_urls:
        print("Nothing to save.")
        return 0

    extension_directories = prepare_extensions(update=arguments.update_extensions)
    failed = save_all(new_urls, extension_directories)
    print(f"Saved {len(new_urls) - len(failed)} of {len(new_urls)} URLs.")
    for url in failed:
        print(f"Failed: {url}", file=sys.stderr)
    if failed:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
