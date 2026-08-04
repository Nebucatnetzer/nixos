"""Download new videos from a list of RSS feeds.

Works like bash-podder: a TOML file lists the feeds, a download archive records the
videos already fetched, and every run only downloads what is new. The feeds are parsed
here and yt-dlp is handed the entry URLs, so nothing but the entries a feed actually
lists is ever visited.

A feed is a ``[[feed]]`` table with a ``url``, an optional ``directory`` to sort its
videos into a subdirectory of the download directory, and an optional ``description``
that says what the URL means and is otherwise ignored. The download directory itself is
the top level ``output`` of the same file, or ``--output``, which wins over it.

Most channels are ones where only some of their videos are worth watching, so a video is
offered for review before it is downloaded. Its thumbnail is written into the inbox, a
directory named by ``inbox_directory`` or ``--inbox-directory``, and only a thumbnail
that has been moved into the ``wanted`` directory of the inbox is downloaded, into the
directory its feed asked for. A thumbnail that is deleted instead counts as declined and
is never offered again, and one that is left alone is forgotten once the video is older
than --days. Doing nothing therefore downloads nothing.

The thumbnail is the one the feed itself names, so no thumbnail has to be looked up. A
feed that names none gets a text file with the title and the URL in its place, so a video
is picked the same way whatever its feed offers.

A feed whose videos are all worth having sets ``inbox = false`` and is downloaded right
away, and so are all feeds when the top level ``inbox`` is false.

The decisions live in a JSON state file, ``--state``, because a feed keeps listing a
declined video for as long as it is among its newest entries. A record of that file is
dropped once its video is older than --days, which is also why raising --days later can
offer a video again that once expired unreviewed.

Shorts are left alone. A feed links most of them as one, ``/shorts/<id>`` instead of
``/watch?v=<id>``, and that settles it for free. The rest are found by asking yt-dlp,
because a video that is taller than it is wide is almost always a short as well, and a
short's thumbnail is a letterboxed landscape image like any other, so it could not be told
apart in the inbox. Either way it is recorded as skipped and never offered or asked about
again.

A stream that is still running is left out as well, and so is one that has not started or
has only just ended. A feed lists all three like any other video, and recording one takes
as long as the broadcast lasts. Nothing is recorded about them, so once the stream is over
and YouTube has cut the recording, a later run offers it like any other video.

That question costs one request per video it is asked about, which is what reviewing adds
beyond the thumbnail, and the shorts a feed links as such are never asked about at all.
The first run of a long feed list is the expensive one; --days keeps it in hand, and a
rate limit only ends the run, it loses nothing that was already offered.

A run stops as soon as a server answers that we are asking too often. The feeds, the
thumbnails and yt-dlp are all watched for it, and the exit status then says so, because
retrying right away only extends the limit.

Atom (``<entry>``) and RSS 2.0 (``<item>``) feeds are both understood. A YouTube channel
feed, for example, lives at
``https://www.youtube.com/feeds/videos.xml?channel_id=<CHANNEL_ID>``
and lists
the 15 most recent videos, which is also the limit of how far back a single
run can look.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tomllib
import unicodedata
from collections.abc import Iterable
from datetime import UTC
from datetime import datetime
from datetime import timedelta
from email.utils import parsedate_to_datetime
from hashlib import sha256
from pathlib import Path
from pathlib import PurePosixPath
from typing import IO
from typing import NamedTuple
from urllib.error import HTTPError
from urllib.parse import parse_qs
from urllib.parse import urlparse
from urllib.request import Request
from urllib.request import urlopen
from xml.etree.ElementTree import Element
from xml.etree.ElementTree import ParseError
from xml.etree.ElementTree import fromstring

DEFAULT_DAYS = 30
FETCH_TIMEOUT_SECONDS = 30
FETCH_SCHEMES = ("http", "https")
# '#S' is yt-dlp's restricted sanitization: the field is transliterated to ASCII and
# its spaces become underscores. Dashes separate the fields, underscores the words.
OUTPUT_TEMPLATE = "%(upload_date)s-%(uploader)#S-%(title)#S-%(id)s.%(ext)s"
# yt-dlp match filter that keeps everything at least as wide as it is tall. It is
# checked against the metadata, so a rejected video is never downloaded. The '?' keeps
# the videos whose aspect ratio yt-dlp cannot determine: a stray short costs less than
# a silently dropped video.
LANDSCAPE_FILTER = "aspect_ratio>=?1"
# A feed lists a live broadcast as an ordinary entry, and yt-dlp would then record it in
# real time until the streamer stops, holding up every video queued behind it. An
# upcoming one, a premiere or a scheduled stream, has nothing to download yet and only
# makes yt-dlp fail. 'post_live' is the just ended stream whose VOD YouTube is still
# cutting, which downloads incomplete. Nothing is recorded about any of them, neither in
# the archive nor in the state file, so the finished VOD is picked up by a later run while
# it is still within --days.
UNFINISHED_STATUSES = ("is_live", "is_upcoming", "post_live")
FINISHED_FILTER = " & ".join(
    f"live_status!={status}" for status in UNFINISHED_STATUSES
)
# What yt-dlp is asked about a video before it is offered for review, and how many fields
# an answer has. A feed says neither what a video's aspect ratio is nor whether it is a
# stream that has finished, and both decide whether it is worth reviewing at all.
PROBE_TEMPLATE = "%(original_url)s\t%(live_status)s\t%(aspect_ratio)s"
PROBE_FIELDS = 3
# A feed links a short as 'https://www.youtube.com/shorts/<id>', which says what it is
# before anyone has to be asked about it. It is also the better answer than the shape of
# the video: what makes a short a short is that YouTube files it as one.
SHORTS_PATH = "/shorts/"
USER_AGENT = "tube-podder"
ATOM = "{http://www.w3.org/2005/Atom}"
# Media RSS, which is where a YouTube feed keeps the thumbnail of an entry, and YouTube's
# own namespace, which is where it keeps the video ID.
MEDIA = "{http://search.yahoo.com/mrss/}"
YOUTUBE = "{http://www.youtube.com/xml/schemas/2015}"
HTTP_TOO_MANY_REQUESTS = 429
# yt-dlp reports a rate limit in its output instead of in its exit status, so its output
# is read along the way and searched for these.
RATE_LIMIT_MARKERS = ("HTTP Error 429", "Too Many Requests")
RATE_LIMIT_MESSAGE = "yt-dlp was told that we are asking too often"
OUTPUT_CHUNK_BYTES = 4096
# Kept from the previous chunk so a marker split between two reads is still recognized.
MARKER_OVERLAP = max(len(marker) for marker in RATE_LIMIT_MARKERS) - 1
# Own exit status so a timer can tell "come back later" apart from a real failure.
EXIT_RATE_LIMITED = 2
FEED_TABLE = "feed"
FEED_KEYS = frozenset({"url", "directory", "description", "inbox"})
OUTPUT_KEY = "output"
INBOX_KEY = "inbox"
INBOX_DIRECTORY_KEY = "inbox_directory"
TOP_LEVEL_KEYS = frozenset({OUTPUT_KEY, INBOX_KEY, INBOX_DIRECTORY_KEY, FEED_TABLE})
# Reviewing is what a channel one only watches now and then needs, so it is what a feed
# gets unless it says otherwise.
DEFAULT_INBOX = True
# The inbox below the download directory, used when the config file names none.
INBOX_DIRECTORY_NAME = "inbox"
# The directory inside the inbox a thumbnail is moved into to ask for the download. One
# destination for the whole inbox: which directory the video belongs in is remembered in
# the state file, so it does not have to be remembered by where its thumbnail lies.
WANTED_DIRECTORY = "wanted"
# The feed list of the versions before the config file became TOML.
LEGACY_CONFIG_NAME = "feeds.txt"

STATE_VERSION = 1
STATE_VERSION_KEY = "version"
STATE_VIDEOS_KEY = "videos"
ENTRY_KEYS = frozenset({"title", "published", "directory", "thumbnail", "decision"})
DECISION_PENDING = "pending"
DECISION_DECLINED = "declined"
DECISION_DOWNLOADED = "downloaded"
# Never offered in the first place, because it is a short. Recorded like a decision so it
# is not asked about again either.
DECISION_SKIPPED = "skipped"
DECISIONS = frozenset(
    {DECISION_PENDING, DECISION_DECLINED, DECISION_DOWNLOADED, DECISION_SKIPPED}
)

# Everything that is neither a letter, a digit nor one of these becomes an underscore in
# an inbox file name, which leaves a name every file manager and shell is happy with.
UNSAFE_CHARACTERS = re.compile(r"[^A-Za-z0-9._-]+")
# A suffix taken from a thumbnail URL is only used when it looks like one.
THUMBNAIL_SUFFIX = re.compile(r"^\.[A-Za-z0-9]{1,5}$")
DEFAULT_THUMBNAIL_SUFFIX = ".jpg"
# What a video without a thumbnail is offered as: a file to read instead of one to look
# at, but one that is moved and deleted just the same.
PLACEHOLDER_SUFFIX = ".txt"
UPLOADER_LIMIT = 40
TITLE_LIMIT = 80
# A YouTube video ID is 11 characters long, the limit leaves room for the longer ones of
# other sites, and the hash is what stands in for an ID that cannot be found at all.
ID_LIMIT = 24
ID_HASH_LENGTH = 11

EXAMPLE_CONFIG = """\
# Everything below is commented out. The download directory can be named once,
# above the feeds, and --output overrides it:
#
# output = "~/Videos/tube-podder"
#
# By default a video is not downloaded right away but offered for review: its
# thumbnail is written into the inbox, and only a thumbnail moved into the
# 'wanted' directory of the inbox is downloaded. A deleted one is declined and
# never offered again, one left alone is forgotten once the video is older than
# --days. The inbox is the 'inbox' directory of the download directory unless it
# is named here, and --inbox-directory overrides it:
#
# inbox_directory = "~/Videos/tube-podder/inbox"
#
# Reviewing can be turned off for every feed at once, which downloads whatever
# the feeds list, as the versions before the inbox did:
#
# inbox = true
#
# One [[feed]] table per feed:
#
#   url          the feed to read (required)
#   directory    where to put its videos, as a path relative to the download
#                directory (optional, they land directly in it when left out)
#   inbox        false for a feed whose videos are all worth having: they are
#                downloaded without being offered for review (optional,
#                defaults to the 'inbox' above)
#   description  a note about what the URL means, for you rather than for
#                tube-podder, which only reads it to check its spelling
#
# Every entry a feed lists is looked at, so the feed decides what can be
# downloaded. A YouTube channel feed looks like this:
#
# [[feed]]
# url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q"
# directory = "music"
# inbox = false
# description = "Synthwave mixes, every one of them worth having"
#
# [[feed]]
# url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q"
# description = "Talks, reviewed in the inbox and then straight into the download directory"
#
# The channel ID of a channel that only has a handle (@name) is in the page
# source of its YouTube page.
"""


class Feed(NamedTuple):
    """A feed of the config file and what is to be done with its videos."""

    url: str
    # Relative to the download directory, empty for the download directory itself.
    directory: str
    # Whether the videos are offered for review instead of downloaded right away. The
    # top level default is already folded in, see read_config().
    inbox: bool


class Config(NamedTuple):
    """What the config file has to say."""

    # None when the file names no download directory, which leaves it to --output.
    output: Path | None
    # None when the file names no inbox, which leaves it to --inbox-directory and to the
    # inbox of the download directory.
    inbox_directory: Path | None
    feeds: list[Feed]


class Video(NamedTuple):
    """A single entry of a feed."""

    url: str
    title: str
    published: datetime
    # A feed document says nothing about where its videos belong or how they are to be
    # treated, so both are only filled in once the video is tied back to its feed, see
    # collect_videos().
    directory: str = ""
    inbox: bool = False
    # What the entry says beyond the above, as far as it says anything: the channel, the
    # ID of the video and the thumbnail to offer it with.
    uploader: str = ""
    video_id: str = ""
    thumbnail: str = ""


class Entry(NamedTuple):
    """What the state file remembers about one video that was offered for review."""

    title: str
    published: datetime
    directory: str
    # The file name inside the inbox, without a directory: where it lies is what says
    # whether the video was picked, so the path is built when it is looked for.
    thumbnail: str
    decision: str


class Probe(NamedTuple):
    """What yt-dlp says about a video a feed wants to have reviewed.

    Both fields are yt-dlp's output as it printed it, 'NA' included: what they mean is
    decided in is_short() and is_unfinished().
    """

    live_status: str
    aspect_ratio: str


class Offering(NamedTuple):
    """What became of the videos a run set out to offer for review."""

    entries: dict[str, Entry]
    offered: int
    # Shorts, recorded as skipped and done with.
    skipped: int
    # Streams that have not finished, left for a later run to offer.
    unfinished: int
    # Videos yt-dlp said nothing about, also left for a later run.
    unjudged: int


class Review(NamedTuple):
    """What the inbox says about the videos recorded in the state file.

    Every field holds URLs, so a decision can be looked up in the state file it came
    from. A video is in exactly one of them, and only the videos that are still pending
    are in any of the first three.
    """

    picked: list[str]
    declined: list[str]
    waiting: list[str]
    # Too old to be offered any longer, whatever was once decided about it.
    expired: list[str]


class Selection(NamedTuple):
    """What a run found to do, each list newest first."""

    # From the feeds that skip the inbox: downloaded without being asked about.
    direct: list[Video]
    # Picked in the inbox, so due to be downloaded now.
    picked: list[Video]
    # New videos of the feeds that use the inbox, to be offered for review.
    offered: list[Video]
    # Already offered and not decided about yet.
    waiting: list[Video]


class ConfigError(Exception):
    """Raised when the config file cannot be read as a list of feeds."""


class StateError(Exception):
    """Raised when the state file cannot be read as a list of decisions."""


class RateLimited(Exception):
    """Raised when a server tells us that we are asking too often.

    Deliberately not an OSError: it has to travel past the handlers that shrug off a
    single unreadable feed and end the whole run instead.
    """


def warn(message: str) -> None:
    """Print *message* on stderr so it stays out of the listing on stdout."""
    print(message, file=sys.stderr)


def _xdg_directory(variable: str, fallback: str) -> Path:
    """Return the XDG base directory in *variable*, or *fallback* in $HOME."""
    configured = os.environ.get(variable)
    if configured:
        return Path(configured)
    return Path.home() / fallback


def default_config() -> Path:
    """Return the default path of the feed list."""
    return _xdg_directory("XDG_CONFIG_HOME", ".config") / "tube-podder" / "feeds.toml"


def default_output() -> Path:
    """Return the download directory used when neither the config nor --output names one."""
    return Path.home() / "Videos" / "tube-podder"


def default_archive() -> Path:
    """Return the default path of the download archive."""
    return (
        _xdg_directory("XDG_STATE_HOME", ".local/state") / "tube-podder" / "archive.txt"
    )


def default_state() -> Path:
    """Return the default path of the file recording the inbox decisions."""
    return (
        _xdg_directory("XDG_STATE_HOME", ".local/state") / "tube-podder" / "inbox.json"
    )


def write_example_config(config: Path) -> None:
    """Create *config* with a commented example feed list."""
    config.parent.mkdir(parents=True, exist_ok=True)
    config.write_text(EXAMPLE_CONFIG)


def _stays_inside(directory: str) -> bool:
    """Say whether *directory* stays inside the directory it is resolved against."""
    path = PurePosixPath(directory)
    return not path.is_absolute() and ".." not in path.parts


def _feed_directory(directory: str, position: int) -> str:
    """Return *directory* normalized, refusing anything that leaves the download directory."""
    if not _stays_inside(directory):
        message = (
            f"the 'directory' of feed {position} is '{directory}', but it has to be a "
            "relative path without '..', it is created inside the download directory"
        )
        raise ConfigError(message)

    path = PurePosixPath(directory)
    if path.parts and path.parts[0] == WANTED_DIRECTORY:
        message = (
            f"the 'directory' of feed {position} starts with '{WANTED_DIRECTORY}', "
            "which is the name the inbox keeps for the videos picked for download"
        )
        raise ConfigError(message)

    # PurePosixPath('') is '.', which as a directory name would be the literal dot.
    return str(path) if directory else ""


def _flag(value: object, description: str) -> bool:
    """Return the boolean *value* of the setting *description* names."""
    if not isinstance(value, bool):
        message = f"{description} has to be true or false"
        raise ConfigError(message)
    return value


def _feed(table: object, position: int, inbox: bool) -> Feed:
    """Return the feed described by one ``[[feed]]`` *table*.

    *inbox* is the top level default the table can override, and *position* is only there
    for the error messages, so they can point at the entry to fix in a file where every
    entry looks the same.
    """
    if not isinstance(table, dict):
        message = f"feed {position} is not a table, write it as [[{FEED_TABLE}]]"
        raise ConfigError(message)

    unknown = sorted(set(table) - FEED_KEYS)
    if unknown:
        message = f"feed {position} has unknown key(s): {', '.join(unknown)}"
        raise ConfigError(message)

    url = table.get("url")
    if not isinstance(url, str) or not url.strip():
        message = f"feed {position} has no 'url'"
        raise ConfigError(message)

    directory = table.get("directory", "")
    if not isinstance(directory, str):
        message = f"the 'directory' of feed {position} has to be a string"
        raise ConfigError(message)

    return Feed(
        url.strip(),
        _feed_directory(directory.strip(), position),
        _flag(table.get(INBOX_KEY, inbox), f"the '{INBOX_KEY}' of feed {position}"),
    )


def _config_directory(value: object, key: str) -> Path:
    """Return the directory the top level *key* names.

    A relative path would leave open what it is relative to, so only an absolute one and
    a '~' are accepted.
    """
    if not isinstance(value, str) or not value.strip():
        message = f"'{key}' has to be the path of a directory"
        raise ConfigError(message)

    directory = Path(value.strip()).expanduser()
    if not directory.is_absolute():
        message = (
            f"'{key}' is '{value}', but it has to be an absolute path or start with '~'"
        )
        raise ConfigError(message)
    return directory


def read_config(config: Path) -> Config:
    """Return the directories and the feeds of the TOML *config*.

    A typo is reported instead of quietly costing us a feed, so an unknown key or a
    misspelled ``[[feed]]`` ends the run. The 'description' of a feed is read for that
    check alone: what it says is for whoever opens the file.
    """
    try:
        with config.open("rb") as handle:
            document = tomllib.load(handle)
    except tomllib.TOMLDecodeError as error:
        message = f"not valid TOML: {error}"
        raise ConfigError(message) from error

    unexpected = sorted(set(document) - TOP_LEVEL_KEYS)
    if unexpected:
        message = (
            f"unknown top level key(s): {', '.join(unexpected)}. A feed is written as "
            f"[[{FEED_TABLE}]], the download directory as {OUTPUT_KEY} = \"...\""
        )
        raise ConfigError(message)

    tables = document.get(FEED_TABLE, [])
    if not isinstance(tables, list):
        message = f"'{FEED_TABLE}' has to be a list of tables, written as [[{FEED_TABLE}]]"
        raise ConfigError(message)

    inbox = _flag(document.get(INBOX_KEY, DEFAULT_INBOX), f"'{INBOX_KEY}'")
    output = document.get(OUTPUT_KEY)
    inbox_directory = document.get(INBOX_DIRECTORY_KEY)
    return Config(
        _config_directory(output, OUTPUT_KEY) if output is not None else None,
        (
            _config_directory(inbox_directory, INBOX_DIRECTORY_KEY)
            if inbox_directory is not None
            else None
        ),
        [
            _feed(table, position, inbox)
            for position, table in enumerate(tables, start=1)
        ],
    )


def _entry_text(record: dict, key: str, url: str) -> str:
    """Return the string the *key* of the state file's *record* of *url* holds."""
    text = record.get(key)
    if not isinstance(text, str):
        message = f"the '{key}' of the record of {url} has to be a string"
        raise StateError(message)
    return text


def _state_entry(url: str, record: object) -> Entry:
    """Return what the state file's *record* of *url* remembers.

    Everything is checked, including that the file name and the directory are ones inside
    the inbox: they are turned into paths whose files are deleted, and a state file that
    was edited by hand is no reason to delete anything else.
    """
    if not isinstance(record, dict):
        message = f"the record of {url} is not an object"
        raise StateError(message)

    unknown = sorted(set(record) - ENTRY_KEYS)
    if unknown:
        message = f"the record of {url} has unknown key(s): {', '.join(unknown)}"
        raise StateError(message)

    decision = _entry_text(record, "decision", url)
    if decision not in DECISIONS:
        message = (
            f"the 'decision' of the record of {url} is '{decision}', but it has to be "
            f"one of: {', '.join(sorted(DECISIONS))}"
        )
        raise StateError(message)

    thumbnail = _entry_text(record, "thumbnail", url)
    if not thumbnail or PurePosixPath(thumbnail).name != thumbnail:
        message = (
            f"the 'thumbnail' of the record of {url} is '{thumbnail}', but it has to be "
            "the name of a file in the inbox"
        )
        raise StateError(message)

    directory = _entry_text(record, "directory", url)
    if not _stays_inside(directory):
        message = (
            f"the 'directory' of the record of {url} is '{directory}', but it has to be "
            "a relative path without '..'"
        )
        raise StateError(message)

    published = _entry_text(record, "published", url)
    try:
        timestamp = datetime.fromisoformat(published)
    except ValueError as error:
        message = f"the 'published' of the record of {url} is unreadable: {error}"
        raise StateError(message) from error

    return Entry(
        _entry_text(record, "title", url),
        _as_utc(timestamp),
        directory,
        thumbnail,
        decision,
    )


def read_state(state: Path) -> dict[str, Entry]:
    """Return the decisions recorded in *state*, keyed by video URL.

    An empty state is only assumed when there is no file yet. A file that cannot be read
    ends the run instead, because carrying on without it would offer every declined video
    again and download whatever is still lying in the inbox.
    """
    if not state.is_file():
        return {}

    try:
        with state.open("rb") as handle:
            document = json.load(handle)
    except json.JSONDecodeError as error:
        message = f"not valid JSON: {error}"
        raise StateError(message) from error

    if not isinstance(document, dict):
        message = "not an object"
        raise StateError(message)

    version = document.get(STATE_VERSION_KEY)
    if version != STATE_VERSION:
        message = (
            f"'{STATE_VERSION_KEY}' is {version!r}, but this tube-podder writes "
            f"{STATE_VERSION}. It was written by another version."
        )
        raise StateError(message)

    videos = document.get(STATE_VIDEOS_KEY, {})
    if not isinstance(videos, dict):
        message = f"'{STATE_VIDEOS_KEY}' has to be an object"
        raise StateError(message)

    return {url: _state_entry(url, record) for url, record in videos.items()}


def write_state(state: Path, entries: dict[str, Entry]) -> None:
    """Record *entries* in *state*.

    The file is written next to itself and then moved into place, so a run that is cut
    short cannot leave half a state file behind and lose every decision with it.
    """
    document = {
        STATE_VERSION_KEY: STATE_VERSION,
        STATE_VIDEOS_KEY: {
            url: {
                "title": entry.title,
                "published": entry.published.isoformat(),
                "directory": entry.directory,
                "thumbnail": entry.thumbnail,
                "decision": entry.decision,
            }
            for url, entry in sorted(entries.items())
        },
    }

    state.parent.mkdir(parents=True, exist_ok=True)
    temporary = state.with_name(state.name + ".new")
    temporary.write_text(json.dumps(document, indent=2, ensure_ascii=False) + "\n")
    temporary.replace(state)


def _rate_limit_message(url: str, error: HTTPError) -> str:
    """Describe the rate limit *error* of *url*, including Retry-After when it is given."""
    retry_after = (error.headers or {}).get("Retry-After")
    if retry_after:
        return f"{url} answered 429 Too Many Requests, retry after {retry_after}"
    return f"{url} answered 429 Too Many Requests"


def fetch(url: str) -> bytes:
    """Return the raw body of *url*, a feed document or a thumbnail.

    Only the schemes in ``FETCH_SCHEMES`` are fetched, so a stray ``file:`` line in the
    feed list cannot turn into a local file read. A 429 becomes a RateLimited instead of
    a plain error, so it ends the run rather than counting as one broken feed.
    """
    if urlparse(url).scheme not in FETCH_SCHEMES:
        message = f"not an {' or '.join(FETCH_SCHEMES)} URL"
        raise ValueError(message)

    # S310: the scheme check above leaves only http(s), so neither call can be
    # talked into reading a local file or into a custom scheme.
    request = Request(url, headers={"User-Agent": USER_AGENT})  # noqa: S310
    try:
        with urlopen(request, timeout=FETCH_TIMEOUT_SECONDS) as response:  # noqa: S310
            payload: bytes = response.read()
    except HTTPError as error:
        if error.code == HTTP_TOO_MANY_REQUESTS:
            raise RateLimited(_rate_limit_message(url, error)) from error
        raise
    return payload


def _as_utc(published: datetime) -> datetime:
    """Return *published* as an aware datetime, assuming UTC when it has no zone."""
    if published.tzinfo is None:
        return published.replace(tzinfo=UTC)
    return published


def _thumbnail_url(entry: Element) -> str:
    """Return the thumbnail a feed entry names, empty when it names none.

    The element is looked for anywhere below the entry: a YouTube feed keeps it inside a
    ``<media:group>``, other feeds put it right into the entry.
    """
    for thumbnail in entry.iter(f"{MEDIA}thumbnail"):
        url = thumbnail.get("url")
        if url:
            return url.strip()
    return ""


def _atom_entry(entry: Element) -> Video | None:
    """Return the video of an Atom ``<entry>``, or None when it cannot be read."""
    url = None
    for link in entry.iter(f"{ATOM}link"):
        if link.get("rel", "alternate") == "alternate":
            url = link.get("href")
            break

    published = entry.findtext(f"{ATOM}published") or entry.findtext(f"{ATOM}updated")
    if not url or not published:
        return None

    try:
        timestamp = datetime.fromisoformat(published)
    except ValueError as error:
        warn(f"Ignoring <entry> with an unreadable date: {error}")
        return None

    title = entry.findtext(f"{ATOM}title") or url
    return Video(
        url.strip(),
        title.strip(),
        _as_utc(timestamp),
        uploader=(entry.findtext(f"{ATOM}author/{ATOM}name") or "").strip(),
        video_id=(entry.findtext(f"{YOUTUBE}videoId") or "").strip(),
        thumbnail=_thumbnail_url(entry),
    )


def _rss_item(item: Element) -> Video | None:
    """Return the video of an RSS 2.0 ``<item>``, or None when it cannot be read."""
    url = item.findtext("link")
    if not url:
        enclosure = item.find("enclosure")
        url = enclosure.get("url") if enclosure is not None else None

    published = item.findtext("pubDate")
    if not url or not published:
        return None

    try:
        timestamp = parsedate_to_datetime(published)
    except ValueError as error:
        warn(f"Ignoring <item> with an unreadable date: {error}")
        return None

    title = item.findtext("title") or url
    return Video(
        url.strip(),
        title.strip(),
        _as_utc(timestamp),
        thumbnail=_thumbnail_url(item),
    )


def parse_feed(payload: bytes) -> list[Video]:
    """Return the videos listed in the Atom or RSS 2.0 feed *payload*.

    Entries without a URL or with an unreadable date are skipped, so one broken entry
    does not cost us the rest of the feed.
    """
    # S314: the payload comes from a feed the user subscribed to, and ElementTree
    # resolves no external entities and retrieves no DTDs on Python 3.12.
    feed = fromstring(payload)  # noqa: S314
    videos: list[Video] = []

    for entry in feed.iter(f"{ATOM}entry"):
        video = _atom_entry(entry)
        if video is not None:
            videos.append(video)

    for item in feed.iter("item"):
        video = _rss_item(item)
        if video is not None:
            videos.append(video)

    return videos


def _field(text: str, limit: int) -> str:
    """Return *text* as one field of a file name, empty when nothing is left of it.

    This is what yt-dlp's '#S' does for the video files: the text is transliterated to
    ASCII, whatever a file name is better off without becomes an underscore, and the
    result is cut to *limit* characters so a long title cannot use up the whole name.
    """
    ascii_text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode()
    field = UNSAFE_CHARACTERS.sub("_", ascii_text).strip("._-")
    return field[:limit].strip("._-")


def _video_id(video: Video) -> str:
    """Return the ID that tells *video* apart from every other one in the inbox.

    The feed's own ID is used when it gives one. Otherwise the 'v' of the URL, then its
    last path segment, and a hash of the whole URL when even that is empty: the ID only
    has to be unique, it is not looked up anywhere.
    """
    if video.video_id:
        return _field(video.video_id, ID_LIMIT)

    parsed = urlparse(video.url)
    identifier = parse_qs(parsed.query).get("v", [""])[0]
    if not identifier:
        identifier = PurePosixPath(parsed.path).name

    field = _field(identifier, ID_LIMIT)
    if field:
        return field
    return sha256(video.url.encode()).hexdigest()[:ID_HASH_LENGTH]


def _suffix(thumbnail: str) -> str:
    """Return the file suffix an inbox file for *thumbnail* gets."""
    if not thumbnail:
        return PLACEHOLDER_SUFFIX

    suffix = PurePosixPath(urlparse(thumbnail).path).suffix
    if THUMBNAIL_SUFFIX.match(suffix):
        return suffix
    return DEFAULT_THUMBNAIL_SUFFIX


def inbox_name(video: Video) -> str:
    """Return the file name *video* is offered under in the inbox.

    It is built like OUTPUT_TEMPLATE, so an inbox file reads like the video file it may
    turn into: date, channel, title, ID.
    """
    fields = [f"{video.published:%Y-%m-%d}"]
    fields += [
        field
        for field in (_field(video.uploader, UPLOADER_LIMIT), _field(video.title, TITLE_LIMIT))
        if field
    ]
    fields.append(_video_id(video))
    return "-".join(fields) + _suffix(video.thumbnail)


def placeholder(video: Video) -> str:
    """Return what a *video* whose feed names no thumbnail is offered with instead."""
    return (
        f"{video.title}\n"
        f"{video.published:%Y-%m-%d %H:%M %Z}\n"
        f"{video.url}\n"
        "\n"
        f"Its feed names no thumbnail. Move this file into '{WANTED_DIRECTORY}' to "
        "download the video, delete it to decline it.\n"
    )


def _forward_output(stream: IO[bytes]) -> bool:
    """Copy *stream* to stdout and report whether it mentioned a rate limit.

    The chunks are forwarded as they arrive rather than line by line: yt-dlp ends a
    progress update with a carriage return, and waiting for a newline would hold the
    whole progress of a download back until it is finished.
    """
    descriptor = stream.fileno()
    seen = ""
    limited = False
    while not limited:
        chunk = os.read(descriptor, OUTPUT_CHUNK_BYTES)
        if not chunk:
            break
        text = chunk.decode("utf-8", errors="replace")
        sys.stdout.write(text)
        sys.stdout.flush()
        seen = seen[-MARKER_OVERLAP:] + text
        limited = any(marker in seen for marker in RATE_LIMIT_MARKERS)
    return limited


def run_yt_dlp(command: list[str]) -> bool:
    """Run *command* and return True when it succeeded.

    Raises RateLimited once yt-dlp reports a rate limit, and stops yt-dlp at that point:
    every further request would only extend the limit.
    """
    with subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    ) as process:
        limited = _forward_output(process.stdout)
        if limited:
            process.terminate()

    if limited:
        raise RateLimited(RATE_LIMIT_MESSAGE)
    if process.returncode != 0:
        warn(f"At least one download failed, yt-dlp exited with {process.returncode}.")
        return False
    return True


def download(
    videos: list[Video],
    archive: Path,
    output: Path,
    browser: str | None,
    skip_vertical: bool,
) -> bool:
    """Download *videos* with yt-dlp and return True when yt-dlp succeeded.

    The archive is yt-dlp's own download log: videos recorded in it are reported as
    known and skipped instead of downloaded again. A vertical video rejected by
    ``LANDSCAPE_FILTER`` is not recorded, so it is looked at again on the next run and
    picked up should it ever turn out not to be a short after all. Raises RateLimited
    when yt-dlp runs into a rate limit.
    """
    # yt-dlp ORs the filters of several --match-filters, so a video kept by any one of
    # them is downloaded. The filters that all have to hold go into a single one.
    filters = [FINISHED_FILTER]
    if skip_vertical:
        filters.append(LANDSCAPE_FILTER)

    command = [
        "yt-dlp",
        "--no-playlist",
        "--download-archive",
        str(archive),
        "--remux-video=mkv",
        "--paths",
        str(output),
        "--output",
        OUTPUT_TEMPLATE,
        "--match-filters",
        " & ".join(filters),
    ]
    if browser is not None:
        command += ["--cookies-from-browser", browser]
    command += [video.url for video in videos]

    return run_yt_dlp(command)


def read_probes(command: list[str], urls: set[str]) -> dict[str, Probe]:
    """Run *command* and return the answers it printed about *urls*.

    yt-dlp mixes its own messages into the same output, so a line only counts as an answer
    when it has the shape of one and names a video we asked about; everything else is
    forwarded, because a video that could not be looked at is worth seeing. Raises
    RateLimited as soon as the output says we are asking too often.
    """
    probes: dict[str, Probe] = {}
    limited = False

    with subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        errors="replace",
    ) as process:
        for line in process.stdout:
            fields = line.rstrip("\n").split("\t")
            if len(fields) == PROBE_FIELDS and fields[0] in urls:
                probes[fields[0]] = Probe(fields[1], fields[2])
                continue

            sys.stdout.write(line)
            if any(marker in line for marker in RATE_LIMIT_MARKERS):
                limited = True
                process.terminate()
                break

    if limited:
        raise RateLimited(RATE_LIMIT_MESSAGE)
    return probes


def probe_videos(videos: list[Video], browser: str | None) -> dict[str, Probe]:
    """Ask yt-dlp about *videos*, keyed by the URL each answer is about.

    Nothing is downloaded and, without a --download-archive, nothing is recorded either: a
    video looked at here has to stay a video that can still be downloaded later. yt-dlp
    carries on past a video it cannot read, and one it says nothing about is simply missing
    from the result.
    """
    command = [
        "yt-dlp",
        "--no-playlist",
        "--simulate",
        "--ignore-errors",
        "--print",
        PROBE_TEMPLATE,
    ]
    if browser is not None:
        command += ["--cookies-from-browser", browser]
    command += [video.url for video in videos]

    return read_probes(command, {video.url for video in videos})


def is_shorts_link(url: str) -> bool:
    """Say whether *url* is the link a feed gives a short."""
    return urlparse(url).path.startswith(SHORTS_PATH)


def is_short(probe: Probe) -> bool:
    """Say whether the video is taller than it is wide, which on YouTube means a short."""
    try:
        ratio = float(probe.aspect_ratio)
    except ValueError:
        # 'NA': yt-dlp cannot tell, and a stray short in the inbox costs less than a video
        # that is never offered at all. The '?' of LANDSCAPE_FILTER says the same.
        return False
    return ratio < 1


def is_unfinished(probe: Probe) -> bool:
    """Say whether the video is a stream that cannot be had in one piece yet."""
    return probe.live_status in UNFINISHED_STATUSES


def collect_videos(feeds: list[Feed], cutoff: datetime) -> tuple[list[Video], list[str]]:
    """Return the videos of all *feeds* published after *cutoff*, newest first.

    Every video carries the directory of the feed that listed it and whether that feed
    wants it reviewed. A video listed by several feeds is only returned once, by the first
    feed that lists it, and that is the feed both of those come from. The second element
    of the result lists the URLs of the feeds that could not be read.
    """
    pending: dict[str, Video] = {}
    failed: list[str] = []

    for feed in feeds:
        try:
            videos = parse_feed(fetch(feed.url))
        except (OSError, ParseError, ValueError) as error:
            warn(f"Failed to read {feed.url}: {error}")
            failed.append(feed.url)
            continue

        recent = [video for video in videos if video.published >= cutoff]
        print(f"{len(recent)} of {len(videos)} entries are recent: {feed.url}")
        for video in recent:
            pending.setdefault(
                video.url,
                video._replace(directory=feed.directory, inbox=feed.inbox),
            )

    return newest_first(pending.values()), failed


def newest_first(videos: Iterable[Video]) -> list[Video]:
    """Return *videos* sorted by their publishing date, the newest one first."""
    return sorted(videos, key=lambda video: video.published, reverse=True)


def group_by_directory(videos: list[Video]) -> dict[str, list[Video]]:
    """Return *videos* grouped by the directory their feed asked for, newest first."""
    groups: dict[str, list[Video]] = {}
    for video in videos:
        groups.setdefault(video.directory, []).append(video)
    return groups


def inbox_path(inbox: Path, entry: Entry, picked: bool) -> Path:
    """Return where *entry* lies in the *inbox*, once picked and once not yet."""
    directory = WANTED_DIRECTORY if picked else entry.directory
    return inbox / directory / entry.thumbnail


def review_inbox(entries: dict[str, Entry], inbox: Path, cutoff: datetime) -> Review:
    """Return what the inbox says about *entries*, without touching a single file.

    Deciding and acting on the decision are kept apart so that --list can say what a run
    would do without doing any of it.

    A video is picked when its thumbnail was moved into the 'wanted' directory, still
    waiting while the thumbnail lies where it was written, and declined once the
    thumbnail is in neither place, which is what deleting it leaves behind. A video older
    than *cutoff* has expired: the feeds no longer offer it, so there is nothing left to
    decide and its record can go, whatever it says.
    """
    review = Review([], [], [], [])
    for url, entry in entries.items():
        if entry.published < cutoff:
            review.expired.append(url)
        elif entry.decision != DECISION_PENDING:
            continue
        elif inbox_path(inbox, entry, picked=True).exists():
            review.picked.append(url)
        elif inbox_path(inbox, entry, picked=False).exists():
            review.waiting.append(url)
        else:
            review.declined.append(url)
    return review


def entry_videos(entries: dict[str, Entry], urls: list[str]) -> list[Video]:
    """Return the *entries* of *urls* as videos, newest first."""
    return newest_first(
        Video(url, entries[url].title, entries[url].published, entries[url].directory)
        for url in urls
    )


def select(
    feeds: list[Feed],
    entries: dict[str, Entry],
    review: Review,
    cutoff: datetime,
) -> tuple[Selection, list[str]]:
    """Return what the *feeds* and the inbox add up to, and the feeds that failed.

    A video the state file already knows is never offered again: it is either waiting in
    the inbox, or it was decided about and that decision stands until it expires.
    """
    pending, failed = collect_videos(feeds, cutoff)
    selection = Selection(
        direct=[video for video in pending if not video.inbox],
        picked=entry_videos(entries, review.picked),
        offered=[
            video for video in pending if video.inbox and video.url not in entries
        ],
        waiting=entry_videos(entries, review.waiting),
    )
    return selection, failed


def _remove(path: Path) -> None:
    """Delete *path* if it is there, warning instead of failing the run when it stays."""
    try:
        path.unlink(missing_ok=True)
    except OSError as error:
        warn(f"Could not remove '{path}': {error}")


def _forget(entry: Entry, inbox: Path) -> None:
    """Remove whatever *entry* still has lying in the *inbox*.

    Both places are cleared: a thumbnail that was copied rather than moved leaves one
    behind where it was written, and nothing will ever look at it again.
    """
    _remove(inbox_path(inbox, entry, picked=True))
    _remove(inbox_path(inbox, entry, picked=False))


def apply_review(
    entries: dict[str, Entry],
    review: Review,
    inbox: Path,
) -> dict[str, Entry]:
    """Return *entries* with the declined videos recorded and the expired ones dropped.

    A picked video is only recorded once it has really been downloaded, see
    download_run(): until then its thumbnail in 'wanted' is what remembers the decision,
    and a download that failed has to be tried again.
    """
    updated = dict(entries)
    for url in review.declined:
        updated[url] = updated[url]._replace(decision=DECISION_DECLINED)
    for url in review.expired:
        _forget(updated.pop(url), inbox)
    return updated


def offer(video: Video, inbox: Path) -> str | None:
    """Write the preview of *video* into the *inbox* and return its file name.

    None is returned when the thumbnail could not be fetched: the video is then left out
    of the state file, so a later run offers it again. A feed that names no thumbnail at
    all is a different matter, and gets a text file that is moved and deleted just like a
    thumbnail. Raises RateLimited when the thumbnail is refused for asking too often.
    """
    name = inbox_name(video)
    directory = inbox / video.directory
    directory.mkdir(parents=True, exist_ok=True)

    if not video.thumbnail:
        (directory / name).write_text(placeholder(video))
        return name

    try:
        payload = fetch(video.thumbnail)
    except (OSError, ValueError) as error:
        warn(f"Failed to fetch the thumbnail of {video.url}: {error}")
        return None

    (directory / name).write_bytes(payload)
    return name


def _entry(video: Video, thumbnail: str, decision: str) -> Entry:
    """Return what the state file remembers about *video*."""
    return Entry(video.title, video.published, video.directory, thumbnail, decision)


def offer_videos(
    videos: list[Video],
    entries: dict[str, Entry],
    inbox: Path,
    browser: str | None,
    skip_vertical: bool,
) -> Offering:
    """Offer *videos* for review, as far as they are worth reviewing at all.

    A short is not: it is recorded as skipped without a thumbnail ever being written, so
    it is neither offered nor asked about again. A feed that links one as a short says so
    itself, and that one is settled before yt-dlp is asked anything, which is what most
    shorts cost: nothing.

    What is left goes to probe_videos(), because a feed says neither how a video is shaped
    nor whether it is a stream that has finished. A stream that has not is left out without
    being recorded, so a later run offers the finished recording.

    The 'wanted' directory is created along the way, because a video can only be picked
    once there is somewhere to move its thumbnail to.
    """
    updated = dict(entries)
    if not videos:
        return Offering(updated, 0, 0, 0, 0)

    (inbox / WANTED_DIRECTORY).mkdir(parents=True, exist_ok=True)

    skipped = 0
    candidates = []
    for video in videos:
        if skip_vertical and is_shorts_link(video.url):
            updated[video.url] = _entry(video, inbox_name(video), DECISION_SKIPPED)
            skipped += 1
            continue
        candidates.append(video)

    probes = probe_videos(candidates, browser) if candidates else {}

    offered = 0
    unfinished = 0
    for video in candidates:
        probe = probes.get(video.url)
        if probe is None:
            continue
        if is_unfinished(probe):
            unfinished += 1
            continue
        if skip_vertical and is_short(probe):
            updated[video.url] = _entry(video, inbox_name(video), DECISION_SKIPPED)
            skipped += 1
            continue

        name = offer(video, inbox)
        if name is None:
            continue
        updated[video.url] = _entry(video, name, DECISION_PENDING)
        offered += 1

    return Offering(updated, offered, skipped, unfinished, len(candidates) - len(probes))


def record_downloads(
    entries: dict[str, Entry],
    urls: list[str],
    inbox: Path,
) -> dict[str, Entry]:
    """Return *entries* with the videos of *urls* recorded as downloaded.

    Their files leave the inbox with them: the decision is remembered in the state file
    from here on, and the download archive keeps yt-dlp from fetching them again.
    """
    updated = dict(entries)
    for url in urls:
        _forget(updated[url], inbox)
        updated[url] = updated[url]._replace(decision=DECISION_DOWNLOADED)
    return updated


def list_group(videos: list[Video], heading: str) -> None:
    """Print *videos* under *heading*, in the directory they would be downloaded into."""
    if not videos:
        return

    print(f"\n{heading}")
    for directory, grouped in sorted(group_by_directory(videos).items()):
        print(f"  {directory or '.'}/")
        for video in grouped:
            print(f"    {video.published:%Y-%m-%d} {video.title} <{video.url}>")


def list_selection(selection: Selection, review: Review, inbox: Path) -> None:
    """Print what a run would download, offer, leave waiting and record."""
    list_group(selection.picked, "Picked in the inbox, downloaded by the next run:")
    list_group(selection.direct, "From a feed that skips the inbox, downloaded by the next run:")
    list_group(
        selection.offered,
        "Offered for review by the next run, shorts and unfinished streams excepted:",
    )
    list_group(selection.waiting, f"Waiting for review in '{inbox}':")

    print(
        f"\n{len(selection.picked) + len(selection.direct)} video(s) to download, "
        f"{len(selection.offered)} to offer, {len(selection.waiting)} waiting, "
        f"{len(review.declined)} declined, {len(review.expired)} expired."
    )


def report_offering(offering: Offering, inbox: Path) -> None:
    """Say what became of the videos this run set out to offer."""
    if offering.offered:
        print(f"\n{offering.offered} video(s) offered for review in '{inbox}'.")
    if offering.skipped:
        print(f"{offering.skipped} short(s) skipped, they are not offered.")
    if offering.unfinished:
        print(f"{offering.unfinished} unfinished stream(s) left for a later run.")
    if offering.unjudged:
        warn(f"{offering.unjudged} video(s) could not be looked at, a later run offers them.")


def report_review(review: Review, inbox: Path) -> None:
    """Say what the inbox contributed to this run."""
    if review.picked:
        print(f"\n{len(review.picked)} video(s) picked in '{inbox}'.")
    if review.declined:
        print(f"{len(review.declined)} video(s) declined, they are not offered again.")
    if review.expired:
        print(f"{len(review.expired)} video(s) expired, they are no longer offered.")
    if review.waiting:
        print(f"{len(review.waiting)} video(s) still waiting for review.")


def cookie_browser(args: argparse.Namespace) -> str | None:
    """Return the browser to read cookies from, or None when cookies are disabled."""
    browser: str = args.cookies_from_browser
    if args.no_cookies or browser == "none":
        return None
    return browser


def download_pending(
    args: argparse.Namespace,
    pending: list[Video],
    download_directory: Path,
) -> list[str]:
    """Create the target directories, hand *pending* to yt-dlp and return what failed.

    yt-dlp downloads into one directory per run, so the videos are handed over in one
    run per directory below *download_directory*, and the result names the directories
    whose run did not succeed. A rate limit raises out of here and leaves the remaining
    directories for the next run.
    """
    print(f"\nPassing {len(pending)} video(s) to yt-dlp.")
    args.archive.parent.mkdir(parents=True, exist_ok=True)

    failed = []
    for directory, videos in sorted(group_by_directory(pending).items()):
        output = download_directory / directory
        print(f"\n{len(videos)} video(s) into '{output}'.")
        output.mkdir(parents=True, exist_ok=True)
        if not download(
            videos,
            args.archive,
            output,
            cookie_browser(args),
            skip_vertical=not args.include_vertical,
        ):
            failed.append(directory)
    return failed


def download_run(
    args: argparse.Namespace,
    selection: Selection,
    entries: dict[str, Entry],
    review: Review,
    directories: tuple[Path, Path],
) -> bool:
    """Record the review, offer the new videos, download what is due.

    *directories* is the download directory and the inbox. Returns True when every yt-dlp
    run succeeded.
    """
    download_directory, inbox = directories

    entries = apply_review(entries, review, inbox)
    offering = offer_videos(
        selection.offered,
        entries,
        inbox,
        cookie_browser(args),
        skip_vertical=not args.include_vertical,
    )
    entries = offering.entries
    report_offering(offering, inbox)

    # Written before the first download: a rate limit ends the run in the middle of the
    # downloads, and the thumbnails just written would otherwise be offered a second time
    # while their files are already lying in the inbox.
    write_state(args.state, entries)

    pending = selection.picked + selection.direct
    if not pending:
        print("\nNothing to download.")
        return True

    failed = download_pending(args, pending, download_directory)
    downloaded = [url for url in review.picked if entries[url].directory not in failed]
    write_state(args.state, record_downloads(entries, downloaded, inbox))
    return not failed


def arguments() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        prog="tube-podder",  # writePython3Bin would show ".tube-podder-wrapped"
        description=(
            "Offer the videos published in the last few days by the RSS feeds "
            "listed in the config file for review, and download the ones whose "
            "thumbnail was moved into the 'wanted' directory of the inbox. Feeds "
            "with 'inbox = false' are downloaded without being offered. Videos "
            "already recorded in the download archive are skipped, as are vertical "
            "videos, which are almost always shorts, and streams that have not "
            "finished, which are looked at again by a later run."
        ),
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=default_config(),
        help="TOML file listing the RSS feeds (default: %(default)s)",
    )
    parser.add_argument(
        "--archive",
        type=Path,
        default=default_archive(),
        help="File recording the already downloaded videos (default: %(default)s)",
    )
    parser.add_argument(
        "--state",
        type=Path,
        default=default_state(),
        help="File recording the decisions made in the inbox (default: %(default)s)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help=(
            "Directory to download into, overriding the 'output' of the config file "
            f"(default: {default_output()})"
        ),
    )
    parser.add_argument(
        "--inbox-directory",
        type=Path,
        help=(
            "Directory to offer the videos for review in, overriding the "
            f"'{INBOX_DIRECTORY_KEY}' of the config file (default: the "
            f"'{INBOX_DIRECTORY_NAME}' directory of the download directory)"
        ),
    )
    parser.add_argument(
        "--days",
        type=int,
        default=DEFAULT_DAYS,
        help="Only look at videos published in the last N days (default: %(default)s)",
    )
    cookies = parser.add_mutually_exclusive_group()
    cookies.add_argument(
        "--cookies-from-browser",
        default="firefox",
        help="Browser to read cookies from, or 'none' (default: %(default)s)",
    )
    cookies.add_argument(
        "--no-cookies",
        action="store_true",
        help="Send no cookies at all, same as --cookies-from-browser none",
    )
    parser.add_argument(
        "--include-vertical",
        action="store_true",
        help="Offer and download vertical videos too instead of treating them as shorts",
    )
    parser.add_argument(
        "--list",
        action="store_true",
        help="Only say what a run would do, without writing or downloading anything",
    )
    return parser.parse_args()


def announce_example_config(config: Path) -> None:
    """Tell the user about the freshly created *config*, and about a feed list to port."""
    print(f"Created an example feed list at '{config}'.")
    print("Add one [[feed]] table per feed, then run tube-podder again.")

    legacy = config.with_name(LEGACY_CONFIG_NAME)
    if legacy.is_file():
        print(f"\nThe feed list is TOML now, so '{legacy}' is no longer read.")
        print("Its URLs go into the new file as the 'url' of a [[feed]] table.")


def run(args: argparse.Namespace) -> int:
    """List or download the videos the feeds and the inbox offer, and return the status."""
    if not args.config.is_file():
        write_example_config(args.config)
        announce_example_config(args.config)
        return 0

    config = read_config(args.config)
    if not config.feeds:
        print(f"No feeds listed in '{args.config}'. Nothing to do.")
        return 0

    # --output beats the config file, which beats the default, and the inbox follows the
    # download directory as long as neither names one.
    download_directory = args.output or config.output or default_output()
    inbox = args.inbox_directory or config.inbox_directory
    if inbox is None:
        inbox = download_directory / INBOX_DIRECTORY_NAME

    cutoff = datetime.now(UTC) - timedelta(days=args.days)
    entries = read_state(args.state)
    review = review_inbox(entries, inbox, cutoff)
    selection, failed = select(config.feeds, entries, review, cutoff)

    downloaded = True
    if args.list:
        list_selection(selection, review, inbox)
    else:
        report_review(review, inbox)
        downloaded = download_run(
            args,
            selection,
            entries,
            review,
            (download_directory, inbox),
        )

    if failed:
        warn(f"{len(failed)} feed(s) failed:")
        for feed in failed:
            warn(f"  {feed}")

    if failed or not downloaded:
        return 1
    return 0


def main() -> None:
    """Entry point."""
    args = arguments()

    if args.days < 1:
        warn("Error: --days must be at least 1.")
        sys.exit(1)

    try:
        status = run(args)
    except ConfigError as error:
        warn(f"Error in '{args.config}': {error}")
        sys.exit(1)
    except StateError as error:
        warn(f"Error in '{args.state}': {error}")
        sys.exit(1)
    except RateLimited as error:
        warn(f"Rate limited: {error}")
        warn("Stopping here. The videos left over are picked up by the next run.")
        sys.exit(EXIT_RATE_LIMITED)
    except (OSError, UnicodeDecodeError) as error:
        warn(f"Error: {error}")
        sys.exit(1)

    sys.exit(status)


if __name__ == "__main__":
    main()
