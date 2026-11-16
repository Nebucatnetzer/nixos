import argparse
import re
import shutil
import sys
import tempfile
from collections.abc import Callable
from pathlib import Path
from typing import Any

import yt_dlp
from yt_dlp.postprocessor import PostProcessor

BYTES_PER_GIB = 1024**3
PLAYLIST_OUTPUT = "%(playlist)s/%(playlist_index)s - %(title)s [%(id)s].%(ext)s"
# yt-dlp sizes are estimates. Ask for 10 % more free space.
SIZE_MARGIN = 1.1
# Each run gets its own directory here. It is removed when the run ends.
# yt-dlp finishes every video in it, then MoveIntoDestinationPP moves the video out.
TEMP_ROOT = Path.home() / "Downloads" / ".yt-dlp"
# YouTube IDs are 11 characters. yt-dlp puts them in brackets in the filename.
YOUTUBE_ID_PATTERN = re.compile(r"\[([A-Za-z0-9_-]{11})\]")
# yt-dlp's own main() turns these into exit code 1.
YT_DLP_ERRORS = (
    yt_dlp.cookies.CookieLoadError,
    yt_dlp.utils.DownloadError,
    yt_dlp.utils.UnsafeExecExpansionError,
)

# yt-dlp calls a match filter with the video info. A returned string skips the video.
MatchFilter = Callable[..., str | None]


def existing_youtube_ids(video_root: Path) -> list[str]:
    youtube_ids: list[str] = []
    for path in video_root.rglob("*"):
        if not path.is_file():
            continue
        match = YOUTUBE_ID_PATTERN.search(path.name)
        if match is None:
            continue
        youtube_ids.append(match.group(1))
    return youtube_ids


def make_match_filter(
    existing_ids: set[str],
    destination: Path,
    stop_when_full: bool,
    user_filter: MatchFilter | None,
) -> MatchFilter:
    # yt-dlp passes incomplete as a bool or as a set of format fields.
    def match_filter(
        info: dict[str, Any], *, incomplete: bool | set[str]
    ) -> str | None:
        if user_filter is not None:
            user_reason = user_filter(info, incomplete=incomplete)
            if user_reason is not None:
                return user_reason
        if info.get("id") in existing_ids:
            return f"{info['id']} already exists in the video directory"
        # Playlist entries are checked twice: first without formats, then with.
        if incomplete:
            return None
        size = info.get("filesize") or info.get("filesize_approx")
        if size is None:
            print(
                f"Warning: yt-dlp gives no size for {info.get('id')}.", file=sys.stderr
            )
            return None
        message = missing_space_message(destination, int(size * SIZE_MARGIN))
        if message is not None and stop_when_full:
            # yt-dlp prints the message and cancels the remaining downloads.
            raise yt_dlp.utils.RejectedVideoReached(message)
        return message

    return match_filter


def missing_space_message(directory: Path, needed_bytes: int) -> str | None:
    free_bytes = shutil.disk_usage(directory).free
    if free_bytes >= needed_bytes:
        return None
    needed_gib = needed_bytes / BYTES_PER_GIB
    free_gib = free_bytes / BYTES_PER_GIB
    return f"Not enough space in {directory}: needs {needed_gib:.1f} GiB, has {free_gib:.1f} GiB."


def copy_then_rename(source: Path, target: Path) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    # The random hidden name has no YouTube ID, so a leftover never counts as downloaded.
    with tempfile.NamedTemporaryFile(
        dir=target.parent, prefix=".", suffix=".part", delete=False
    ) as partial_file:
        partial_path = Path(partial_file.name)
    try:
        shutil.copy2(source, partial_path)
        partial_path.replace(target)
    except BaseException:
        # Also on Ctrl+C: never leave a partial copy in the destination.
        partial_path.unlink(missing_ok=True)
        raise
    source.unlink()


class MoveIntoDestinationPP(PostProcessor):
    """Move each finished video from the run directory into the destination."""

    def __init__(
        self, run_directory: Path, destination: Path, stop_when_full: bool
    ) -> None:
        super().__init__()
        self._run_directory = run_directory
        self._destination = destination
        self._stop_when_full = stop_when_full

    def run(self, info: dict[str, Any]) -> tuple[list[str], dict[str, Any]]:
        source = Path(info["filepath"])
        target = self._destination / source.relative_to(self._run_directory)
        message = missing_space_message(self._destination, source.stat().st_size)
        if message is not None:
            # Delete it now, so a long run does not fill the run directory.
            source.unlink()
            if self._stop_when_full:
                raise yt_dlp.utils.RejectedVideoReached(message)
            raise yt_dlp.utils.PostProcessingError(message)
        self.to_screen(f'Moving "{source.name}" to {target.parent}')
        copy_then_rename(source, target)
        info["filepath"] = str(target)
        return [], info


def yt_dlp_options(run_directory: Path, mode: str) -> list[str]:
    options = [
        "--cookies-from-browser",
        "firefox",
        "--remux-video=mkv",
        "--sponsorblock-mark=all",
        "--sponsorblock-remove=sponsor,selfpromo,interaction",
        # No separate temp path, so yt-dlp does not move the video itself.
        # MoveIntoDestinationPP moves it from here into the destination.
        f"--paths=home:{run_directory}",
    ]
    if mode == "video":
        options.append("--no-playlist")
    else:
        options.append("--yes-playlist")
    if mode == "playlist":
        options.append(f"--output={PLAYLIST_OUTPUT}")
    return options


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Download videos with yt-dlp. Unknown arguments go to yt-dlp.",
        allow_abbrev=False,
    )
    parser.add_argument("--destination", type=Path, required=True)
    parser.add_argument("--video-root", type=Path, required=True)
    # watch-later skips videos that do not fit, playlist stops at the first one.
    parser.add_argument(
        "--mode", choices=["video", "playlist", "watch-later"], required=True
    )
    arguments, yt_dlp_arguments = parser.parse_known_args()

    arguments.destination.mkdir(parents=True, exist_ok=True)
    TEMP_ROOT.mkdir(parents=True, exist_ok=True)
    existing_ids = set(existing_youtube_ids(arguments.video_root))
    with tempfile.TemporaryDirectory(dir=TEMP_ROOT) as run_directory_name:
        run_directory = Path(run_directory_name)
        stop_when_full = arguments.mode == "playlist"
        options = yt_dlp_options(run_directory, arguments.mode)
        parsed = yt_dlp.parse_options([*options, *yt_dlp_arguments])
        ydl_options = parsed.ydl_opts
        ydl_options["match_filter"] = make_match_filter(
            existing_ids,
            arguments.destination,
            stop_when_full,
            ydl_options.get("match_filter"),
        )
        try:
            with yt_dlp.YoutubeDL(ydl_options) as downloader:
                downloader.add_post_processor(
                    MoveIntoDestinationPP(
                        run_directory, arguments.destination, stop_when_full
                    ),
                    when="after_move",
                )
                return downloader.download(parsed.urls)
        except yt_dlp.utils.DownloadCancelled:
            # Same exit code as yt-dlp's own main().
            return 101
        except YT_DLP_ERRORS:
            # yt-dlp has already printed the error.
            return 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        # The with block in main has already removed the run directory.
        sys.exit(130)
