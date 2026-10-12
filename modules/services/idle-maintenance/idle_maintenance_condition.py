"""Decide whether systemd may start an idle maintenance unit.

Exit 0 lets the unit start. Exit 1 makes systemd skip it without a failure.
"""

import argparse
import json
import subprocess
import sys
import time
from pathlib import Path

GRAPHICAL_SESSION_TYPES = {"wayland", "x11"}
EXIT_START = 0
EXIT_SKIP = 1


def seconds_since_last_run(stamp_file: Path) -> float | None:
    if not stamp_file.exists():
        return None
    return time.time() - stamp_file.stat().st_mtime


def ran_recently(stamp_file: Path, minimum_interval_seconds: int) -> bool:
    age_seconds = seconds_since_last_run(stamp_file)
    if age_seconds is None:
        return False
    # A negative age means the clock was wrong at the last run, so ignore the stamp.
    if age_seconds < 0:
        return False
    return age_seconds < minimum_interval_seconds


def user_session_ids() -> list[str]:
    result = subprocess.run(
        ["loginctl", "list-sessions", "--json=short"],
        check=True,
        capture_output=True,
        text=True,
    )
    session_ids: list[str] = []
    for session in json.loads(result.stdout):
        # The class filter skips the greeter, lock-screen and manager sessions.
        if session["class"].startswith("user"):
            session_ids.append(session["session"])
    return session_ids


def session_properties(session_id: str) -> dict[str, str] | None:
    result = subprocess.run(
        [
            "loginctl",
            "show-session",
            session_id,
            "--property=State",
            "--property=Type",
            "--property=LockedHint",
        ],
        capture_output=True,
        text=True,
    )
    # The session can end between list-sessions and show-session.
    if result.returncode != 0:
        return None
    properties: dict[str, str] = {}
    for line in result.stdout.splitlines():
        key, _, value = line.partition("=")
        properties[key] = value
    return properties


def unlocked_graphical_session() -> str | None:
    for session_id in user_session_ids():
        properties = session_properties(session_id)
        if properties is None:
            continue
        # After a logout the session stays in closing while leftover processes run.
        if properties.get("State") == "closing":
            continue
        if properties.get("Type") not in GRAPHICAL_SESSION_TYPES:
            continue
        if properties.get("LockedHint") != "yes":
            return session_id
    return None


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("stamp_file", type=Path)
    parser.add_argument("minimum_interval_seconds", type=int)
    return parser.parse_args()


def main() -> int:
    arguments = parse_arguments()
    if ran_recently(arguments.stamp_file, arguments.minimum_interval_seconds):
        print(
            f"Last run is less than {arguments.minimum_interval_seconds} seconds ago."
        )
        return EXIT_SKIP
    session_id = unlocked_graphical_session()
    if session_id is not None:
        print(f"Graphical session {session_id} is not locked.")
        return EXIT_SKIP
    return EXIT_START


if __name__ == "__main__":
    sys.exit(main())
