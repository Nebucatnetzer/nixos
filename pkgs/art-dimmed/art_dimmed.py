import os
import signal
import subprocess
import sys

import dbus

DIMMED_BRIGHTNESS_PERCENT = 30

BRIGHTNESS_SERVICE = "org.kde.ScreenBrightness"
BRIGHTNESS_PATH = "/org/kde/ScreenBrightness"
BRIGHTNESS_INTERFACE = "org.kde.ScreenBrightness"
DISPLAY_INTERFACE = "org.kde.ScreenBrightness.Display"
# SetBrightness flag 0x1 hides the brightness on-screen indicator.
SUPPRESS_INDICATOR_FLAG = 0x1

NIGHT_LIGHT_SERVICE = "org.kde.KWin"
NIGHT_LIGHT_PATH = "/org/kde/KWin/NightLight"
NIGHT_LIGHT_INTERFACE = "org.kde.KWin.NightLight"


def get_display_paths(bus: dbus.SessionBus) -> list[str]:
    brightness_object = bus.get_object(BRIGHTNESS_SERVICE, BRIGHTNESS_PATH)
    display_names = brightness_object.Get(
        BRIGHTNESS_INTERFACE,
        "DisplaysDBusNames",
        dbus_interface=dbus.PROPERTIES_IFACE,
    )
    display_paths: list[str] = []
    for display_name in display_names:
        display_paths.append(f"{BRIGHTNESS_PATH}/{display_name}")
    return display_paths


def dim_displays(bus: dbus.SessionBus, display_paths: list[str]) -> dict[str, int]:
    original_brightness: dict[str, int] = {}
    for display_path in display_paths:
        display_object = bus.get_object(BRIGHTNESS_SERVICE, display_path)
        properties = dbus.Interface(display_object, dbus.PROPERTIES_IFACE)
        brightness = int(properties.Get(DISPLAY_INTERFACE, "Brightness"))
        max_brightness = int(properties.Get(DISPLAY_INTERFACE, "MaxBrightness"))
        original_brightness[display_path] = brightness

        dimmed_brightness = max_brightness * DIMMED_BRIGHTNESS_PERCENT // 100
        display = dbus.Interface(display_object, DISPLAY_INTERFACE)
        display.SetBrightness(
            dbus.Int32(dimmed_brightness),
            dbus.UInt32(SUPPRESS_INDICATOR_FLAG),
        )
    return original_brightness


def restore_displays(bus: dbus.SessionBus, original_brightness: dict[str, int]) -> None:
    for display_path, brightness in original_brightness.items():
        # A display can disappear while ART runs, for example an unplugged monitor.
        try:
            display_object = bus.get_object(BRIGHTNESS_SERVICE, display_path)
            display = dbus.Interface(display_object, DISPLAY_INTERFACE)
            display.SetBrightness(
                dbus.Int32(brightness),
                dbus.UInt32(SUPPRESS_INDICATOR_FLAG),
            )
        except dbus.DBusException as error:
            print(
                f"art-dimmed: cannot restore {display_path}: {error}", file=sys.stderr
            )


def inhibit_night_light(bus: dbus.SessionBus) -> int:
    night_light_object = bus.get_object(NIGHT_LIGHT_SERVICE, NIGHT_LIGHT_PATH)
    night_light = dbus.Interface(night_light_object, NIGHT_LIGHT_INTERFACE)
    return int(night_light.inhibit())


def uninhibit_night_light(bus: dbus.SessionBus, cookie: int) -> None:
    # KWin drops the inhibit anyway when this process disconnects, so a failure is harmless.
    try:
        night_light_object = bus.get_object(NIGHT_LIGHT_SERVICE, NIGHT_LIGHT_PATH)
        night_light = dbus.Interface(night_light_object, NIGHT_LIGHT_INTERFACE)
        night_light.uninhibit(dbus.UInt32(cookie))
    except dbus.DBusException as error:
        print(f"art-dimmed: cannot uninhibit night light: {error}", file=sys.stderr)


def run_art(art_binary: str, arguments: list[str]) -> int:
    art_process = subprocess.Popen([art_binary, *arguments])

    def forward_sigterm(signal_number: int, frame: object) -> None:
        art_process.terminate()

    signal.signal(signal.SIGTERM, forward_sigterm)
    # Ctrl+C in a terminal also reaches ART, so the launcher waits for ART to exit.
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    return art_process.wait()


def main() -> int:
    art_binary = os.environ["ART_REAL_BINARY"]
    bus = dbus.SessionBus()
    original_brightness: dict[str, int] = {}
    night_light_cookie: int | None = None

    try:
        original_brightness = dim_displays(bus, get_display_paths(bus))
    except dbus.DBusException as error:
        print(f"art-dimmed: cannot dim displays: {error}", file=sys.stderr)

    try:
        night_light_cookie = inhibit_night_light(bus)
    except dbus.DBusException as error:
        print(f"art-dimmed: cannot inhibit night light: {error}", file=sys.stderr)

    try:
        return run_art(art_binary, sys.argv[1:])
    finally:
        restore_displays(bus, original_brightness)
        if night_light_cookie is not None:
            uninhibit_night_light(bus, night_light_cookie)


if __name__ == "__main__":
    sys.exit(main())
