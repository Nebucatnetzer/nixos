# pylint: disable=missing-docstring,C0103
import os
import subprocess

from typing import List  # noqa: F401

from libqtile import bar
from libqtile import hook
from libqtile import layout
from libqtile import qtile
from libqtile import widget

from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
terminal = guess_terminal()


@lazy.layout.function
def add_treetab_section(layout_arg):
    prompt = qtile.widgets_map["section_prompt"]
    prompt.start_input("Section name", layout_arg.cmd_add_section)


keys = [
    # Switch between windows in current stack pane
    Key([mod, "mod1"], "j", lazy.layout.down()),
    Key([mod, "mod1"], "k", lazy.layout.up()),
    Key([mod, "mod1"], "h", lazy.layout.left()),
    Key([mod, "mod1"], "l", lazy.layout.right()),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    # Move windows around
    Key(
        [mod, "mod1", "shift"], "j", lazy.layout.shuffle_down(), lazy.layout.move_down()
    ),
    Key([mod, "mod1", "shift"], "k", lazy.layout.shuffle_up(), lazy.layout.move_up()),
    Key([mod, "mod1", "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "mod1", "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "q", lazy.window.kill()),
    Key([mod, "shift"], "Tab", add_treetab_section),
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    # Resize windows
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload config"),
    Key([mod, "control", "shift"], "q", lazy.shutdown(), desc="Shutdown qtile"),
    Key([mod, "control", "shift"], "r", lazy.restart(), desc="Restart qtile"),
    # Move windows between sections
    Key([mod, "control", "shift"], "j", lazy.layout.section_down()),
    Key([mod, "control", "shift"], "k", lazy.layout.section_up()),
    Key(
        [mod],
        "Return",
        lazy.spawn("alacritty -e tmux new -A -s 0"),
        desc="Launch terminal",
    ),
    Key([mod], "c", lazy.spawn("i3lock -c 000000")),
    Key([mod], "space", lazy.spawn("rofi -show drun -show-icons")),
    Key([mod], "e", lazy.spawn("nautilus")),
    Key([mod], "p", lazy.spawn("xrandr --auto")),
    Key([mod], "r", lazy.spawn("rofi -matching-negate-char \\0 -show run")),
    Key(["control", "shift"], "s", lazy.spawn("rofi-search")),
    Key([mod], "w", lazy.spawn("librewolf")),
    Key(["mod1"], "Tab", lazy.spawn("rofi -show window")),
    # Toggle between different layouts as defined below
    Key([], "XF86AudioMute", lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")),
    Key(
        [],
        "XF86AudioLowerVolume",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%"),
    ),
    Key(
        [],
        "XF86AudioRaiseVolume",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%"),
    ),
    Key([mod], "F5", lazy.spawn("toggle_touchpad.sh")),
    Key([], "Print", lazy.spawn("gnome-screenshot --clipboard --area")),
    Key([], "XF86AudioPlay", lazy.spawn("playerctl play")),
    Key([], "XF86AudioPause", lazy.spawn("playerctl pause")),
    Key([], "XF86AudioNext", lazy.spawn("playerctl next")),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl previous")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 10%-")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +10%")),
]

group_matches = [
    None,  # a
    None,  # s
    None,  # d
    None,  # f
    [
        Match(wm_class="JDownloader"),
        Match(wm_class="Plexamp"),
    ],  # g
    None,  # h
    [
        Match(wm_class="linphone"),
    ],  # j
    [
        Match(wm_class="Steam"),
    ],  # k
    [
        Match(wm_class="TelegramDesktop"),
        Match(wm_class="Signal"),
        Match(title="WhatsApp"),
        Match(title="Threema"),
    ],  # l
]


def toscreen(qtile_arg, group_name):
    if group_name == qtile_arg.current_screen.group.name:
        qtile_arg.current_screen.set_group(qtile_arg.current_screen.previous_group)
        return
    for position, group in enumerate(qtile_arg.groups):
        if group_name == group.name:
            qtile_arg.current_screen.set_group(qtile_arg.groups[position])
            return


groups = [Group(name=c, matches=group_matches[i]) for i, c in enumerate("asdfghjkl")]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.function(toscreen, i.name),
                desc=f"Switch to group {i.name}",
            ),
            # mod1 + shift + letter of group = switch to & move focused window to
            # group Key([mod, "shift"], i.name, lazy.window.togroup(i.name,
            # switch_group=True), desc="Switch to & move focused window to group
            # {}".format(i.name)), Or, use below if you prefer not to switch to
            # that group. # mod1 + shift + letter of group = move focused window to
            # group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name),
                desc=f"move focused window to group {i.name}",
            ),
        ]
    )

border = {"border_width": 1, "border_focus": "#000000"}

layouts = [
    layout.Columns(**border),
    layout.TreeTab(**border),
]


widget_defaults = {
    "font": "SourceCodePro",
    "fontsize": 16,
    "padding": 5,
}
extension_defaults = widget_defaults.copy()


def primary_widgets():
    """Widgets for the primary monitor."""
    widgets = [
        widget.GroupBox(
            highlight_method="line",
            highlight_color=["002b36", "268bd2"],
            inactive="657b83",
        ),
        widget.Sep(padding=5),
        widget.Prompt(name="section_prompt"),
        widget.TaskList(
            border="268bd2", font="sans", highlight_method="border", icon_size=20
        ),
        widget.Sep(padding=5),
        widget.DF(fmt="🗄️ {}", visible_on_warn=False),
        widget.Sep(padding=5),
    ]
    widgets_end = [
        widget.Battery(
            charge_char="🔌",
            discharge_char="⚡",
            full_char="🔋",
            show_short_text=False,
        ),
        widget.Sep(padding=5),
        widget.Systray(background="#00000000"),
        widget.Sep(padding=5),
        widget.Clock(format="%Y-%m-%d %a %H:%M"),
    ]
    backlight_widget = [
        widget.Backlight(backlight_name="intel_backlight", fmt="☀️ {}"),
        widget.Sep(padding=5),
    ]
    if os.path.exists("/sys/class/backlight/intel_backlight"):
        widgets.extend(backlight_widget)
    widgets.extend(widgets_end)
    return widgets


def secondary_widgets():
    """Widgets for the secondary monitor."""
    widgets = [
        widget.GroupBox(
            highlight_method="line",
            highlight_color=["002b36", "268bd2"],
            inactive="657b83",
        ),
        widget.Sep(padding=5),
        widget.TaskList(
            border="268bd2", font="sans", highlight_method="border", icon_size=20
        ),
        widget.Sep(padding=5),
        widget.Clock(format="%Y-%m-%d %a %H:%M"),
    ]
    return widgets


def virtual_widgets():
    """Widgets for fake_screens which arent the primary screen."""
    widgets = [
        widget.GroupBox(
            highlight_method="line",
            highlight_color=["002b36", "268bd2"],
            inactive="657b83",
        ),
        widget.Sep(padding=5),
        widget.TaskList(
            border="268bd2", font="sans", highlight_method="border", icon_size=20
        ),
    ]
    return widgets


physical_screens = [
    Screen(
        top=bar.Bar(
            primary_widgets(),
            36,
            background="#00000080",
        ),
    ),
    Screen(
        top=bar.Bar(
            secondary_widgets(),
            36,
            background="#00000080",
        )
    ),
    Screen(
        top=bar.Bar(
            secondary_widgets(),
            36,
            background="#00000080",
        )
    ),
]

fullhd_screens = [
    Screen(
        top=bar.Bar(
            virtual_widgets(),
            36,
            background="#00000080",
        ),
        x=0,
        y=0,
        width=1920,
        height=1080,
    ),
    Screen(
        top=bar.Bar(
            primary_widgets(),
            36,
            background="#00000080",
        ),
        x=1920,
        y=0,
        width=1920,
        height=1080,
    ),
    Screen(
        top=bar.Bar(
            virtual_widgets(),
            36,
            background="#00000080",
        ),
        x=0,
        y=1080,
        width=1920,
        height=1080,
    ),
    Screen(
        top=bar.Bar(
            virtual_widgets(),
            36,
            background="#00000080",
        ),
        x=1920,
        y=1080,
        width=1920,
        height=1080,
    ),
]

screens_list = [physical_screens, fullhd_screens]
# fake_screens = fullhd_screens
screens = physical_screens

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.toggle_floating()),
]

auto_fullscreen = True
bring_front_click = False
cursor_warp = False
dgroups_app_rules = []  # type: List
dgroups_key_binder = None
focus_on_window_activation = "smart"
follow_mouse_focus = True
main = None


@hook.subscribe.suspend
def lock_on_sleep():
    # Run screen locker
    qtile.spawn("i3lock -c 000000")


@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser("~/.config/qtile/autostart.sh")
    subprocess.run([home], check=True)


floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirm"),
        Match(wm_class="dialog"),
        Match(wm_class="download"),
        Match(wm_class="error"),
        Match(wm_class="file_progress"),
        Match(wm_class="notification"),
        Match(wm_class="splash"),
        Match(wm_class="toolbar"),
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="ssh-askpass"),  # ssh-askpass
    ]
)

# Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
