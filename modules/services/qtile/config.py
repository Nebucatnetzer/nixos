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
def add_treetab_section(layout):
    prompt = qtile.widgets_map["section_prompt"]
    prompt.start_input("Section name", layout.cmd_add_section)


keys = [
    # Switch between windows in current stack pane
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "space", lazy.next_layout(), desc="Toggle between layouts"),
    # Move windows around
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), lazy.layout.move_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), lazy.layout.move_up()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "q", lazy.window.kill()),
    Key([mod, "shift"], "space", add_treetab_section),
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
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown qtile"),
    Key([mod, "control"], "r", lazy.restart(), desc="Restart qtile"),
    # Move windows between sections
    Key([mod, "control", "shift"], "j", lazy.layout.section_down()),
    Key([mod, "control", "shift"], "k", lazy.layout.section_up()),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "c", lazy.spawn("i3lock -c 000000")),
    Key([mod], "d", lazy.spawn("rofi -show drun")),
    Key([mod], "e", lazy.spawn("nautilus")),
    Key([mod], "p", lazy.spawn("xrandr --auto")),
    Key([mod], "r", lazy.spawn("rofi -matching-negate-char \\0 -show run")),
    Key([mod], "w", lazy.spawn("firefox")),
    Key([mod], "Tab", lazy.spawn("rofi -show window")),
    # Toggle between different layouts as defined below
    Key([mod], "F1", lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")),
    Key([mod], "F2", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%")),
    Key([mod], "F3", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%")),
    Key([mod], "F5", lazy.spawn("toggle_touchpad.sh")),
    Key([], "XF86AudioPlay", lazy.spawn("playerctl play")),
    Key([], "XF86AudioPause", lazy.spawn("playerctl pause")),
    Key([], "XF86AudioNext", lazy.spawn("playerctl next")),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl previous")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 10%-")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +10%")),
]

group_matches = [
    [
        Match(wm_class="TelegramDesktop"),
        Match(wm_class="Signal"),
        Match(title="WhatsApp"),
        Match(title="Threema"),
    ],  # 0
    None,  # 1
    None,  # 2
    None,  # 3
    None,  # 4
    None,  # 5
    [
        Match(wm_class="KeeWeb"),
        Match(wm_class="JDownloader"),
        Match(wm_class="Plexamp"),
    ],  # 6
    None,  # 7
    [
        Match(wm_class="linphone"),
    ],  # 8
    [
        Match(wm_class="Steam"),
    ],  # 9
]


def toscreen(qtile, group_name):
    if group_name == qtile.current_screen.group.name:
        qtile.current_screen.set_group(qtile.current_screen.previous_group)
        return
    for i, group in enumerate(qtile.groups):
        if group_name == group.name:
            qtile.current_screen.set_group(qtile.groups[i])
            return


groups = [Group(name=i, matches=group_matches[int(i)]) for i in "1234567890"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.function(toscreen, i.name),
                desc="Switch to group {}".format(i.name),
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
                desc="move focused window to group {}".format(i.name),
            ),
        ]
    )

border = dict(border_width=1, border_focus="#000000")

layouts = [
    layout.Columns(**border),
    layout.TreeTab(**border),
]


widget_defaults = dict(
    font="SourceCodePro",
    fontsize=14,
    padding=5,
)
extension_defaults = widget_defaults.copy()


def top_bar_widgets():
    widgets = [
        widget.GroupBox(),
        widget.Sep(padding=5),
        widget.Prompt(name="section_prompt"),
        widget.WindowName(),
        widget.Sep(padding=5),
        widget.DF(fmt="🗄️ {}", visible_on_warn=False),
        widget.Sep(padding=5),
        widget.Volume(emoji=True),
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
        widget.Maildir(maildir_path="~/Maildir/personal", sub_folders=[
            {
                "label":"📬",
                "path": "INBOX",
            }
        ]),
        widget.Sep(padding=5),
        widget.Systray(),
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


screens = [
    Screen(
        top=bar.Bar(
            top_bar_widgets(),
            30,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.Sep(padding=5),
                widget.WindowName(),
                widget.Sep(padding=5),
                widget.CurrentLayout(),
                widget.Sep(padding=5),
                widget.Volume(),
                widget.Sep(padding=5),
                widget.Clock(format="%Y-%m-%d %a %H:%M"),
            ],
            30,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.Sep(padding=5),
                widget.WindowName(),
                widget.Sep(padding=5),
                widget.CurrentLayout(),
                widget.Sep(padding=5),
                widget.Volume(),
                widget.Sep(padding=5),
                widget.Clock(format="%Y-%m-%d %a %H:%M"),
            ],
            30,
        ),
    ),
]

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

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"


@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser("~/.config/qtile/autostart.sh")
    subprocess.run([home])


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

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
