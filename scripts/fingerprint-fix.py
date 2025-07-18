#!/usr/bin/env nix-shell
#! nix-shell -i python3 --pure
#! nix-shell -p gobject-introspection libfprint gusb "python3.withPackages (p: [p.pygobject3])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/f0946fa5f1fb876a9dc2e1850d9d3a4e3f914092.tar.gz

import gi

gi.require_version('FPrint', '2.0')
from gi.repository import FPrint

ctx = FPrint.Context()

for dev in ctx.get_devices():
    print(dev)
    print(dev.get_driver())
    print(dev.props.device_id);

    dev.open_sync()

    dev.clear_storage_sync()
    print("All prints deleted.")

    dev.close_sync()
