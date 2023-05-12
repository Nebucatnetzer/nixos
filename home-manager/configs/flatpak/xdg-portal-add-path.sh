#!/usr/bin/env bash
systemctl --user import-environment PATH
systemctl --user restart xdg-desktop-portal.service
