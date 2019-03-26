#!/bin/bash
set -eu -o pipefail

dconf dump /org/gnome/ > ~/drive/backup/gnome-settings.dconf
cp -r ~/.config/autostart ~/drive/backup
cp -r ~/.local/share/gnome-shell/extensions ~/drive/backup
