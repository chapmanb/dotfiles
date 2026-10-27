# Brad's dotfiles

The unified flake provides `homeConfigurations."bchapman@bchapman-ubnt"` for
**User-Space Replication (Stage 1)** on Ubuntu and
`nixosConfigurations.bchapman-ubnt` for **Dual-Boot Cutover (Stage 2)**. The latter
imports `hosts/bchapman-ubnt`, supplied by the separate Stage 2 implementation;
it is not buildable until that host module is present.

## Stage 1

Keep this checkout at `/home/bchapman/.dotfiles`. With Nix and flakes installed,
first build without activating (safe inside the read-only sandbox):

```sh
nix run github:nix-community/home-manager/release-24.11 -- build \
  --flake '/home/bchapman/.dotfiles#bchapman@bchapman-ubnt'
```

Review the build and existing files before switching in a host terminal via the
migration gatekeeper workflow. Existing non-Home-Manager files may conflict with
activation: preserve a backup before replacing them. Do not run activation or
remove Homebrew/NVM from the sandbox. Commit and review the generated `flake.lock`
on the host before relying on repeatable package versions.

Home Manager links active Neovim, WezTerm, herdr, shell, Git, and retained user
configs back into this checkout using `mkOutOfStoreSymlink`: edits apply live,
without another switch. It owns CLI tools and base runtimes; `uv`/`pixi` project
environments and agent installers remain mutable. Agent launchers in
`~/.local/bin` and `~/.bun/bin` remain on PATH. For tmux plugins, start tmux and
press `ctrl-o + I`.

All five runtime CA variables point to `/etc/ssl/certs/ca-certificates.crt`.
Before activation, verify that the host bundle trusts the required Ginkgo roots
and excludes the distrusted eMudhra roots; these variables do not install CAs.
The backup service receives the same trust variables and an explicit Nix tool
PATH (it does not read `.bashrc`).

Initialize and verify the daily backup before activating its daily persistent
user timer. The timer runs `/home/bchapman/.dotfiles/bin/daily-backup run`; its
CLI is supplied by the daily-backup implementation. Check scheduling and logs
with `systemctl --user list-timers dotfiles-daily-backup.timer` and
`journalctl --user -u dotfiles-daily-backup.service`. Passwords, credentials, and
Cisco assets must stay outside this repository.

Dotbot's `install` entrypoint and configuration, along with retired legacy
configs, are removed. The dormant `dotbot` submodule and `.gitmodules` are left
untouched so patches apply cleanly across worktrees.

## NixOS default terminal

Ghostty is installed from the pinned unstable package set. The NixOS Home Manager
configuration launches it at GNOME login, binds **Ctrl+Alt+T**, and sets `TERMINAL`,
GNOME's terminal preference, and the `xdg-terminal-exec` preference to Ghostty.
GNOME Terminal remains installed as a manual fallback (`gnome-terminal`). Apps
with their own terminal setting or a hard-coded launcher may still need their
own configuration; GNOME does not have one universal terminal default.

Desktop integration is imported only by the NixOS output, not standalone Ubuntu
Home Manager. It replaces the known legacy autostart file with a disabled entry
and adds a Ghostty entry, leaving other login applications alone. Before the
first activation, back up the old file **in a host terminal outside pibox**:

```sh
cp -n ~/.config/autostart/gnome-terminal.desktop \
  ~/.config/autostart/gnome-terminal.desktop.before-ghostty
sudo nixos-rebuild switch --flake '/home/bchapman/.dotfiles#bchapman-ubnt'
```

After a successful rebuild, log out and back into GNOME. Check that only Ghostty
opens automatically,
**Ctrl+Alt+T** opens Ghostty, and `gnome-terminal` still works manually. The shared
home directory means NixOS activation changes shared files; reapply standalone
Home Manager if returning to Ubuntu. Reverting the configuration commit and
rebuilding restores the prior declarative configuration; restore the backup
above if you also want the unmanaged GNOME Terminal login entry back.

## Cache cleanup

`bin/cold-archive prune-caches` previews removal of `.dotfiles/tmp` and agent-plan
`scratch`/`repos` directories; deletion requires `--execute --confirm`.
It leaves uv caches untouched. Use `uv cache prune` separately to reclaim unused
uv cache entries without deleting the entire cache.

## Offline checks

```sh
python3 -B -m unittest tests.test_flake_and_home
```

These checks validate declarative structure and the imported configuration
snapshot, not Nix evaluation or host activation. Run the Nix build preflight on
a Nix-enabled host before switching.
