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
