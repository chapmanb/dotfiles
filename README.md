# Brad's dotfiles

## Permanent architecture

Ubuntu is the **Ubuntu Host Base**: boot, LUKS/LVM, kernel/OEM drivers,
GNOME/GDM/Xorg, native `gnome-terminal` fallback, Docker, OpenSSH, Chrome, Zoom,
and the mandatory **Corporate Endpoint Stack** stay native. The unified flake's
`homeConfigurations."bchapman@bchapman-ubnt"` is the permanent **Declarative
User-Space**: developer tools, language runtimes, Ghostty, Brave, LibreOffice,
Inkscape and Redshift. WezTerm is retired.

`hosts/bchapman-ubnt/` and `nixosConfigurations.bchapman-ubnt` are a **Dormant
NixOS Reference**, not an active dual-boot target. The Cisco FHS browser launcher,
root-certificate notes and mount dependencies remain available for reference.
There are no active LVM cutover or Ubuntu retirement commands.

Keep this checkout at `/home/bchapman/.dotfiles`. All migration-tool Nix
operations use `git+file://` inputs; ignored scratch files must never enter a
world-readable store snapshot. Review and track intended flake changes before
building. With Nix flakes installed, build without activation:

```sh
nix build --no-link --print-out-paths \
  'git+file:///home/bchapman/.dotfiles#homeConfigurations."bchapman@bchapman-ubnt".activationPackage'
```

Home Manager links active Neovim, herdr, shell, Git and retained user configs
back into the checkout using `mkOutOfStoreSymlink`. It owns base runtimes;
`uv`/`pixi` project environments and coding-agent installers remain mutable.
`~/.local/bin` and `~/.bun/bin` stay on PATH, with Nix ahead of `/usr/local/bin`.
For tmux plugins, start tmux and press `ctrl-o + I`.

All five runtime CA variables point to `/etc/ssl/certs/ca-certificates.crt`.
Verify that this host bundle trusts Ginkgo roots and excludes distrusted eMudhra
roots; environment variables do not install certificates. The daily backup
service receives the same variables and an explicit Nix tool PATH.
Initialize and verify the daily backup before enabling its persistent 10pm
Eastern user timer. Inspect it with
`systemctl --user list-timers dotfiles-daily-backup.timer` and
`journalctl --user -u dotfiles-daily-backup.service`.
Passwords, credentials and Cisco assets must stay outside this repository.

## Backup-gated Ubuntu simplification

Run `bin/daily-backup verify` first. The gatekeeper requires a report less than
48 hours old covering both Snapshot Backup and every Browsable Sync root.
Every command defaults to a preview; mutations require **both** `--execute
--confirm`, and `--dry-run` overrides execution. Previews still evaluate/build
Nix and query services/package simulations; they are not network-free.

```sh
bin/migration-gatekeeper preflight-ubuntu
bin/migration-gatekeeper clean-user-space
bin/migration-gatekeeper clean-user-space --execute --confirm
# The following require an unsandboxed host terminal with sudo available:
bin/migration-gatekeeper activate-host-bridge
bin/migration-gatekeeper activate-host-bridge --execute --confirm
bin/migration-gatekeeper simplify-ubuntu-host
bin/migration-gatekeeper simplify-ubuntu-host --execute --confirm
```

`preflight-ubuntu` checks the Git-filtered evaluated source has no `tmp`, builds
the activation package, checks all migrated executables in its `home-path`,
and verifies native Cisco assets, local CA certificates and active corporate
services. Host bridge activation and host simplification rerun this preflight.

`clean-user-space` checks backup freshness and the built Nix replacements,
relocates `gws` and the two `fix_docker_vpn*.sh` scripts to `~/.local/bin` without
overwriting existing launchers, removes retired gcloud shell source lines,
and deletes retired toolchains, Python 3.8 libraries/legacy pip scripts,
WezTerm and Cisco VM scratch artifacts. Active agent/auth launchers, Poetry,
keyring and Python 3.12 are allowlisted. If a destination already exists, the
original is left for host cleanup. In a read-only pibox mount, scripts are
copied into the user directory and original removal is explicitly reported as
deferred to `simplify-ubuntu-host`. Never run the whole gatekeeper under sudo
(Home Manager and home files must retain the user's identity).

`activate-host-bridge` installs the systemd-tmpfiles rule linking
`/run/opengl-driver` to `/home/bchapman/.local/state/nix-opengl-driver`, creates
it immediately and switches standalone Home Manager as the current user.
Home Manager supplies ABI-matching Nixpkgs Mesa, Ghostty GNOME autostart and
Ctrl+Alt+T integration, and Brave browser registration. Native Ubuntu
`/usr/bin/gnome-terminal` remains the fallback; no Nix GNOME Terminal client or
per-binary nixGL wrapper is needed. Back up conflicting unmanaged config files
before switching, and log out/in to validate desktop changes.

`simplify-ubuntu-host` targets an **Upgrade-Clean APT State**. The **Protected
Host Package Guard** rejects protected `Remv`/`Purg` entries in `apt-get -s
purge` and `apt-get -s autoremove --purge` output, including architecture-qualified
names. It rechecks immediately before purging/autoremoving and also guards the
Git downgrade. It removes retired `/usr/local` entries and Linuxbrew, all snaps,
and unused packages; pins snapd at priority -10; retains only the six approved
corporate/Docker/Chrome/OEM APT source files; restores stock Focal Git/git-man
`1:2.25.1-1ubuntu3.14`; and purges residual configurations. A failure stops the
workflow but does not roll back earlier actions: inspect the preview, retain
verified off-host backups and rerun preflight after any interruption. No Ubuntu
release upgrade is performed.

## Cache cleanup and offline checks

`bin/cold-archive prune-caches` previews removal of `.dotfiles/tmp` and agent-plan
`scratch`/`repos` directories; deletion requires `--execute --confirm`.
It leaves uv caches untouched. Use `uv cache prune` separately.

```sh
python3 -B -m unittest discover -s tests
```

These checks validate declarative structure and offline command/filesystem
contracts, not live Nix builds, root operations, VPN login or host activation.
Dotbot's old entrypoint/configuration is retired; its dormant submodule and
`.gitmodules` remain untouched.
