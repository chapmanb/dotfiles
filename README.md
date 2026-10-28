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

For an explicitly accepted DriveStrike health issue, add
`--allow-inactive-drivestrike` to each preflight/host invocation. This permits only
its observed `inactive`, `failed`, `activating`, or `deactivating` state; missing
units and command errors still fail. Results report `VERIFIED_WITH_EXCEPTIONS`
and `corporate_service_exceptions` with the observed state, not full endpoint
health. The option does not persist or disable/uninstall DriveStrike: its package
and APT source remain protected. All other service, asset, backup, confirmation,
and package-removal checks remain enforced. Restic verifies backups; it does not
replace endpoint security.

`clean-user-space` checks backup freshness and the built Nix replacements,
relocates `gws` and the two `fix_docker_vpn*.sh` scripts to `~/.local/bin` without
overwriting existing launchers, removes retired gcloud shell source lines,
and deletes retired toolchains, Python 3.8 libraries/legacy pip scripts,
WezTerm and Cisco VM scratch artifacts. Active agent/auth launchers, Poetry,
keyring and Python 3.12 are allowlisted. If a destination already exists, the
original is left for host cleanup. In a read-only pibox mount, scripts are
copied into the user directory and original removal is explicitly reported as
deferred to `simplify-ubuntu-host`. If pibox also mounts `~/.local/bin` read-only,
run user cleanup in an unsandboxed host terminal too. When the normal backup
state is read-only, use `daily-backup --state-dir <writable-directory> verify`
and pass its real `verification.json` via `--backup-report` to the gatekeeper.
Never run the whole gatekeeper under sudo (Home Manager and home files must
retain the user's identity).

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
`1:2.25.1-1ubuntu3.14`; and purges unprotected residual configurations. Already
uninstalled protected packages (including old kernel images) retain their
configuration and are listed in `preserved_protected_residuals`; the guard still
refuses any protected removal APT proposes. This deliberately leaves those `rc`
records rather than claiming a completely residual-free host. A failure stops the
workflow but does not roll back earlier actions: inspect the preview, retain
verified off-host backups and rerun preflight after any interruption. No Ubuntu
release upgrade is performed.

## Native Bash prompt

Interactive Bash uses a small built-in prompt: `[:~/path] $`, with a green colon,
bold path/mark and a purple nonzero exit status before the mark. SSH sessions
include the hostname; root includes its username and uses `#`. Bash's `\w`
provides HOME abbreviation without interpreting directory names as shell code.
History is flushed before each prompt, and existing prompt hooks are retained.
There are no VCS/system widgets or width-based path shortening.

LiquidPrompt and its submodule/config are retired. The prompt leaves Bash's
normal prompt expansion enabled so Ghostty's preexec hook can run. Open a **new
tab/shell** to switch: sourcing the new bashrc into a shell that already loaded
LiquidPrompt leaves its old functions, hooks and disabled expansion in memory.
The checkout-linked bashrc applies immediately to new shells; a normal Home
Manager activation outside pibox removes the obsolete managed config link.
Retired files still bind-mounted by an old sandbox need host-side removal, not
an attempt to unlink the read-only mount from inside pibox. To roll back, revert
the native-prompt commit, then run `git submodule update --init shell/liquidprompt`,
reactivate Home Manager and start a fresh shell.

## Ghostty font

Home Manager installs `pkgs.jetbrains-mono`, enables profile font discovery
through `fonts.fontconfig.enable`, and manages `~/.config/ghostty/config.ghostty`
with JetBrains Mono at 11 points, visually matched to the previous GNOME Terminal
prompt. Equal point sizes across different font families did not look equal.
GNOME's font settings and global font aliases are not changed. This uses the
regular font package, not a Nerd Font variant or an ad-hoc download.

Activate from **GNOME Terminal outside pibox**, preserving an existing unmanaged
config with a backup suffix:

```sh
home-manager switch -b before-ghostty-font --flake 'git+file:///home/bchapman/.dotfiles#bchapman@bchapman-ubnt'
```

Keep any existing backup with that suffix; choose another suffix if it already
exists. In Ghostty, press **Ctrl+Shift+,** to reload config (or use Reload Config
in the command palette). If a tab had manually adjusted zoom, press **Ctrl+0**
to reset to the configured size. Compare with GNOME Terminal on the same display;
the different font and rendering engines may have different apparent sizes.
If a running instance has cached the old font list, retry in a fresh Ghostty
process when work can safely be closed; do not terminate working agents. To roll
back, revert the relevant font-setting commit and reactivate; restore the saved
unmanaged config if desired.

## Ghostty application launcher on Ubuntu

Home Manager overrides `com.mitchellh.ghostty.desktop` in the generic-Linux
profile with a high-priority entry. It sets `DBusActivatable=false` and uses the
absolute patched Ghostty executable for launch, `TryExec` and New Window. This
avoids GNOME requesting a systemd activation unit that is outside the user unit
search path and uses settings unsupported by Ubuntu's systemd 245. The existing
icon, single-instance behavior and terminal-launch metadata are retained. This
override does not install a Ghostty systemd service or change autostart or
shortcut configuration.
The dormant NixOS configuration retains the upstream desktop entry.

Activate from **GNOME Terminal outside pibox**:

```sh
home-manager switch --flake 'git+file:///home/bchapman/.dotfiles#bchapman@bchapman-ubnt'
```

Then select Ghostty in Ubuntu's application launcher and try its New Window
menu action. When existing work can safely be closed, also test launching after
all Ghostty windows have exited; a working existing instance alone does not
prove cold startup. If GNOME still caches the old entry, save work and log out
and back in before retesting. Do not try to write desktop caches into the
read-only Nix store. Keep GNOME Terminal available as a fallback. To roll back,
`git revert` the launcher-override commit and rerun the activation command.

## Ghostty child runtime isolation

The overlay's `pkgs.ghostty` uses the pinned unstable Ghostty with
`patches/ghostty-child-environment.patch`. Both the package profile and desktop
commands use this patched package. The GTK terminal-child environment drops
`GI_TYPELIB_PATH`, `GIO_EXTRA_MODULES`, `GDK_PIXBUF_MODULE_FILE` and
`GST_PLUGIN_SYSTEM_PATH_1_0`, which otherwise make native Ubuntu tools load
incompatible Nix libraries. Ghostty's own wrapper/runtime is unchanged; wrapped
Nix applications supply their own runtime. Shell configuration, CA trust,
shortcut dispatch and package pins are unchanged. Ghostty's normal PATH handling
is retained. This patch does not sanitize its own URL-opening subprocesses.

After building, activate as the normal user from **GNOME Terminal outside pibox**:

```sh
home-manager switch --flake 'git+file:///home/bchapman/.dotfiles#bchapman@bchapman-ubnt'
```

Keep GNOME Terminal open, fully quit and restart Ghostty, then relaunch herdr/pibox.
A new window of an already-running Ghostty process can still use the old binary;
existing shells and agent processes retain the old environment. In the restarted
Ghostty, `/usr/bin/gnome-terminal --version` and
`/usr/bin/gsettings get org.gnome.desktop.interface clock-format` should work
without Nix GLIBC or module-load errors. Confirm LibreOffice/Inkscape still open.

The opt-in runtime probe must fail against unpatched Ghostty and pass against the
patched binary. Run it without host activation using the package store path:

```sh
mkdir -p tmp
ghostty_package=$(nix build --no-link --print-out-paths \
  'git+file:///home/bchapman/.dotfiles#homeConfigurations."bchapman@bchapman-ubnt".pkgs.ghostty')
nix shell --inputs-from 'git+file:///home/bchapman/.dotfiles' nixpkgs#xvfb-run -c \
  xvfb-run -d -f "$PWD/tmp/ghostty-xauthority" \
  env GDK_BACKEND=x11 LIBGL_ALWAYS_SOFTWARE=1 \
  python3 tests/ghostty_child_probe.py "$ghostty_package/bin/ghostty" \
  --report tmp/ghostty-child.json
```

Retain the patch until the same runtime probe passes without it on a future
upstream/package update; upgrading alone is not evidence of a fix.

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
