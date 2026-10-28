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

## Guarded system updates

The stable inputs track `nixos-26.05` and Home Manager `release-26.05`;
state versions remain `24.11`. The migration uses `mysql84` and
`argo-workflows`, and pins Snowflake CLI's Click dependency to 8.1.7.

```sh
bin/system-update check
bin/system-update run                    # preview only
bin/system-update install-root-helper   # preview one-time privileged setup
bin/system-update install-root-helper --execute --confirm
bin/system-update run --execute --confirm
# Explicitly advance unstable even without actionable advisories:
bin/system-update run --execute --confirm --include-unstable
```

Mutations require both flags; `--dry-run` always wins. A fresh (<48 hours),
complete verified backup and healthy Corporate Endpoint Stack are required.
Stable inputs advance nightly; unstable advances only for actionable advisories
(or `--include-unstable`). Dirty/non-master checkouts skip lockfile changes.
Validated changes activate with Home Manager and commit the lockfile locally,
never push. Future stable branch migrations are reported, not automatic.

The one-time setup prompts for sudo and installs a standalone root-owned code
snapshot at `/usr/local/sbin/dotfiles-system-update` (`0755`), not an import from
this checkout. Its `0440` single-command sudoers rule is validated with
`visudo -cf`. Reinstall after reviewing helper code changes. The helper accepts
no arguments and runs fixed root tracks with a sanitized environment, evaluates
and builds the flake candidate as `bchapman`. Before installation it independently
reconstructs the bundle from official, revision-pinned `NixOS/nixpkgs` inputs and
requires an identical store path: a root-owned Nix store path alone is not proof
that user-defined code is safe to run as root. It also rechecks backup freshness
and ownership invariants. Root reports stay under `/var/lib/system-update/`.

The daemon profile changes only for a newer candidate or an advisory-affected
running version with a different candidate; restart failure restores the prior
profile. APT uses `upgrade --with-new-pkgs`, refuses every simulated removal or
purge (including protected host packages), preserves config-file defaults,
rechecks corporate health, and reports reboot requirements. Ubuntu's fallback
APT timers remain enabled. User and system profiles/store retain 14 days;
system GC is skipped after daemon failure to preserve rollback evidence.

Nightly maintenance verifies the backup, runs updates and user GC, then invokes
`sudo -n /usr/local/sbin/dotfiles-system-update` for daemon/APT/system GC. Missing
helper or unavailable sudo is deferred, not fatal. Verification/update failures
warn on stderr but do not prevent repository syncs or nightly shutdown. Inspect
the private `~/.local/state/system-update/report.json` for per-track outcomes;
`--allow-inactive-drivestrike` is only a reported service-health exception.

## MySQL client provider

Home Manager adds stable pinned `mysql84` (MySQL 8.4 LTS) without retiring any
existing Nix utility or adding an explicit JDK. This is the full upstream package,
including server programs such as `mysqld`; `mysql84.client` aliases the same
derivation, not a client-only output. This is **not** a server-free store closure.
No MySQL service or data directory is enabled, started or initialized by this
addition. Existing Clojure/Leiningen and LibreOffice Application-Private JVMs
remain unchanged; this does not reduce the entire closure to a single JVM.

A local package composition retains every upstream program, replacing only the
`mysqldumpslow` launcher: it invokes the original script with explicit Nix Perl
instead of its broken upstream interpreter, forwards arguments unchanged, and
prefixes PATH with the same upstream MySQL bin directory only for this helper
(to resolve `my_print_defaults`). It does not rebuild MySQL, install global Perl
or change the shell PATH.

Preflight requires executable `mysql`, `mysqladmin`, `mysqlcheck`, `mysqldump`,
`mysqlimport`, `mysqlshow`, `mysqlslap`, `mysql_config_editor` and
`mysqldumpslow` in the built `home-path`, alongside all prior replacements.
Executable presence is not runtime proof: validate version/help launches and
synthetic slow-log parsing separately, with empty HOME and absent login-file
configuration. MySQL 8.4 removed `mysqlpump`; it is no longer a preflight
requirement. Do not use unsupported `--no-login-paths`. These offline checks do
not validate database authentication, TLS or live-database compatibility. Never put credentials,
private logs or database configuration in build inputs or evidence.

The package supplies `myisam_ftdump`, but not Ubuntu's `mysqlreport` script or
`mysqlanalyze`, `mysqloptimize` and `mysqlrepair` convenience symlinks. The latter
three use `mysqlcheck` on Ubuntu; upstream `mysqlcheck` supports `--analyze`,
`--optimize` and `--repair`. No aliases or caller rewrites are supplied here.
Pause before host removal if any missing helper name is required; resolve usage
with the operator rather than silently retiring capabilities.

Building is not activation or removal readiness. After review, the operator must
separately activate the package profile, verify active-profile replacements and
review helper usage and host safeguards before any separately approved purge.
Full Home Manager activation and service changes are not needed to test these
clients. Do not rerun the historical `simplify-ubuntu-host` workflow for duplicate
removal. No host purge or database initialization is part of this addition.

## Guarded Ubuntu duplicate pruning

Use the separate `prune-ubuntu-duplicates` command, **not** the historical
`simplify-ubuntu-host` workflow. Building alone does not update the active profile.
After reviewing the build and missing MySQL helper usage, the operator can install
only the package profile as the normal user, without full Home Manager activation:

```sh
generation=$(nix build --no-link --print-out-paths \
  'git+file:///home/bchapman/.dotfiles#homeConfigurations."bchapman@bchapman-ubnt".activationPackage')
nix-env --profile "$HOME/.nix-profile" --install "$generation/home-path"
```

This changes user packages only; do not run `$generation/activate` or
`home-manager switch` for this step. Verify all nine MySQL commands through
`~/.nix-profile/bin` with the isolated version/help and synthetic-log checks above;
Ubuntu `/usr/bin/mysql` is not proof of replacement readiness. Resolve any required
`mysqlreport`, `mysqlanalyze`, `mysqloptimize` or `mysqlrepair` usage with the operator
before removing their Ubuntu provider. No database connection is implicit.

Obtain a fresh real backup verification (never edit a report to pass the gate),
then preview and review every selected identifier and APT removal:

```sh
bin/daily-backup verify
bin/migration-gatekeeper prune-ubuntu-duplicates
# If verification used a different writable state directory:
bin/migration-gatekeeper prune-ubuntu-duplicates --backup-report /path/to/verification.json
# Only after separate operator approval, in an unsandboxed host terminal:
bin/migration-gatekeeper prune-ubuntu-duplicates --execute --confirm
```

Never run the whole gatekeeper under sudo. Preview is the default; `--execute`
requires `--confirm`, and `--dry-run` wins over both. Empty installed selection
never invokes APT purge. Previews can evaluate/build Nix but do not mutate host
packages, alternatives or services. The command reuses the <48-hour complete
backup, Git-filtered build, Cisco assets/CA and corporate-service gates. An
explicitly accepted `--allow-inactive-drivestrike` is a reported **health-only**
exception, never permission to remove the agent or bypass other checks.

The exact approved Ubuntu removal names are `openjdk-11-jre`,
`openjdk-11-jre-headless`, `mysql-client`, `mysql-client-8.0`,
`mysql-client-core-8.0`, `datamash`, `ffmpeg`, `graphviz`, `imagemagick`,
`imagemagick-6.q16`, `ncdu`, `parallel`, `qpdf`, `sqlite3`, `tree`, `vim`,
`vim-runtime`, and legacy `docker-compose`. Only installed members are selected;
architecture-qualified package IDs are retained. All existing Nix tools remain,
including ImageMagick, sqlite3, tree, ffmpeg, Graphviz, datamash, ncdu, parallel,
qpdf and Neovim. Both built and active Nix replacements must be executable;
native Java17 packages/runtime, `vim-tiny` recovery editing and Docker Compose v2
must remain available.

Caller investigation found that the `campaign-client`, `ice-floe-web` and
`ice-jotunn` Makefile `%-ext` workflows invoke legacy `docker-compose`. The
operator explicitly accepted retiring those workflows and removing the Ubuntu
legacy provider: those targets will no longer work as written after removal.
Docker and Compose v2 remain, but do not supply the legacy command on PATH.
No project rewrite or compatibility shim is included. Inspect relevant tooling
before proceeding; any newly discovered required legacy caller outside this
accepted scope still needs an operator decision. A scoped source search cannot
prove that all other project or binary launchers are independent of the name.

The protected corporate/boot/desktop/system Python/graphics stack, Docker and
Compose v2, kernels, GCC/G++/Fortran and **all other host compiler/build support**
remain native. Ubuntu Java17 stays for ConnectWise; existing Clojure/Leiningen
OpenJDK21 and LibreOffice private JRE17 stay unchanged. There is no new explicit
Nix JDK and no claim of a single JVM on disk.

Initial and immediately-before-purge APT simulations must contain no removal
outside the selected installed set, including unselected approved names or
foreign explicit architectures, and no installs/upgrades/configuration actions.
For a nonempty selection, each simulation must cover every selected package;
empty or partial removal output is a refusal, not proof of safety. A changed
removal graph also stops execution. Execution issues only the explicit
selected `sudo apt-get --yes purge ...`: no autoremove, residual-config sweep,
APT update, upgrade/downgrade, source/mark change, filesystem cleanup, activation,
alternatives command or service restart. The preview's `apt_simulation` preserves
APT's orphan-candidate report separately from `purge_packages`; those candidates
require another approval and are never automatically removed here.

After an actual purge, the gatekeeper requires `/usr/bin/java` to resolve to the
retained native Java17 and report major 17. A failure is reported **after** the
purge, without hidden repair or rollback; stop and inspect manually. Also audit
package status, active replacements, native editor/Compose v2 and corporate
service health separately. Existing Java11 processes may still run: changing the
default command does not switch their JVM. Any restart/reboot or alternatives
repair needs separate approval. Unit tests and a clean simulation do not authorize
host execution or prove application behavior.

## Backup-gated Ubuntu simplification (historical workflow)

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
