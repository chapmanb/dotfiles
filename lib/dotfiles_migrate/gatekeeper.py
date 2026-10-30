"""Backup-gated Ubuntu Host Base simplification with offline-safe previews."""

import argparse
from datetime import datetime, timedelta, timezone
import errno
import fnmatch
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

from .daily_backup import BROWSABLE_REMOTE, BROWSABLE_ROOTS, RESTIC_REPOSITORY
from .runner import CommandRunner


MAX_BACKUP_AGE = timedelta(hours=48)
CLIENT_ASSETS = (
    "bin/vpnagentd", "bin/vpn", "bin/vpnui", "bin/acwebhelper",
    "vpn/profile/ginkgo-corp-profile.xml", "AnyConnectLocalPolicy.xml",
)
CORPORATE_SERVICES = (
    "centrifydc", "cisco-amp", "vpnagentd", "ir_agent", "drivestrike",
    "connectwisecontrol-a8194903625f6c0d",
)
MIGRATED_BINARIES = (
    "ghostty", "brave", "libreoffice", "inkscape", "redshift", "redshift-gtk",
    "gcloud", "gh", "task", "glow", "lazygit", "gitu", "mdbook", "pandoc",
    "clj-kondo", "clojure-lsp", "kubectl", "helm", "k9s", "argo", "terraform",
    "terraform-docs", "pre-commit", "ffmpeg", "dot", "magick", "qpdf", "sqlite3",
    "ncdu", "parallel", "datamash", "tree", "fx",
    "mysql", "mysqladmin", "mysqlcheck", "mysqldump", "mysqlimport",
    "mysqlshow", "mysqlslap", "mysql_config_editor", "mysqldumpslow",
)
APPROVED_UBUNTU_DUPLICATES = frozenset((
    "openjdk-11-jre", "openjdk-11-jre-headless", "mysql-client", "mysql-client-8.0",
    "mysql-client-core-8.0", "datamash", "ffmpeg", "graphviz", "imagemagick",
    "imagemagick-6.q16", "ncdu", "parallel", "qpdf", "sqlite3", "tree", "vim",
    "vim-runtime", "docker-compose",
))
DUPLICATE_REPLACEMENT_BINARIES = (*MIGRATED_BINARIES, "nvim")
RELOCATED_SCRIPTS = ("gws", "fix_docker_vpn.sh", "fix_docker_vpn_chris.sh")
PRESERVED_LAUNCHERS = frozenset((
    "claude", "codex", "herdr", "pibox", "nori-auth", "skills-curl", "skills-curl.ps1",
    "catalyst-agent-hook-runner", "catalyst-agent-skills-auth", "keyring", "poetry",
    "python3.12", *RELOCATED_SCRIPTS,
))
PROTECTED_HOST_PACKAGES = frozenset((
    "ubuntu-desktop", "gdm3", "gnome-shell", "gnome-terminal", "network-manager",
    "docker-ce", "docker-ce-cli", "containerd.io", "docker-buildx-plugin",
    "docker-compose-plugin", "openssh-server", "google-chrome-stable", "zoom",
    "centrifydc", "centrifydc-openssl", "centrifydc-openldap", "centrifydc-curl",
    "ciscoampconnector", "cisco-orbital", "drivestrike",
    "connectwisecontrol-a8194903625f6c0d", "dev42-lx-agent", "auditd", "lvm2",
    "cryptsetup", "cryptsetup-bin", "cryptsetup-initramfs", "grub-common",
    "grub-efi-amd64-signed", "shim-signed", "linux-image-generic-hwe-20.04",
    "intel-microcode", "oem-sutton.newell-meta", "oem-sutton.newell-aekerley-meta",
))
# Vendor updates may introduce new package suffixes; protect those families too.
PROTECTED_PACKAGE_PATTERNS = (
    "centrifydc*", "cisco*", "connectwisecontrol*", "rapid7*", "insightagent*",
    "cryptsetup*", "grub*", "linux-image*", "oem-sutton.newell-*",
)
RETIRED_PACKAGE_PATTERNS = (
    "mysql-server*", "postgresql*", "openvpn", "mongodb-org*", "virtualbox-7.0",
    "wine-stable-amd64", "typedb*", "code", "code-insiders", "antigravity",
    "brave-browser", "teams", "firefox", "libreoffice*", "inkscape", "shutter", "peek",
    "nodejs", "snowflake-cli", "glab", "gh", "task", "glow", "k9s", "pandoc",
    "ncbi-blast+", "samtools", "openbabel*", "gfortran", "coinor-libipopt-dev",
    "liblapack-dev", "libblas-dev", "libmysqlclient-dev", "cython3", "cargo",
    "mkvtoolnix", "lynx", "mosh", "silversearcher-ag", "sox", "redshift",
    "redshift-gtk", "dconf-editor", "gifsicle", "python3.9*", "python3.10*",
    "python3.11*", "ripgrep", "fzf", "jq", "zstd", "tmux", "xclip", "snapd",
)
APPROVED_APT_SOURCES = frozenset((
    "artifactory-centrify2.list",
    "artifactory_releng_ginkgo_zone_artifactory_ubuntu_local.list",
    "drivestrike.list", "docker.list", "google-chrome.sources",
    "oem-sutton.newell-aekerley-meta.list",
))
RETIRED_LOCAL_PATTERNS = (
    "aws*", "bb", "clj*", "clojure*", "lein", "nvim", "rclone", "snow", "agy",
    "Erdos*", "Cursor*", "cursor*", "orca-linux.AppImage", "jbrowse*", "joker",
    "cloud-nuke", "gdrive", "datomic*", "assume-role", *RELOCATED_SCRIPTS,
)
GIT_VERSION = "1:2.25.1-1ubuntu3.14"
BRIDGE_CONFIG = "etc/tmpfiles.d/nix-opengl-driver.conf"
BRIDGE_CONTENT = "L+ /run/opengl-driver - - - - /home/bchapman/.local/state/nix-opengl-driver\n"
SNAP_PIN = "Package: snapd\nPin: release *\nPin-Priority: -10\n"


class GateError(Exception):
    pass


def require_backup(report_path, home):
    report = json.loads(report_path.read_text())
    if (report.get("version") != 1 or report.get("repository") != RESTIC_REPOSITORY
            or report.get("browsable_remote") != BROWSABLE_REMOTE):
        raise GateError("Daily-backup verification identifies the wrong backup")
    verified_at = datetime.fromisoformat(report["verified_at"])
    if verified_at.tzinfo is None:
        raise GateError("Daily-backup verification timestamp must include a timezone")
    age = datetime.now(timezone.utc) - verified_at
    if age < timedelta(0) or age >= MAX_BACKUP_AGE:
        raise GateError("Daily-backup verification is stale or future-dated (must be <48 hours)")
    snapshot = report.get("Snapshot Backup", {})
    browsable = report.get("Browsable Sync", {})
    if (snapshot.get("status") != "VERIFIED" or snapshot.get("snapshot_count", 0) < 1
            or snapshot.get("encrypted") is not True
            or browsable.get("status") != "VERIFIED"):
        raise GateError("Daily-backup Snapshot Backup and Browsable Sync must be VERIFIED")
    expected = {(str(home / root), f"{BROWSABLE_REMOTE}/{root}", "VERIFIED")
                for root in BROWSABLE_ROOTS}
    actual = {(root.get("source"), root.get("remote"), root.get("status"))
              for root in browsable.get("roots", [])}
    if actual != expected:
        raise GateError("Daily-backup verification does not cover every browsable root")


def flake_uri(args):
    return "git+" + args.flake.as_uri()


def require_nix_profile(args, runner):
    uri = flake_uri(args)
    expression = f"(builtins.getFlake {json.dumps(uri)}).outPath"
    source = Path(runner.run([
        "nix", "eval", "--raw", "--impure", "--expr", expression,
    ]).stdout.strip())
    if not source.is_absolute() or not source.is_dir():
        raise GateError("Nix did not return an evaluated flake source directory")
    if (source / "tmp").exists() or (source / "tmp").is_symlink():
        raise GateError("Ignored tmp artifacts exist in the evaluated flake store source")
    target = f'{uri}#homeConfigurations."bchapman@bchapman-ubnt".activationPackage'
    paths = runner.run(["nix", "build", "--no-link", "--print-out-paths", target]).stdout.splitlines()
    if len(paths) != 1 or not Path(paths[0]).is_absolute():
        raise GateError("Nix did not return one Home Manager activation package")
    profile = Path(paths[0]) / "home-path"
    missing = [name for name in MIGRATED_BINARIES
               if not (profile / "bin" / name).is_file()
               or not os.access(profile / "bin" / name, os.X_OK)]
    if missing:
        raise GateError("Missing migrated Nix binaries: " + ", ".join(missing))
    return str(profile)


def require_corporate_stack(args, runner):
    client = args.system_root / "opt/cisco/secureclient"
    if not all((client / asset).is_file() and (client / asset).stat().st_size > 0
               and (not asset.startswith("bin/") or os.access(client / asset, os.X_OK))
               for asset in CLIENT_ASSETS):
        raise GateError("Cisco Secure Client assets are missing or not executable")
    certificates = args.system_root / "usr/local/share/ca-certificates"
    if not any(path.is_file() and path.stat().st_size > 0 for path in certificates.glob("*.crt")):
        raise GateError("Corporate Endpoint Stack CA certificates are missing")
    exceptions = {}
    for service in CORPORATE_SERVICES:
        try:
            state = runner.run(["systemctl", "is-active", service]).stdout.strip()
        except subprocess.CalledProcessError as error:
            if error.returncode != 3:
                raise
            state = (error.stdout or "").strip()
        if state == "active":
            continue
        if (service == "drivestrike" and getattr(args, "allow_inactive_drivestrike", False)
                and state in ("inactive", "failed", "activating", "deactivating")):
            exceptions[service] = state
            continue
        raise GateError(f"Corporate Endpoint Stack service is not active: {service}")
    return exceptions


def preflight_ubuntu(args, runner):
    require_backup(args.backup_report, args.home)
    profile = require_nix_profile(args, runner)
    exceptions = require_corporate_stack(args, runner)
    return {"daily_backup": "VERIFIED", "home_manager_profile": profile,
            "corporate_endpoint_stack": "VERIFIED_WITH_EXCEPTIONS" if exceptions else "VERIFIED",
            "corporate_service_exceptions": exceptions}


def matches(name, patterns):
    return any(fnmatch.fnmatchcase(name, pattern) for pattern in patterns)


def legacy_pip_script(path):
    if not path.is_file() or path.is_symlink():
        return False
    with path.open("rb") as stream:
        shebang = stream.readline(512)
    if not shebang.startswith(b"#!"):
        return False
    command = shebang[2:].strip().split()
    if not command:
        return False
    interpreter = command[0]
    # Ubuntu Focal's unversioned host interpreter is also Python 3.8.
    return (interpreter == b"/usr/bin/python3"
            or interpreter.rsplit(b"/", 1)[-1] in (b"python3.8", b"python3.10"))


def relocate_user_script(source, destination):
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, destination)
    try:
        source.unlink()
    except OSError as error:
        if error.errno not in (errno.EACCES, errno.EPERM, errno.EROFS):
            raise
        # pibox cannot unlink host files; simplify-ubuntu-host removes originals.
        return str(source)
    return None


def remove_user_path(path):
    if path.is_symlink() or not path.is_dir():
        path.unlink(missing_ok=True)
    else:
        shutil.rmtree(path)


def clean_user_space(args, runner, execute):
    require_backup(args.backup_report, args.home)
    require_nix_profile(args, runner)
    local_bin = args.home / ".local/bin"
    moves = []
    for name in RELOCATED_SCRIPTS:
        source = args.system_root / "usr/local/bin" / name
        destination = local_bin / name
        if source.exists() and not (destination.exists() or destination.is_symlink()):
            moves.append((source, destination))
    removals = [args.home / path for path in (
        ".nvm.retired", "install", ".local/lib/python3.8", ".local/opt/wezterm",
        ".dotfiles/tmp/cisco-vm/cisco-probe.qcow2", ".dotfiles/tmp/cisco-vm/runtime",
    )]
    if local_bin.is_dir():
        removals.extend(path for path in sorted(local_bin.iterdir())
                        if path.name not in PRESERVED_LAUNCHERS
                        and (path.name in ("agy", "ruff", "wezterm", "wezterm-gui", "wezterm-mux-server")
                             or legacy_pip_script(path)))
    removals = [path for path in removals if path.exists() or path.is_symlink()]
    bashrc = args.home / ".bashrc.local"
    old_shell = bashrc.read_text() if bashrc.exists() else ""
    new_shell = "".join(line for line in old_shell.splitlines(keepends=True)
                        if "install/system/google-cloud-sdk" not in line)
    deferred = []
    if execute:
        for source, destination in moves:
            original = relocate_user_script(source, destination)
            if original:
                deferred.append(original)
        if new_shell != old_shell:
            bashrc.write_text(new_shell)
        for path in removals:
            remove_user_path(path)
    return {"relocations": [[str(src), str(dst)] for src, dst in moves],
            "source_removal_deferred_to_host_cleanup": deferred,
            "remove": list(map(str, removals)), "update_bashrc_local": new_shell != old_shell}


def install_host_file(path, content, runner):
    # Only the install runs as root; Home Manager must keep the user's identity.
    with tempfile.TemporaryDirectory(prefix="migration-gatekeeper-") as temporary:
        source = Path(temporary) / path.name
        source.write_text(content)
        runner.run(["sudo", "install", "-D", "-m", "0644", str(source), str(path)])


def activate_host_bridge(args, runner, execute):
    result = preflight_ubuntu(args, runner)
    config = args.system_root / BRIDGE_CONFIG
    commands = [
        ["sudo", "systemd-tmpfiles", "--create", str(config)],
        ["home-manager", "switch", "--flake", f"{flake_uri(args)}#bchapman@bchapman-ubnt"],
    ]
    if execute:
        install_host_file(config, BRIDGE_CONTENT, runner)
        for command in commands:
            runner.run(command)
    return {**result, "write": {str(config): BRIDGE_CONTENT}, "commands": commands}


def protected_package(name):
    name = name.split(":", 1)[0]
    return name in PROTECTED_HOST_PACKAGES or matches(name, PROTECTED_PACKAGE_PATTERNS)


def guard_apt(runner, arguments):
    output = runner.run(["apt-get", "-s", *arguments]).stdout
    for line in output.splitlines():
        fields = line.split()
        if len(fields) >= 2 and fields[0] in ("Remv", "Purg") and protected_package(fields[1]):
            raise GateError(f"Protected Host Package Guard refused removal of {fields[1]}")


def guard_duplicate_purge(runner, candidates):
    output = runner.run(["apt-get", "-s", "purge", *candidates]).stdout
    removals = set()
    for line in output.splitlines():
        fields = line.split()
        if not fields:
            continue
        if fields[0] in ("Inst", "Conf"):
            raise GateError("Duplicate purge must not install, upgrade or configure packages")
        if fields[0] not in ("Remv", "Purg"):
            continue
        if len(fields) < 2:
            raise GateError("Malformed APT removal row")
        name = fields[1]
        if protected_package(name):
            raise GateError(f"Protected Host Package Guard refused removal of {name}")
        # APT may omit an installed native architecture suffix, but an explicit
        # suffix must match the selected identifier exactly (never another arch).
        matching = [candidate for candidate in candidates
                    if candidate == name or (":" not in name
                                             and candidate.split(":", 1)[0] == name)]
        if len(matching) != 1:
            raise GateError(f"Duplicate purge refused unselected removal of {name}")
        removals.add(matching[0])
    missing = set(candidates) - removals
    if missing:
        raise GateError("Duplicate purge simulation omitted selected packages: "
                        + ", ".join(sorted(missing)))
    return frozenset(removals), output


def require_duplicate_replacements(args, built_profile):
    active = args.home / ".nix-profile"
    store = (args.system_root / "nix/store").resolve()
    if not active.resolve().is_relative_to(store):
        raise GateError("Active Nix profile does not resolve into the Nix store")
    for label, profile in (("built", Path(built_profile)), ("active", active)):
        missing = [name for name in DUPLICATE_REPLACEMENT_BINARIES
                   if not (profile / "bin" / name).is_file()
                   or not os.access(profile / "bin" / name, os.X_OK)
                   or (label == "active"
                       and not (profile / "bin" / name).resolve().is_relative_to(store))]
        if missing:
            raise GateError(f"Missing {label} Nix replacements: " + ", ".join(missing))


def require_java17_version(runner, binary):
    result = runner.run([str(binary), "-version"])
    if not re.search(r'\b(?:openjdk|java) version "17[."]',
                     result.stdout + result.stderr):
        raise GateError(f"Retained Java runtime does not report major 17: {binary}")


def require_native_duplicate_fallbacks(args, runner, installed):
    names = {name.split(":", 1)[0] for name in installed}
    required = {"openjdk-17-jre", "openjdk-17-jre-headless", "vim-tiny"}
    if required - names:
        raise GateError("Missing retained native packages: " + ", ".join(sorted(required - names)))
    headless = next(name for name in installed if name.split(":", 1)[0] == "openjdk-17-jre-headless")
    files = runner.run(["dpkg-query", "-L", headless]).stdout.splitlines()
    java_paths = [args.system_root / name.lstrip("/") for name in files
                  if name.startswith("/usr/lib/jvm/") and name.endswith("/bin/java")]
    if len(java_paths) != 1 or not java_paths[0].is_file() or not os.access(java_paths[0], os.X_OK):
        raise GateError("Missing retained native Java17 executable")
    java = java_paths[0].resolve()
    require_java17_version(runner, java)
    editor = args.system_root / "usr/bin/vim.tiny"
    docker = args.system_root / "usr/bin/docker"
    for binary in (editor, docker):
        if not binary.is_file() or not os.access(binary, os.X_OK):
            raise GateError(f"Missing native fallback: {binary}")
    runner.run([str(editor), "--version"])
    compose = runner.run([str(docker), "compose", "version"]).stdout
    if not re.search(r"Docker Compose version v?2\.", compose):
        raise GateError("Native Docker Compose v2 is unavailable")
    return java


def verify_default_java17(args, runner, retained_java):
    try:
        default = args.system_root / "usr/bin/java"
        if default.resolve(strict=True) != retained_java:
            raise GateError("/usr/bin/java does not resolve to retained Java17")
        require_java17_version(runner, default)
    except (GateError, OSError, subprocess.CalledProcessError) as error:
        raise GateError("Purge completed but default Java17 postcheck FAILED; "
                        "no repair or restart attempted. Inspect /usr/bin/java manually.") from error


def prune_ubuntu_duplicates(args, runner, execute):
    result = preflight_ubuntu(args, runner)
    require_duplicate_replacements(args, result["home_manager_profile"])
    installed, _ = installed_packages(runner)
    retained_java = require_native_duplicate_fallbacks(args, runner, installed)
    candidates = [name for name in installed
                  if name.split(":", 1)[0] in APPROVED_UBUNTU_DUPLICATES]
    commands = []
    simulation = ""
    if candidates:
        removals, simulation = guard_duplicate_purge(runner, candidates)
        commands = [["sudo", "apt-get", "--yes", "purge", *candidates]]
        if execute:
            current_removals, _ = guard_duplicate_purge(runner, candidates)
            if current_removals != removals:
                raise GateError("Duplicate purge removal graph changed before execution")
            runner.run(commands[0])
            verify_default_java17(args, runner, retained_java)
    return {**result, "purge_packages": candidates, "commands": commands,
            "apt_simulation": simulation,
            "default_java17": "VERIFIED" if execute and candidates else "NOT_CHECKED",
            "follow_up": "Orphan candidates require separate review; no automatic autoremove. "
                         "Existing JVM processes are unchanged; no restart was performed."}


def residual_purge_arguments(residual):
    candidates = [name for name in residual if not protected_package(name)]
    return ["purge", *candidates] if candidates else []


def installed_packages(runner):
    output = runner.run(["dpkg-query", "-W", "-f=${binary:Package}\t${db:Status-Abbrev}\\n"]).stdout
    installed, residual = [], []
    for line in output.splitlines():
        name, status = line.split("\t", 1)
        if status.startswith("ii"):
            installed.append(name)
        elif status.startswith("rc"):
            residual.append(name)
    return installed, residual


def host_removals(args):
    paths = [args.system_root / path for path in (
        "home/linuxbrew.retired", "usr/local/aws-cli", "usr/local/share/Antigravity-x64",
    )]
    directory = args.system_root / "usr/local/bin"
    if directory.is_dir():
        for path in sorted(directory.iterdir()):
            if path.name in RELOCATED_SCRIPTS and path.exists():
                if not (args.home / ".local/bin" / path.name).is_file():
                    raise GateError(f"Run clean-user-space before removing {path.name}")
            if matches(path.name, RETIRED_LOCAL_PATTERNS) or (path.is_symlink() and not path.exists()):
                paths.append(path)
    sources = args.system_root / "etc/apt/sources.list.d"
    if sources.is_dir():
        paths.extend(path for path in sorted(sources.iterdir())
                     if path.name not in APPROVED_APT_SOURCES
                     and matches(path.name, ("*.list", "*.sources", "*.save")))
    return [path for path in paths if path.exists() or path.is_symlink()]


def snap_removal_commands(runner):
    rows = runner.run(["snap", "list", "--all"]).stdout.splitlines()[1:]
    names = {row.split()[0] for row in rows if row.strip()}
    # Remove apps before content providers and bases, and snapd itself last.
    def removal_order(name):
        if name == "snapd":
            return (3, name)
        if name == "bare" or name.startswith("core"):
            return (2, name)
        if name.startswith(("gnome-", "gtk-", "kde-", "mesa-")):
            return (1, name)
        return (0, name)
    return [["sudo", "snap", "remove", "--purge", name]
            for name in sorted(names, key=removal_order)]


def simplify_ubuntu_host(args, runner, execute):
    result = preflight_ubuntu(args, runner)
    installed, residual = installed_packages(runner)
    candidates = [name for name in installed
                  if matches(name.split(":", 1)[0], RETIRED_PACKAGE_PATTERNS)]
    purge = ["purge", *candidates] if candidates else []
    residual_purge = residual_purge_arguments(residual)
    # Refuse known dangerous plans before any filesystem or snap changes.
    for operation in (purge, residual_purge, ["autoremove", "--purge"]):
        if operation:
            guard_apt(runner, operation)
    removals = host_removals(args)
    snaps = snap_removal_commands(runner) if "snapd" in installed else []
    pin = args.system_root / "etc/apt/preferences.d/no-snapd.pref"
    downgrade = ["install", "--allow-downgrades", f"git={GIT_VERSION}", f"git-man={GIT_VERSION}"]
    commands = [*snaps]
    if removals:
        commands.append(["sudo", "rm", "--recursive", "--force", "--", *map(str, removals)])
    if execute:
        # apt stops package-owned daemons on purge; stop running retired units first.
        units = runner.run(["systemctl", "list-units", "--type=service", "--state=running",
                            "--no-legend", "--plain"]).stdout
        retired_units = [line.split()[0] for line in units.splitlines() if line.strip()
                         and matches(line.split()[0], ("mysql*", "postgresql*", "openvpn*",
                                                      "mongod*", "typedb*"))]
        if retired_units:
            runner.run(["sudo", "systemctl", "stop", *retired_units])
        for command in commands:
            runner.run(command)
        install_host_file(pin, SNAP_PIN, runner)
        if purge:
            guard_apt(runner, purge)
            runner.run(["sudo", "apt-get", "--yes", *purge])
        runner.run(["sudo", "apt-get", "update"])
        guard_apt(runner, downgrade)
        runner.run(["sudo", "apt-get", "--yes", *downgrade])
        # Re-simulate against the changed package graph, not just the initial plan.
        guard_apt(runner, ["autoremove", "--purge"])
        runner.run(["sudo", "apt-get", "--yes", "autoremove", "--purge"])
        _, residual = installed_packages(runner)
        residual_purge = residual_purge_arguments(residual)
        if residual_purge:
            guard_apt(runner, residual_purge)
            runner.run(["sudo", "apt-get", "--yes", *residual_purge])
    return {**result, "commands": commands, "purge_packages": candidates,
            "residual_config_packages": residual,
            "preserved_protected_residuals": [name for name in residual if protected_package(name)],
            "git_downgrade": downgrade,
            "autoremove": ["apt-get", "autoremove", "--purge"], "write": {str(pin): SNAP_PIN}}


def main(argv=None, runner=None):
    common = argparse.ArgumentParser(add_help=False, argument_default=argparse.SUPPRESS)
    for flag in ("home", "flake", "backup-report"):
        common.add_argument(f"--{flag}", type=Path)
    common.add_argument("--system-root", type=Path,
                        help="Filesystem fixture root only; external commands still target the running host")
    common.add_argument("--allow-inactive-drivestrike", action="store_true",
                        help="Report and allow only DriveStrike's nonactive state; keep the agent installed")
    for flag in ("dry-run", "execute", "confirm"):
        common.add_argument(f"--{flag}", action="store_true")
    parser = argparse.ArgumentParser(description=__doc__, parents=[common])
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("preflight-ubuntu", "clean-user-space", "activate-host-bridge",
                 "simplify-ubuntu-host", "prune-ubuntu-duplicates"):
        commands.add_parser(name, parents=[common])
    args = parser.parse_args(argv)
    args.home = getattr(args, "home", Path.home()).absolute()
    args.flake = getattr(args, "flake", args.home / ".dotfiles").absolute()
    args.backup_report = getattr(args, "backup_report", args.home / ".local/state/daily-backup/verification.json")
    args.system_root = getattr(args, "system_root", Path("/")).absolute()
    execute = getattr(args, "execute", False) and not getattr(args, "dry_run", False)
    runner = runner or CommandRunner()
    try:
        if execute and not getattr(args, "confirm", False):
            raise GateError("Host execution requires --execute --confirm")
        if args.command == "preflight-ubuntu":
            result = preflight_ubuntu(args, runner)
        elif args.command == "clean-user-space":
            result = clean_user_space(args, runner, execute)
        elif args.command == "activate-host-bridge":
            result = activate_host_bridge(args, runner, execute)
        elif args.command == "prune-ubuntu-duplicates":
            result = prune_ubuntu_duplicates(args, runner, execute)
        else:
            result = simplify_ubuntu_host(args, runner, execute)
        print(json.dumps({"status": "EXECUTED" if execute else "DRY_RUN", **result}, indent=2))
        return 0
    except (GateError, OSError, ValueError, TypeError, KeyError, AttributeError,
            subprocess.CalledProcessError) as error:
        message = "External preflight/host command failed" if isinstance(error, subprocess.CalledProcessError) else str(error)
        print(f"migration-gatekeeper: {message}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
