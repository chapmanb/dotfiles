"""Daily encrypted snapshots, browsable document sync, and verified retirement."""

import argparse
from contextlib import ExitStack, contextmanager
from datetime import date, datetime, timedelta, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import secrets
import shlex
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile

from .config import BACKUP_REMOTE, CUTOFF_TS, excluded, rclone_exclusions
from .cold_archive import git_metadata, write_private
from .runner import CommandRunner, Rclone


RESTIC_REPOSITORY = f"rclone:{BACKUP_REMOTE}/restic-repo"
BROWSABLE_REMOTE = f"{BACKUP_REMOTE}/browsable"
TRASH_REMOTE = f"{BACKUP_REMOTE}/browsable-trash"
LEGACY_REMOTE = "gdrive:bchapman-ubnt-backup"
BROWSABLE_ROOTS = ("personal", "Documents")
GINKGO_ROOTS = ("ginkgo/analyses", "ginkgo/methods", "ginkgo/notes")
SNAPSHOT_ROOTS = (
    ".dotfiles", ".ssh", ".gnupg", ".password-store", ".aws", ".kube", ".vpn",
    ".snowflake", ".dbt", ".pypirc", ".bashrc.local", ".bashrc.ginkgo", "secrets.env",
    "todo.txt", "notes.txt", ".spacemacs.env", ".agents", ".claude",
    ".claude.json", ".codex", ".pi/agent", ".gemini", ".config/herdr", ".config/nvim",
    ".config/msmtp", ".config/gspread", ".config/rclone", ".config/autostart",
    ".local/bin", ".config/monitors.xml", ".config/dconf", ".config/gh",
    ".config/glab-cli", ".config/gcloud", ".docker/config.json", ".local/share/keyrings",
)
EXCLUDED_CODE_REPOS = frozenset({"acs-mirror-local", "hamilton-ultimate"})
PRIVATE_EXCLUSIONS = (
    ".dotfiles/tmp", ".codex/packages", ".pi/agent/npm", ".pi/agent/git",
    ".pi/agent/sessions", ".pi/agent/web-search-cache", ".cache", ".local/share/pnpm",
    ".bun", ".nvm", "bio", "ginkgo/guild", ".config/restic",
    "ginkgo/code/acs-mirror-local", "ginkgo/code/hamilton-ultimate",
    ".config/gcloud/logs", ".config/gcloud/cache",
)


VPN_INSTALLER = ".vpn/installers/cisco-secure-client-linux64-5.1.18.314-predeploy-deb-k9.tgz"
SECURECLIENT_ASSETS = (
    "ginkgo-corp-profile.xml", "AnyConnectLocalPolicy.xml", "vpnagentd", "vpnui", "acwebhelper",
)
REQUIRED_OVERLAY = (
    "etc/os-release", "etc/apparmor.d/nix-userns", "etc/bash.bashrc",
    "etc/tmpfiles.d/nix-opengl-driver.conf",
)
OVERLAY_PATHS = (
    *REQUIRED_OVERLAY,
    "etc/apparmor.d/abstractions/centrifydc",
    "etc/apparmor.d/abstractions/authentication.d/centrifydc",
    "etc/apparmor.d/abstractions/nameservice.d/centrifydc", "etc/apparmor.d/local",
    "etc/sysctl.d", "etc/brave/policies/managed", "etc/opt/chrome/policies/managed",
    "etc/apt/sources.list.d", "etc/apt/preferences.d", "etc/apt/keyrings",
    "etc/apt/trusted.gpg.d", "etc/fstab", "etc/crypttab", "etc/default/grub",
    "etc/default/grub.d", "etc/default/kdump-tools", "etc/pam.d", "etc/nsswitch.conf",
    "etc/ssh/sshd_config", "etc/ssh/sshd_config.d", "etc/systemd/system",
)
INVENTORY_COMMANDS = {
    "packages.tsv": ["dpkg-query", "-W", "-f=${binary:Package}\\t${Version}\\t${Status}\\n"],
    "package-selections.txt": ["dpkg", "--get-selections"],
    "manual-packages.txt": ["apt-mark", "showmanual"],
    "held-packages.txt": ["apt-mark", "showhold"],
    "service-unit-files.txt": ["systemctl", "list-unit-files", "--type=service", "--no-pager"],
    "storage.json": ["lsblk", "--json", "--output", "NAME,TYPE,FSTYPE,UUID,PARTUUID,SIZE,MOUNTPOINT"],
    "mounts.json": ["findmnt", "--json", "--output", "TARGET,SOURCE,FSTYPE"],
}
PROTECTED_ARCHIVE = "protected-host-config.tar.gz"
PROTECTED_MANIFEST = "protected-host-config.SHA256SUMS"
PROTECTED_CERTIFICATES = (
    "var/centrify/net/certs/auto_GinkgoComputerAuthenticationv2.crt",
    "var/centrify/net/certs/auto_GinkgoComputerAuthenticationv2.key",
    "usr/local/share/ca-certificates/GinkgoBioworks_Root_CA.crt",
)
PROTECTED_PROFILES = tuple(
    f"etc/NetworkManager/system-connections/Ginkgo {name}.nmconnection"
    for name in ("Corporate", "Internet", "Visitor")
)


class BackupError(Exception):
    pass


@contextmanager
def open_real_directory(path):
    """Anchor traversal to directory descriptors; never follow a symlink component."""
    descriptor = os.open("/", os.O_RDONLY | os.O_DIRECTORY)
    try:
        for part in Path(path).absolute().parts[1:]:
            child = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                            dir_fd=descriptor)
            os.close(descriptor)
            descriptor = child
        yield descriptor
    finally:
        os.close(descriptor)


def require_no_symlink_components(path):
    with open_real_directory(path):
        pass


def require_private_directory(descriptor, uid, gid):
    info = os.fstat(descriptor)
    if (info.st_uid, info.st_gid, stat.S_IMODE(info.st_mode)) != (uid, gid, 0o700):
        raise BackupError("Protected destination ownership invariant requires operator-owned 0700 directories")


@contextmanager
def private_temporary(directory_fd):
    name = ".protected-" + secrets.token_hex(16)
    descriptor = os.open(name, os.O_RDWR | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                         0o600, dir_fd=directory_fd)
    try:
        with os.fdopen(descriptor, "w+b") as stream:
            os.fchmod(stream.fileno(), 0o600)
            yield name, stream
    finally:
        try:
            os.unlink(name, dir_fd=directory_fd)
        except FileNotFoundError:
            pass


def write_protected_bundle(directory_fd, sources, owner):
    with ExitStack() as stack:
        archive_name, archive = stack.enter_context(private_temporary(directory_fd))
        with tarfile.open(fileobj=archive, mode="w:gz") as bundle:
            for name, source in sources:
                info = os.fstat(source.fileno())
                member = tarfile.TarInfo(name)
                member.size = info.st_size
                member.mode = 0o600
                bundle.addfile(member, source)
        archive.seek(0)
        digest = hashlib.file_digest(archive, "sha256").hexdigest()
        manifest_name, manifest = stack.enter_context(private_temporary(directory_fd))
        manifest.write((f"{digest}  {PROTECTED_ARCHIVE}\n" + "".join(
            f"# source: {name}\n" for name, _ in sources)).encode())
        for stream in (archive, manifest):
            stream.flush()
            os.fchown(stream.fileno(), owner.st_uid, owner.st_gid)
            os.fsync(stream.fileno())
        os.replace(archive_name, PROTECTED_ARCHIVE, src_dir_fd=directory_fd, dst_dir_fd=directory_fd)
        os.replace(manifest_name, PROTECTED_MANIFEST, src_dir_fd=directory_fd, dst_dir_fd=directory_fd)
    return digest


def present(path):
    return path.exists() or path.is_symlink()


def warn_unreadable(error):
    if not isinstance(error, PermissionError):
        raise error
    print(f"daily-backup: Permission denied during scan: {error.filename}; "
          "leaving source for restic to inspect", file=sys.stderr)


def secureclient_copy_ignores(directory, names):
    ignored = []
    for name in names:
        path = Path(directory) / name
        # Runtime state is neither needed for restoration nor reliably readable.
        if name in {"temp", "log", "logs"}:
            ignored.append(name)
        elif not path.is_symlink():
            mode = os.R_OK | os.X_OK if path.is_dir() else os.R_OK
            if not os.access(path, mode):
                ignored.append(name)
    return ignored


def restic_pattern(path):
    # An actual filename must not accidentally become an exclusion glob.
    return "".join("\\" + char if char in "*?[]\\" else char for char in str(path))


class DailyBackup:
    def __init__(self, home, state, system_root, runner, rclone, today=None):
        self.home = Path(home).absolute()
        self.state = Path(state).absolute()
        self.system_root = Path(system_root)
        self.runner = runner
        self.rclone = rclone
        self.today = today or date.today()
        self.stage = self.state / "corporate-access-stack"
        self.password = self.home / ".config/restic/password"
        self.report = self.state / "verification.json"
        self._restic_ready = False

    def _private_state(self):
        self.state.mkdir(parents=True, exist_ok=True, mode=0o700)
        if self.state.is_symlink():
            raise BackupError("State directory must not be a symlink")
        self.state.chmod(0o700)

    def stage_corp_stack(self):
        certs = self.system_root / "usr/local/share/ca-certificates"
        config = self.system_root / "etc/ca-certificates.conf"
        client = self.system_root / "opt/cisco/secureclient"
        certificates = sorted(certs.glob("*.crt"))
        if certs.exists() and not certificates:
            raise BackupError("Corporate CA certificate directory contains no certificates")
        self._private_state()
        # Publish only a complete copy; failure leaves the previous staging tree intact.
        with tempfile.TemporaryDirectory(prefix=".corp-", dir=self.state) as temporary:
            bundle = Path(temporary) / "bundle"
            bundle.mkdir(mode=0o700)
            # NixOS manages PKI declaratively; these Ubuntu paths may be absent.
            if certs.exists():
                destination = bundle / "usr/local/share/ca-certificates"
                destination.mkdir(parents=True)
                for certificate in certificates:
                    shutil.copy2(certificate, destination / certificate.name)
            if config.exists():
                (bundle / "etc").mkdir()
                shutil.copy2(config, bundle / "etc/ca-certificates.conf")
            if client.exists():
                staged_client = bundle / "opt/cisco/secureclient"
                shutil.copytree(client, staged_client, symlinks=True, ignore=secureclient_copy_ignores)
                self._require_secureclient(staged_client)
            if self._has_host_recovery():
                self._copy_host_recovery(bundle)
                self._write_host_inventory(bundle)
                self._require_host_recovery(bundle)
                self._refresh_protected_bundle(bundle)
                self._require_protected_bundle(bundle)
            if self.stage.is_symlink():
                raise BackupError("Corporate staging directory must not be a symlink")
            previous = Path(temporary) / "previous"
            if self.stage.exists():
                self.stage.rename(previous)
            try:
                bundle.rename(self.stage)
            except OSError:
                if previous.exists():
                    previous.rename(self.stage)
                raise
        return {"stage": str(self.stage), "status": "STAGED"}

    def _has_host_recovery(self):
        return present(self.system_root / "etc/os-release")

    def _require_file(self, path, *, private=False, nonempty=True):
        info = path.lstat()
        if (not stat.S_ISREG(info.st_mode) or not os.access(path, os.R_OK)
                or (nonempty and info.st_size == 0)
                or (private and stat.S_IMODE(info.st_mode) != 0o600)):
            raise BackupError(f"Required recovery file missing, unreadable, or invalid: {path}")

    def _require_secureclient(self, client):
        for name in SECURECLIENT_ASSETS:
            if not any(path.is_file() and os.access(path, os.R_OK) for path in client.rglob(name)):
                raise BackupError(f"Required Secure Client asset missing or unreadable: {name}")

    def _copy_readable(self, source, destination):
        if source.is_symlink():
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.symlink_to(os.readlink(source))
        elif source.is_dir():
            if os.access(source, os.R_OK | os.X_OK):
                destination.mkdir(parents=True, exist_ok=True)
                for child in sorted(source.iterdir()):
                    self._copy_readable(child, destination / child.name)
        elif source.is_file() and os.access(source, os.R_OK):
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, destination)

    def _copy_host_recovery(self, bundle):
        paths = [self.system_root / name for name in OVERLAY_PATHS]
        paths.extend(self.system_root / name for name in (
            "opt/.cisco/certificates", "etc/centrifydc",
        ))
        paths.extend((self.system_root / "opt").glob("connectwise*/ClientLaunchParameters.txt"))
        paths.extend((self.system_root / "usr/share/keyrings").glob("*oem*.gpg"))
        paths.extend((self.system_root / "usr/share/keyrings").glob("*chrome*.gpg"))
        release = self.system_root / "etc/os-release"
        if release.is_symlink():
            target = Path(os.readlink(release))
            target = (self.system_root / str(target).lstrip("/") if target.is_absolute()
                      else release.parent / target)
            paths.append(Path(os.path.normpath(target)))
        for source in paths:
            self._copy_readable(source, bundle / source.relative_to(self.system_root))

    def _write_host_inventory(self, bundle):
        inventory = bundle / "host-inventory"
        inventory.mkdir(mode=0o700)
        for name, command in INVENTORY_COMMANDS.items():
            write_private(inventory / name, self.runner.run(command).stdout)
        profiles = [self.home / ".nix-profile", self.home / ".local/state/nix/profiles/profile"]
        for directory in (self.system_root / "nix/var/nix/profiles",
                          self.home / ".local/state/nix/profiles"):
            if directory.is_dir():
                profiles.extend(directory.rglob("*"))
        references = [f"{path} -> {os.readlink(path)}" for path in sorted(set(profiles))
                      if path.is_symlink()]
        write_private(inventory / "nix-profile-references.txt", "\n".join(references) + "\n")

    def _require_host_recovery(self, bundle):
        self._require_file(self.home / VPN_INSTALLER, private=True)
        for name in (*REQUIRED_OVERLAY, "etc/ca-certificates.conf"):
            # os-release commonly links to /usr/lib/os-release; preserve that link.
            path = bundle / name
            if path.is_symlink():
                target = Path(os.readlink(path))
                path = bundle / str(target).lstrip("/") if target.is_absolute() else path.parent / target
            self._require_file(path)
        certificates = list((bundle / "usr/local/share/ca-certificates").glob("*.crt"))
        if not certificates:
            raise BackupError("Required staged corporate CA certificates missing")
        for certificate in certificates:
            self._require_file(certificate)
        self._require_secureclient(bundle / "opt/cisco/secureclient")
        for name in (*INVENTORY_COMMANDS, "nix-profile-references.txt"):
            self._require_file(bundle / "host-inventory" / name, nonempty=name != "held-packages.txt")
        for name in ("packages.tsv", "manual-packages.txt", "nix-profile-references.txt"):
            if not (bundle / "host-inventory" / name).read_text().strip():
                raise BackupError(f"Empty host inventory: {name}")

    def _require_protected_bundle(self, bundle):
        for name in (PROTECTED_ARCHIVE, PROTECTED_MANIFEST):
            self._require_file(bundle / name, private=True)
        records = [line for line in (bundle / PROTECTED_MANIFEST).read_text().splitlines()
                   if line and not line.startswith("#")]
        with (bundle / PROTECTED_ARCHIVE).open("rb") as archive:
            digest = hashlib.file_digest(archive, "sha256").hexdigest()
        if records != [f"{digest}  {PROTECTED_ARCHIVE}"]:
            raise BackupError("Protected host archive checksum mismatch")

    def _refresh_protected_bundle(self, bundle):
        # The zero-argument helper has a fixed destination. Temporarily reserve it,
        # restoring the previous tree even if sudo or integrity validation fails.
        if self.stage.is_symlink():
            raise BackupError("Corporate staging directory must not be a symlink")
        previous = bundle.parent / "previous"
        if self.stage.exists():
            self.stage.rename(previous)
        try:
            self.stage.mkdir(mode=0o700)
            self.runner.run(["sudo", "-n", "/usr/local/sbin/dotfiles-backup-stage"])
            self._require_protected_bundle(self.stage)
            for name in (PROTECTED_ARCHIVE, PROTECTED_MANIFEST):
                shutil.copy2(self.stage / name, bundle / name)
        finally:
            if self.stage.exists():
                shutil.rmtree(self.stage)
            if previous.exists():
                previous.rename(self.stage)

    def _protected_sources(self):
        connections = self.system_root / "etc/NetworkManager/system-connections"
        sudoers = self.system_root / "etc/sudoers.d"
        for directory in (connections, sudoers):
            require_no_symlink_components(directory)
        names = {"etc/NetworkManager/NetworkManager.conf", "etc/sudoers",
                 *PROTECTED_PROFILES, *PROTECTED_CERTIFICATES}
        names.update(path.relative_to(self.system_root).as_posix()
                     for path in connections.glob("*.nmconnection"))
        names.update(path.relative_to(self.system_root).as_posix()
                     for path in sudoers.iterdir())
        sources = []
        for name in sorted(names):
            path = self.system_root / name
            if name in PROTECTED_CERTIFICATES:
                path = path.resolve(strict=True)
                if not path.is_relative_to(self.system_root.resolve()):
                    raise BackupError(f"Certificate target escapes system root: {name}")
            require_no_symlink_components(path.parent)
            info = path.lstat()
            if not stat.S_ISREG(info.st_mode):
                raise BackupError(f"Protected source must be a regular non-symlink file: {name}")
            sources.append((name, path))
        return sources

    def _open_protected_sources(self, stack):
        opened = []
        for name, path in self._protected_sources():
            with open_real_directory(path.parent) as parent_fd:
                descriptor = os.open(path.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK,
                                     dir_fd=parent_fd)
            source = stack.enter_context(os.fdopen(descriptor, "rb"))
            if not stat.S_ISREG(os.fstat(source.fileno()).st_mode):
                raise BackupError(f"Protected source is not a regular file: {name}")
            opened.append((name, source))
        return opened

    def stage_protected(self):
        from .system_update import BACKUP_HELPER, BACKUP_SUDOERS, require_root_owned

        if os.geteuid() != 0:
            raise BackupError("stage-protected requires root")
        for relative in (BACKUP_HELPER, BACKUP_SUDOERS):
            path = self.system_root / relative
            require_root_owned(path, self.system_root)
            if not path.is_file():
                raise BackupError(f"Installed helper policy must be a regular file: {path}")
        with ExitStack() as stack:
            home_fd = stack.enter_context(open_real_directory(self.home))
            owner = os.fstat(home_fd)
            state_fd = stack.enter_context(open_real_directory(self.state))
            require_private_directory(state_fd, owner.st_uid, owner.st_gid)
            # Validate every source before creating any destination artifacts.
            opened = self._open_protected_sources(stack)
            try:
                os.mkdir(self.stage.name, mode=0o700, dir_fd=state_fd)
                created = True
            except FileExistsError:
                created = False
            stage_fd = os.open(self.stage.name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                               dir_fd=state_fd)
            stack.callback(os.close, stage_fd)
            if created:
                os.fchown(stage_fd, owner.st_uid, owner.st_gid)
                os.fchmod(stage_fd, 0o700)
            require_private_directory(stage_fd, owner.st_uid, owner.st_gid)
            for name in (PROTECTED_ARCHIVE, PROTECTED_MANIFEST):
                try:
                    info = os.stat(name, dir_fd=stage_fd, follow_symlinks=False)
                except FileNotFoundError:
                    continue
                if not stat.S_ISREG(info.st_mode):
                    raise BackupError(f"Protected destination must not be a symlink: {name}")
            digest = write_protected_bundle(stage_fd, opened, owner)
        return {"stage": str(self.stage), "status": "STAGED", "sha256": digest}

    def _require_password(self):
        if self.password.is_symlink() or not self.password.is_file():
            raise BackupError("Run daily-backup init to provision the restic password file")
        if self.password.stat().st_mode & 0o777 != 0o600 or not self.password.read_text().strip():
            raise BackupError("Restic password file must be nonempty and mode 0600")

    def _restic(self, *args):
        self._require_password()
        if not self._restic_ready:
            # Use the shared helper to prepare a writable token-refresh config,
            # including for the rclone subprocess launched internally by restic.
            self.rclone.run("version")
            self._restic_ready = True
        return self.runner.run([
            "restic", "-r", RESTIC_REPOSITORY, "--password-file", str(self.password),
            "-o", "rclone.args=serve restic --stdio --config " + shlex.quote(str(self.rclone.config_path)),
            *args,
        ])

    def init(self):
        if self.password.is_symlink():
            raise BackupError("Restic password file must not be a symlink")
        if not self.password.exists():
            secret = self.runner.run(["pass", "show", "backup/restic-gdrive"]).stdout.splitlines()
            if not secret or not secret[0].strip():
                raise BackupError("pass backup/restic-gdrive contains no password")
            self.password.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
            write_private(self.password, secret[0] + "\n")
        self.password.chmod(0o600)
        try:
            self._restic("init", "--repository-version", "2")
        except subprocess.CalledProcessError:
            # restic init never overwrites an existing repository. Only a
            # successful authenticated config read makes a failed init idempotent.
            self._restic("cat", "config")
        return {"repository": RESTIC_REPOSITORY, "status": "INITIALIZED"}

    def _project_is_active(self, project):
        incomplete = False
        latest_mtime = 0

        def unreadable(error):
            nonlocal incomplete
            warn_unreadable(error)
            incomplete = True

        for root, directories, files in os.walk(project, followlinks=False, onerror=unreadable):
            root = Path(root)
            kept = []
            for name in directories + files:
                path = root / name
                if excluded(Path(project.name) / path.relative_to(project)):
                    continue
                try:
                    info = path.lstat()
                except PermissionError as error:
                    unreadable(error)
                    continue
                if stat.S_ISDIR(info.st_mode):
                    kept.append(name)
                else:
                    latest_mtime = max(latest_mtime, info.st_mtime)
            directories[:] = kept
        try:
            latest_mtime = latest_mtime or project.stat().st_mtime
            commit = git_metadata(project, self.runner)["git_commit_ts"]
        except (subprocess.CalledProcessError, PermissionError, ValueError):
            # Unlike pruning, backing up an uncertain/unborn repository is safe.
            print(f"daily-backup: Git activity unavailable for {project}; including in backup",
                  file=sys.stderr)
            return True
        # Unreadable descendants might contain recent work: include, never omit.
        return incomplete or max(latest_mtime, commit) >= CUTOFF_TS

    def snapshot_sources(self):
        paths = []
        code = self.home / "ginkgo/code"
        if code.exists():
            for project in sorted(code.iterdir()):
                if not project.is_dir() or project.is_symlink() or project.name in EXCLUDED_CODE_REPOS:
                    continue
                if project.name == "agent-plans":
                    paths.append(project)
                    continue
                if self._project_is_active(project):
                    paths.append(project)
        paths.extend(self.home / name for name in GINKGO_ROOTS)
        paths.extend(self.home / name for name in SNAPSHOT_ROOTS if present(self.home / name))
        paths.extend(sorted(self.home.glob(".claude*")))
        return list(dict.fromkeys(paths)) + [self.stage]

    def exclusion_patterns(self, sources):
        paths = {self.home / name for name in PRIVATE_EXCLUSIONS}
        paths.add(Path("/home/linuxbrew"))
        # Expand cache rules to concrete paths, pruning before descending. This
        # preserves .git refs/objects even when a branch is named target or venv.
        for source in sources:
            if not source.is_dir() or source.is_symlink() or source == self.stage:
                continue
            for root, directories, files in os.walk(source, followlinks=False, onerror=warn_unreadable):
                root = Path(root)
                if root != source and ("pyvenv.cfg" in files or "CACHEDIR.TAG" in files):
                    paths.add(root)
                    directories[:] = []
                    continue
                kept = []
                for name in sorted(directories + files):
                    if name == ".git":
                        continue
                    path = root / name
                    if path in paths or excluded(Path(source.name) / path.relative_to(source)):
                        paths.add(path)
                    elif name in directories:
                        try:
                            if not path.is_symlink():
                                kept.append(name)
                        except PermissionError as error:
                            warn_unreadable(error)
                directories[:] = kept
        return sorted(restic_pattern(path) for path in paths)

    def _empty_report(self):
        return {
            "version": 1, "repository": RESTIC_REPOSITORY,
            "browsable_remote": BROWSABLE_REMOTE,
            "verified_at": datetime.now(timezone.utc).isoformat(),
            "Snapshot Backup": {"status": "UNVERIFIED"},
            "Browsable Sync": {"status": "UNVERIFIED", "roots": []},
        }

    def _write_report(self, report):
        self._private_state()
        write_private(self.report, json.dumps(report, indent=2) + "\n")

    def _require_real_roots(self, roots):
        for relative in roots:
            path = self.home / relative
            if not path.is_dir() or path.is_symlink():
                raise BackupError(f"Backup source must be a real directory: {path}")

    def run(self):
        self._write_report(self._empty_report())
        self._require_password()
        self._require_real_roots(BROWSABLE_ROOTS + GINKGO_ROOTS)
        self.stage_corp_stack()
        sources = self.snapshot_sources()
        exclusions = self.state / "restic-excludes.txt"
        write_private(exclusions, "\n".join(self.exclusion_patterns(sources)) + "\n")
        self._restic("backup", "--compression", "auto", "--exclude-file", str(exclusions),
                     *map(str, sources))
        self._restic("forget", "--keep-daily", "7", "--keep-weekly", "4", "--keep-monthly", "6", "--prune")
        for relative in BROWSABLE_ROOTS:
            self.rclone.run(
                "sync", str(self.home / relative), f"{BROWSABLE_REMOTE}/{relative}",
                "--backup-dir", f"{TRASH_REMOTE}/{self.today.isoformat()}/{relative}",
                "--links", *rclone_exclusions(),
            )
        self.prune_trash()
        return {"status": "BACKED_UP", "sources": list(map(str, sources))}

    def prune_trash(self):
        try:
            entries = json.loads(self.rclone.run("lsjson", TRASH_REMOTE, "--dirs-only").stdout)
        except subprocess.CalledProcessError as error:
            if error.returncode == 3:  # No rollover has occurred yet.
                return
            raise
        if not isinstance(entries, list):
            raise BackupError("Invalid browsable trash listing")
        for entry in entries:
            name = entry.get("Name", "")
            if entry.get("IsDir") is not True or not re.fullmatch(r"\d{4}-\d{2}-\d{2}", name):
                continue
            try:
                day = date.fromisoformat(name)
            except ValueError:
                continue
            if day < self.today - timedelta(days=30):
                self.rclone.run("purge", f"{TRASH_REMOTE}/{name}")

    def verify(self):
        report = self._empty_report()
        # Revoke a previous success before doing anything that might fail.
        self._write_report(report)
        try:
            self._require_real_roots(GINKGO_ROOTS)
            if self._has_host_recovery():
                self._require_host_recovery(self.stage)
                self._require_protected_bundle(self.stage)
            self._restic("check")
            config = json.loads(self._restic("cat", "config").stdout)
            if (not isinstance(config, dict) or type(config.get("version")) is not int
                    or config["version"] < 2 or not isinstance(config.get("id"), str)
                    or not config["id"].strip()):
                raise BackupError("Invalid restic encryption config")
            keys = json.loads(self._restic("key", "list", "--json").stdout)
            if not isinstance(keys, list) or not keys or not all(
                isinstance(key, dict) and isinstance(key.get("id"), str) and key["id"].strip()
                for key in keys
            ):
                raise BackupError("No valid restic encryption keys")
            snapshots = json.loads(self._restic("snapshots", "--json").stdout)
            if not isinstance(snapshots, list) or not snapshots or not all(
                isinstance(snapshot, dict) and isinstance(snapshot.get("id"), str)
                and snapshot["id"].strip() for snapshot in snapshots
            ):
                raise BackupError("No valid restic snapshots")
            paths = snapshots[-1].get("paths", [])
            required_paths = [str(self.home / relative) for relative in GINKGO_ROOTS] + [str(self.stage)]
            if not isinstance(paths, list) or not all(path in paths for path in required_paths):
                raise BackupError("Latest restic snapshot omits required Ginkgo roots or corporate staging directory")
            report["Snapshot Backup"] = {
                "status": "VERIFIED", "encrypted": True, "snapshot_count": len(snapshots),
            }
        except (BackupError, OSError, ValueError, subprocess.CalledProcessError):
            report["Snapshot Backup"]["error"] = (
                "Host recovery integrity, Restic check, encryption config/key check, or snapshot coverage failed"
            )
        roots = report["Browsable Sync"]["roots"]
        for relative in BROWSABLE_ROOTS:
            result = {"source": str(self.home / relative), "remote": f"{BROWSABLE_REMOTE}/{relative}",
                      "status": "UNVERIFIED"}
            try:
                source = self.home / relative
                self._require_real_roots((relative,))
                self.rclone.run("check", str(source), result["remote"], "--one-way", "--links",
                                *rclone_exclusions())
                result["status"] = "VERIFIED"
            except (BackupError, OSError, subprocess.CalledProcessError):
                result["error"] = "Browsable source or remote check failed"
            roots.append(result)
        if all(root["status"] == "VERIFIED" for root in roots):
            report["Browsable Sync"]["status"] = "VERIFIED"
        self._write_report(report)
        return report

    def retire_legacy_backup(self, execute, confirm, dry_run=False):
        if not execute or not confirm:
            raise BackupError("Legacy retirement requires --execute --confirm and passing verification")
        report = json.loads(self.report.read_text())
        if (report.get("version") != 1 or report.get("repository") != RESTIC_REPOSITORY
                or report.get("browsable_remote") != BROWSABLE_REMOTE
                or report.get("Snapshot Backup", {}).get("encrypted") is not True
                or report.get("Snapshot Backup", {}).get("snapshot_count", 0) < 1
                or any(report.get(tier, {}).get("status") != "VERIFIED"
                       for tier in ("Snapshot Backup", "Browsable Sync"))):
            raise BackupError("Snapshot Backup and Browsable Sync must both be VERIFIED")
        expected = {(str(self.home / relative), f"{BROWSABLE_REMOTE}/{relative}", "VERIFIED")
                    for relative in BROWSABLE_ROOTS}
        actual = {(root.get("source"), root.get("remote"), root.get("status"))
                  for root in report["Browsable Sync"].get("roots", [])}
        if actual != expected:
            raise BackupError("Verification does not cover all configured browsable roots")
        if not dry_run:
            self.rclone.run("purge", LEGACY_REMOTE)
        return {"status": "DRY_RUN" if dry_run else "RETIRED", "remote": LEGACY_REMOTE}


def main(argv=None, runner=None):
    from .gatekeeper import GateError
    from .system_update import install_root_helper

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--home", type=Path, default=Path.home())
    parser.add_argument("--state-dir", type=Path, help="Writable private staging/report directory")
    parser.add_argument("--system-root", type=Path, default=Path("/"))
    parser.add_argument("--rclone-config", type=Path)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--confirm", action="store_true")
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("stage-corp-stack", "stage-protected", "install-root-helper",
                 "init", "run", "verify", "retire-legacy-backup"):
        command = commands.add_parser(name)
        command.add_argument("--dry-run", action="store_true", default=argparse.SUPPRESS)
        for flag in ("home", "state-dir", "system-root"):
            command.add_argument(f"--{flag}", type=Path, default=argparse.SUPPRESS)
        if name in ("retire-legacy-backup", "install-root-helper"):
            command.add_argument("--execute", action="store_true", default=argparse.SUPPRESS)
            command.add_argument("--confirm", action="store_true", default=argparse.SUPPRESS)
    args = parser.parse_args(argv)
    runner = runner or CommandRunner()
    config = args.rclone_config or args.home / ".config/rclone/rclone.conf"
    state = args.state_dir or args.home / ".local/state/daily-backup"
    try:
        if args.command == "install-root-helper":
            result = {"status": "DRY_RUN"}
            install_root_helper(args, runner, result,
                                args.execute and args.confirm and not args.dry_run)
            print(json.dumps(result, indent=2))
            return 0
        if args.command == "stage-protected":
            engine = DailyBackup(args.home, state, args.system_root, runner, None)
            result = ({"status": "DRY_RUN"} if args.dry_run else engine.stage_protected())
            print(json.dumps(result, indent=2))
            return 0
        with Rclone(runner, config) as rclone:
            engine = DailyBackup(args.home, state, args.system_root, runner, rclone)
            if args.command == "retire-legacy-backup":
                result = engine.retire_legacy_backup(args.execute, args.confirm, args.dry_run)
            elif args.dry_run:
                result = {"status": "DRY_RUN", "command": args.command,
                          "repository": RESTIC_REPOSITORY, "stage": str(engine.stage),
                          "browsable_roots": list(BROWSABLE_ROOTS)}
            else:
                result = getattr(engine, args.command.replace("-", "_"))()
            print(json.dumps(result, indent=2))
            if args.command == "verify" and not args.dry_run:
                return 0 if all(result[tier]["status"] == "VERIFIED"
                                for tier in ("Snapshot Backup", "Browsable Sync")) else 1
            return 0
    except (BackupError, GateError, OSError, ValueError, TypeError, KeyError, AttributeError,
            subprocess.CalledProcessError) as error:
        # Subprocess output may contain credentials; do not echo it.
        message = "External backup command failed" if isinstance(error, subprocess.CalledProcessError) else str(error)
        print(f"daily-backup: {message}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
