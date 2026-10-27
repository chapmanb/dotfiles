"""Daily encrypted snapshots, browsable document sync, and verified retirement."""

import argparse
from datetime import date, datetime, timedelta, timezone
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import stat
import subprocess
import sys
import tempfile

from .config import BACKUP_REMOTE, CUTOFF_TS, excluded, rclone_exclusions
from .cold_archive import git_metadata, write_private
from .runner import CommandRunner, Rclone


RESTIC_REPOSITORY = f"rclone:{BACKUP_REMOTE}/restic-repo"
BROWSABLE_REMOTE = f"{BACKUP_REMOTE}/browsable"
TRASH_REMOTE = f"{BACKUP_REMOTE}/browsable-trash"
LEGACY_REMOTE = "gdrive:bchapman-ubnt-backup"
BROWSABLE_ROOTS = ("personal", "Documents", "ginkgo/analyses", "ginkgo/methods", "ginkgo/notes")
SNAPSHOT_ROOTS = (
    ".dotfiles", ".ssh", ".gnupg", ".password-store", ".aws", ".kube", ".vpn",
    ".snowflake", ".dbt", ".pypirc", ".bashrc.local", ".bashrc.ginkgo", "secrets.env",
    "todo.txt", "notes.txt", ".spacemacs.env", ".agents", ".claude",
    ".claude.json", ".codex", ".pi/agent", ".gemini", ".config/herdr", ".config/nvim",
    ".config/msmtp", ".config/gspread", ".config/rclone", ".config/autostart",
)
EXCLUDED_CODE_REPOS = frozenset({"acs-mirror-local", "hamilton-ultimate"})
PRIVATE_EXCLUSIONS = (
    ".dotfiles/tmp", ".codex/packages", ".pi/agent/npm", ".pi/agent/git",
    ".pi/agent/sessions", ".pi/agent/web-search-cache", ".cache", ".local/share/pnpm",
    ".bun", ".nvm", "bio", "ginkgo/guild",
    "ginkgo/code/acs-mirror-local", "ginkgo/code/hamilton-ultimate",
)


class BackupError(Exception):
    pass


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
                for name in ("ginkgo-corp-profile.xml", "AnyConnectLocalPolicy.xml",
                             "vpnagentd", "vpnui", "acwebhelper"):
                    if not any(path.is_file() for path in staged_client.rglob(name)):
                        raise BackupError(f"Required Secure Client asset missing or unreadable: {name}")
            if self.stage.is_symlink():
                raise BackupError("Corporate staging directory must not be a symlink")
            if self.stage.exists():
                shutil.rmtree(self.stage)
            bundle.rename(self.stage)
        return {"stage": str(self.stage), "status": "STAGED"}

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

    def _require_browsable_roots(self):
        for relative in BROWSABLE_ROOTS:
            path = self.home / relative
            if not path.is_dir() or path.is_symlink():
                raise BackupError(f"Browsable source must be a real directory: {path}")

    def run(self):
        self._write_report(self._empty_report())
        self._require_password()
        self._require_browsable_roots()
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
            self._restic("check")
            snapshots = json.loads(self._restic("snapshots", "--json").stdout)
            if not isinstance(snapshots, list) or not snapshots or not all(
                isinstance(snapshot, dict) and snapshot.get("id") for snapshot in snapshots
            ):
                raise BackupError("No valid restic snapshots")
            report["Snapshot Backup"] = {"status": "VERIFIED", "snapshot_count": len(snapshots)}
        except (BackupError, OSError, ValueError, subprocess.CalledProcessError):
            report["Snapshot Backup"]["error"] = "Restic check or snapshot presence failed"
        roots = report["Browsable Sync"]["roots"]
        for relative in BROWSABLE_ROOTS:
            result = {"source": str(self.home / relative), "remote": f"{BROWSABLE_REMOTE}/{relative}",
                      "status": "UNVERIFIED"}
            try:
                source = self.home / relative
                if not source.is_dir() or source.is_symlink():
                    raise BackupError("Missing browsable source")
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
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--home", type=Path, default=Path.home())
    parser.add_argument("--state-dir", type=Path, help="Writable private staging/report directory")
    parser.add_argument("--system-root", type=Path, default=Path("/"))
    parser.add_argument("--rclone-config", type=Path)
    parser.add_argument("--dry-run", action="store_true")
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("stage-corp-stack", "init", "run", "verify", "retire-legacy-backup"):
        command = commands.add_parser(name)
        command.add_argument("--dry-run", action="store_true", default=argparse.SUPPRESS)
        if name == "retire-legacy-backup":
            command.add_argument("--execute", action="store_true")
            command.add_argument("--confirm", action="store_true")
    args = parser.parse_args(argv)
    runner = runner or CommandRunner()
    config = args.rclone_config or args.home / ".config/rclone/rclone.conf"
    state = args.state_dir or args.home / ".local/state/daily-backup"
    try:
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
    except (BackupError, OSError, ValueError, TypeError, KeyError, AttributeError,
            subprocess.CalledProcessError) as error:
        # Subprocess output may contain credentials; do not echo it.
        message = "External backup command failed" if isinstance(error, subprocess.CalledProcessError) else str(error)
        print(f"daily-backup: {message}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
