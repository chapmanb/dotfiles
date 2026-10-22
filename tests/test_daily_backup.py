import contextlib
from datetime import date, timedelta
import errno
import fnmatch
import io
import json
import os
from pathlib import Path
import shlex
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))

from dotfiles_migrate.daily_backup import main


# Independent expectations: policy changes must not silently change acceptance.
REPOSITORY = "rclone:gdrive:bchapman-backup/restic-repo"
BROWSABLE = "gdrive:bchapman-backup/browsable"
TRASH = "gdrive:bchapman-backup/browsable-trash"
LEGACY = "gdrive:bchapman-ubnt-backup"
ROOTS = ("personal", "Documents", "ginkgo/analyses", "ginkgo/methods", "ginkgo/notes")
SNAPSHOT_PATHS = (
    ".dotfiles", ".ssh", ".gnupg", ".password-store", ".aws", ".kube", ".vpn",
    ".snowflake", ".dbt", ".pypirc", ".bashrc.local", ".bashrc.ginkgo", "secrets.env",
    "todo.txt", "notes.txt", ".wezterm.lua", ".spacemacs.env", ".agents", ".claude",
    ".claude.json", ".codex", ".pi/agent", ".gemini", ".config/herdr", ".config/nvim",
    ".config/msmtp", ".config/gspread", ".config/rclone", ".config/autostart",
)
ARTIFACTS = (
    ".venv", "venv", ".pixi", "node_modules", "target", "__pycache__", ".mypy_cache",
    ".pytest_cache", ".ruff_cache", ".next", ".tox", ".nox", "data-frames",
)
OLD = 1767225600 - 86400
NEW = 1767225600 + 86400


class OfflineRunner:
    """Record CLI effects; reject unexpected commands and never use the network."""

    def __init__(self):
        self.calls = []
        self.git = {}
        self.remote = {}
        self.snapshots = []
        self.trash_listing = []
        self.snapshot_files = set()
        self.exclusion_text = ""
        self.password_observations = []
        self.config_observations = []
        self.fail_check = None
        self.fail_restic = None
        self.fail_pass = False
        self.fail_listing = False
        self.initialized = False

    def run(self, args):
        args = list(map(str, args))
        self.calls.append(args)
        output = ""
        if args[0] == "pass":
            if self.fail_pass:
                raise subprocess.CalledProcessError(1, args, stderr="private pass failure")
            if args != ["pass", "show", "backup/restic-gdrive"]:
                raise AssertionError(args)
            output = "test-only-password\nadditional metadata\n"
        elif args[0] == "git":
            data = self.git[args[2]]
            output = {"rev-parse": "a" * 40, "log": str(data),
                      "config": "git@example.org:project.git"}[args[3]]
        elif args[0] == "restic":
            self.assert_restic_credentials(args)
            operation_index = args.index("-o") + 2
            operation = args[operation_index]
            if self.fail_restic == operation:
                raise subprocess.CalledProcessError(1, args, stderr="private restic failure")
            if operation == "init":
                if self.initialized:
                    raise subprocess.CalledProcessError(1, args)
                self.initialized = True
            elif operation == "cat":
                if args[operation_index + 1:] != ["config"] or not self.initialized:
                    raise subprocess.CalledProcessError(1, args)
                output = '{"version": 2}'
            elif operation == "backup":
                exclusion_file = Path(args[args.index("--exclude-file") + 1])
                self.exclusion_text = exclusion_file.read_text()
                self.exclusion_mode = stat.S_IMODE(exclusion_file.stat().st_mode)
                sources = args[args.index("--exclude-file") + 2:]
                excluded = self.exclusion_text.splitlines()
                for source in sources:
                    source = Path(source)
                    candidates = source.rglob("*") if source.is_dir() else [source]
                    for path in candidates:
                        # Fixtures have literal paths. Production escapes glob
                        # metacharacters, exercised separately below.
                        if not any(str(path) == pattern or str(path).startswith(pattern + "/")
                                   for pattern in excluded):
                            self.snapshot_files.add(str(path))
                self.snapshots = [{"id": "b" * 64}]
            elif operation == "snapshots":
                if args[operation_index + 1:] != ["--json"]:
                    raise AssertionError(args)
                output = json.dumps(self.snapshots)
            elif operation not in ("check", "forget"):
                raise AssertionError(args)
        elif args[0] == "rclone":
            config = Path(args[args.index("--config") + 1])
            self.config_observations.append((config, stat.S_IMODE(config.stat().st_mode), config.read_text()))
            operation = args[3]
            if operation == "version":
                output = "rclone test"
            elif operation == "sync":
                source, destination = args[4:6]
                current = self.files(Path(source), args)
                previous = self.remote.get(destination, {})
                trash = args[args.index("--backup-dir") + 1]
                for name, content in previous.items():
                    if current.get(name) != content:
                        self.remote.setdefault(trash, {})[name] = content
                self.remote[destination] = current
            elif operation == "check":
                source, destination = args[4:6]
                expected = self.files(Path(source), args)
                actual = self.remote.get(destination)
                if self.fail_check == source or actual is None or any(
                    actual.get(name) != content for name, content in expected.items()
                ):
                    raise subprocess.CalledProcessError(1, args, stderr="private remote failure")
            elif operation == "lsjson":
                if self.fail_listing:
                    raise subprocess.CalledProcessError(3, args)
                output = json.dumps(self.trash_listing)
            elif operation == "purge":
                self.remote.pop(args[4], None)
            else:
                raise AssertionError(args)
        else:
            raise AssertionError(args)
        return subprocess.CompletedProcess(args, 0, output, "")

    def files(self, root, args):
        patterns = [args[index + 1] for index, arg in enumerate(args) if arg == "--exclude"]
        result = {}
        for path in root.rglob("*"):
            name = path.relative_to(root).as_posix()
            if any(fnmatch.fnmatchcase(name, pattern) or fnmatch.fnmatchcase(name, "*/" + pattern)
                   for pattern in patterns):
                continue
            if path.is_symlink():
                if "--links" in args:
                    result[name + ".rclonelink"] = os.readlink(path).encode()
            elif path.is_file():
                result[name] = path.read_bytes()
        return result

    def assert_restic_credentials(self, args):
        if args[args.index("-r") + 1] != REPOSITORY:
            raise AssertionError(args)
        password = Path(args[args.index("--password-file") + 1])
        self.password_observations.append((stat.S_IMODE(password.stat().st_mode), password.read_text()))
        option = args[args.index("-o") + 1]
        if not option.startswith("rclone.args="):
            raise AssertionError(option)
        serve = shlex.split(option[len("rclone.args="):])
        if serve[:3] != ["serve", "restic", "--stdio"]:
            raise AssertionError(serve)
        config = Path(serve[serve.index("--config") + 1])
        if not config.is_file():
            raise AssertionError("restic rclone config missing")


class TestDailyBackup(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="test-daily-backup-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.home = self.root / "home"
        self.state = self.root / "state"
        self.system = self.root / "system"
        self.stage = self.state / "corporate-access-stack"
        self.report = self.state / "verification.json"
        self.password = self.home / ".config/restic/password"
        self.config = self.home / ".config/rclone/rclone.conf"
        self.file(self.config, "[gdrive]\ntoken = test-only-token\n")
        self.config.chmod(0o600)
        for relative in ROOTS:
            self.file(self.home / relative / "document.txt", relative)
        self.file(self.system / "usr/local/share/ca-certificates/Ginkgo_Root.crt", "certificate")
        self.file(self.system / "usr/local/share/ca-certificates/trust_custom.crt", "trust")
        self.file(self.system / "usr/local/share/ca-certificates/ignore.txt", "not a certificate")
        self.file(self.system / "etc/ca-certificates.conf", "!mozilla/eMudhra.crt\n")
        for relative in ("vpn/profile/ginkgo-corp-profile.xml", "vpn/AnyConnectLocalPolicy.xml",
                         "bin/vpnagentd", "bin/vpnui", "bin/acwebhelper", "lib/libvpn.so.1"):
            self.file(self.system / "opt/cisco/secureclient" / relative, relative)
        (self.system / "opt/cisco/secureclient/lib/libvpn.so").symlink_to("libvpn.so.1")
        self.runner = OfflineRunner()

    def file(self, path, content="payload", timestamp=OLD):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        os.utime(path, (timestamp, timestamp))
        return path

    def cli(self, *args):
        stdout, stderr = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            status = main([
                "--home", str(self.home), "--state-dir", str(self.state),
                "--system-root", str(self.system), "--rclone-config", str(self.config),
                *map(str, args),
            ], runner=self.runner)
        return status, stdout.getvalue(), stderr.getvalue()

    def ok(self, *args):
        status, stdout, stderr = self.cli(*args)
        self.assertEqual(status, 0, stderr)
        return json.loads(stdout)

    def restic_calls(self, operation):
        return [call for call in self.runner.calls if call[0] == "restic"
                and call[call.index("-o") + 2] == operation]

    def rclone_calls(self, operation):
        return [call for call in self.runner.calls if call[0] == "rclone" and call[3] == operation]

    def test_stage_corp_stack_and_restic_init(self):
        result = self.ok("stage-corp-stack")
        self.assertEqual(result, {"status": "STAGED", "stage": str(self.stage)})
        self.assertEqual(stat.S_IMODE(self.stage.stat().st_mode), 0o700)
        self.assertEqual(stat.S_IMODE(self.state.stat().st_mode), 0o700)
        for source in self.system.rglob("*"):
            if source.is_file() and source.suffix != ".txt":
                destination = self.stage / source.relative_to(self.system)
                self.assertEqual(destination.read_bytes(), source.read_bytes())
        self.assertFalse((self.stage / "usr/local/share/ca-certificates/ignore.txt").exists())
        self.assertTrue((self.stage / "opt/cisco/secureclient/lib/libvpn.so").is_symlink())
        self.file(self.system / "etc/ca-certificates.conf", "changed")
        self.ok("stage-corp-stack")
        self.assertEqual((self.stage / "etc/ca-certificates.conf").read_text(), "changed")

        self.ok("init")
        self.assertEqual(self.password.read_text(), "test-only-password\n")
        self.assertEqual(stat.S_IMODE(self.password.stat().st_mode), 0o600)
        self.assertIn(["pass", "show", "backup/restic-gdrive"], self.runner.calls)
        init = self.restic_calls("init")
        self.assertEqual(len(init), 1)
        self.assertEqual(init[0][-3:], ["init", "--repository-version", "2"])
        self.assertEqual(self.runner.password_observations, [(0o600, "test-only-password\n")])
        self.ok("init")
        self.assertEqual(len(self.restic_calls("cat")), 1)
        self.assertEqual(self.runner.calls.count(["pass", "show", "backup/restic-gdrive"]), 1)

    def test_stage_corp_stack_nixos_fallback(self):
        certs = self.system / "usr/local/share/ca-certificates"
        config = self.system / "etc/ca-certificates.conf"
        client = self.system / "opt/cisco/secureclient"
        self.ok("init")
        for missing in ((certs,), (config,), (certs, config)):
            with self.subTest(missing=missing):
                for path in missing:
                    path.rename(path.with_name(path.name + "-saved"))
                try:
                    self.assertEqual(self.ok("stage-corp-stack")["status"], "STAGED")
                    self.assertEqual(stat.S_IMODE(self.stage.stat().st_mode), 0o700)
                    for path in (certs, config):
                        staged = self.stage / path.relative_to(self.system)
                        self.assertEqual(staged.exists(), path not in missing)
                    for source in client.rglob("*"):
                        if source.is_file():
                            staged = self.stage / source.relative_to(self.system)
                            self.assertEqual(staged.read_bytes(), source.read_bytes())
                    self.assertTrue((self.stage / "opt/cisco/secureclient/lib/libvpn.so").is_symlink())
                    self.assertEqual(self.ok("run")["status"], "BACKED_UP")
                    self.assertIn(str(self.stage / "opt/cisco/secureclient/bin/vpnui"),
                                  self.runner.snapshot_files)
                    required = client / "bin/vpnui"
                    required.unlink()
                    try:
                        self.assertNotEqual(self.cli("stage-corp-stack")[0], 0)
                        self.assertEqual((self.stage / "opt/cisco/secureclient/bin/vpnui").read_text(),
                                         "bin/vpnui")
                    finally:
                        self.file(required, "bin/vpnui")
                finally:
                    for path in missing:
                        path.with_name(path.name + "-saved").rename(path)
        self.assertEqual(len(self.restic_calls("backup")), 3)
        self.assertEqual(len(self.rclone_calls("sync")), 15)

        # A declarative-only installation may have no locally installed client.
        shutil.rmtree(certs)
        config.unlink()
        shutil.rmtree(client)
        self.ok("stage-corp-stack")
        self.assertEqual(list(self.stage.iterdir()), [])  # Do not keep stale assets.
        self.assertEqual(self.ok("run")["status"], "BACKED_UP")

    def test_stage_skips_unreadable_runtime_and_optional_assets(self):
        client = self.system / "opt/cisco/secureclient"
        runtime = client / "temp/downloader"
        private = client / "lib/private"
        for directory in (runtime, private):
            self.file(directory / "unreadable")
            directory.chmod(0)
            self.addCleanup(directory.chmod, 0o700)
        original_scandir = os.scandir
        original_access = os.access

        def scandir(path):
            if not isinstance(path, int) and Path(path) in (runtime, private):
                raise PermissionError(errno.EACCES, "Permission denied", str(path))
            return original_scandir(path)

        def access(path, mode):
            return False if Path(path) == private else original_access(path, mode)

        with patch("os.scandir", side_effect=scandir), patch("os.access", side_effect=access):
            self.ok("stage-corp-stack")
            self.ok("init")
            self.ok("run")
        staged_client = self.stage / "opt/cisco/secureclient"
        self.assertFalse((staged_client / "temp").exists())
        self.assertFalse((staged_client / "lib/private").exists())
        for relative in ("vpn/profile/ginkgo-corp-profile.xml", "vpn/AnyConnectLocalPolicy.xml",
                         "bin/vpnagentd", "bin/vpnui", "bin/acwebhelper", "lib/libvpn.so.1"):
            self.assertEqual((staged_client / relative).read_bytes(), (client / relative).read_bytes())
        self.assertEqual(stat.S_IMODE(self.stage.stat().st_mode), 0o700)
        self.assertEqual(len(self.restic_calls("backup")), 1)

    def test_stage_refuses_unreadable_required_asset_without_replacing_previous_stage(self):
        self.ok("stage-corp-stack")
        required = self.system / "opt/cisco/secureclient/bin/vpnui"
        original_access = os.access

        def access(path, mode):
            return False if Path(path) == required else original_access(path, mode)

        with patch("os.access", side_effect=access):
            self.assertNotEqual(self.cli("stage-corp-stack")[0], 0)
        self.assertEqual((self.stage / "opt/cisco/secureclient/bin/vpnui").read_bytes(),
                         required.read_bytes())

    def test_run_handles_real_unborn_git_head(self):
        self.ok("init")
        project = self.home / "ginkgo/code/unborn"
        self.file(project / "main.py", timestamp=NEW)
        # Minimal real repository with no commits; no Git mutation commands.
        self.file(project / ".git/HEAD", "ref: refs/heads/main\n")
        (project / ".git/objects").mkdir()
        (project / ".git/refs/heads").mkdir(parents=True)
        offline_run = self.runner.run

        def run(args):
            if args[0] == "git":
                self.runner.calls.append(list(args))
                return subprocess.run(args, check=True, text=True, capture_output=True)
            return offline_run(args)

        with patch.object(self.runner, "run", side_effect=run):
            self.ok("run")
        backup = self.restic_calls("backup")[0]
        self.assertIn(str(project), backup[backup.index("--exclude-file") + 2:])
        self.assertIn(str(project / ".git/HEAD"), self.runner.snapshot_files)
        self.assertIn(["git", "-C", str(project), "rev-parse", "HEAD"], self.runner.calls)
        self.assertEqual(len(self.restic_calls("forget")), 1)

    def test_run_tolerates_unreadable_project_subdirectories(self):
        self.ok("init")
        code = self.home / "ginkgo/code"
        active = code / "active"
        uncertain = code / "uncertain"
        self.file(active / "main.py", timestamp=NEW)
        self.file(uncertain / "main.py", timestamp=OLD)
        self.file(code / "inactive/main.py", timestamp=OLD)
        blocked = [active / "docker-data", uncertain / ".cache/snowflake"]
        for directory in blocked:
            self.file(directory / "unreadable")
            directory.chmod(0)
            self.addCleanup(directory.chmod, 0o700)
        self.file(active / "target/junk")
        original_scandir = os.scandir

        def scandir(path):
            if not isinstance(path, int) and Path(path) in blocked:
                raise PermissionError(errno.EACCES, "Permission denied", str(path))
            return original_scandir(path)

        with patch("os.scandir", side_effect=scandir):
            status, stdout, stderr = self.cli("run")
        self.assertEqual(status, 0, stderr)
        self.assertEqual(json.loads(stdout)["status"], "BACKED_UP")
        self.assertIn("Permission denied", stderr)
        backup = self.restic_calls("backup")[0]
        sources = backup[backup.index("--exclude-file") + 2:]
        self.assertIn(str(active), sources)
        self.assertIn(str(uncertain), sources)  # Unknown activity must not omit a project.
        self.assertNotIn(str(code / "inactive"), sources)
        self.assertIn(str(active / "main.py"), self.runner.snapshot_files)
        self.assertIn(str(active / "target"), self.runner.exclusion_text.splitlines())
        self.assertFalse(any(str(directory) in self.runner.exclusion_text for directory in blocked))
        self.assertEqual(len(self.restic_calls("forget")), 1)
        self.assertEqual(len(self.rclone_calls("sync")), 5)

    def test_run_tolerates_unreadable_project_entry_metadata(self):
        self.ok("init")
        project = self.home / "ginkgo/code/active"
        self.file(project / "main.py", timestamp=NEW)
        blocked = project / "docker-data"
        self.file(blocked / "file")
        original_lstat = Path.lstat

        def lstat(path, *args, **kwargs):
            if path == blocked:
                raise PermissionError(errno.EACCES, "Permission denied", str(path))
            return original_lstat(path, *args, **kwargs)

        with patch.object(Path, "lstat", lstat):
            status, stdout, stderr = self.cli("run")
        self.assertEqual(status, 0, stderr)
        self.assertIn("Permission denied", stderr)
        self.assertIn(str(project), json.loads(stdout)["sources"])
        self.assertNotIn(str(blocked), self.runner.exclusion_text.splitlines())
        self.assertEqual(len(self.restic_calls("backup")), 1)

    def test_run_executes_restic_snapshot_with_exclusions_and_retention(self):
        self.ok("init")
        for relative in SNAPSHOT_PATHS:
            path = self.home / relative
            if path.exists() or path.is_symlink():
                continue
            if relative in (".pypirc", ".bashrc.local", ".bashrc.ginkgo", "secrets.env", "todo.txt",
                            "notes.txt", ".wezterm.lua", ".spacemacs.env", ".claude.json"):
                self.file(path)
            else:
                self.file(path / "config")
        code = self.home / "ginkgo/code"
        active = code / "active"
        self.file(active / "main.py", timestamp=NEW)
        self.file(active / ".git/HEAD", "ref: refs/heads/main")
        self.file(active / ".git/refs/heads/target", "must survive")
        self.file(active / ".git/objects/venv/blob", "must survive")
        self.runner.git[str(active)] = OLD
        for artifact in ARTIFACTS:
            self.file(active / artifact / "junk", timestamp=NEW + 100)
        self.file(code / "inactive/main.py")
        self.file(code / "inactive/.venv/new-cache", timestamp=NEW)
        self.file(code / "commit-active/main.py")
        self.file(code / "commit-active/.git/HEAD")
        self.runner.git[str(code / "commit-active")] = NEW
        self.file(code / "agent-plans/old-plan/spec.md")
        self.file(code / "agent-plans/deep/plan/scratch/worktree/source.py")
        self.file(code / "agent-plans/deep/plan/repos/clone/.git/HEAD")
        for relative in (".dotfiles/tmp", ".codex/packages", ".pi/agent/npm", ".pi/agent/git",
                         ".pi/agent/sessions", ".pi/agent/web-search-cache", "bio", "ginkgo/guild"):
            self.file(self.home / relative / "junk")
        self.file(self.home / ".claude.backup")

        result = self.ok("run")
        self.assertEqual(result["status"], "BACKED_UP")
        backup = self.restic_calls("backup")
        self.assertEqual(len(backup), 1)
        call = backup[0]
        sources = set(call[call.index("--exclude-file") + 2:])
        expected = {str(self.home / path) for path in SNAPSHOT_PATHS}
        expected.update(map(str, [active, code / "commit-active", code / "agent-plans", self.stage,
                                  self.home / ".claude.backup"]))
        self.assertEqual(sources, expected)
        self.assertIn("auto", call)
        self.assertEqual(self.runner.exclusion_mode, 0o600)
        for path in (active / ".git/HEAD", active / ".git/refs/heads/target",
                     active / ".git/objects/venv/blob", code / "agent-plans/old-plan/spec.md",
                     self.home / ".ssh/config", self.stage / "etc/ca-certificates.conf"):
            self.assertIn(str(path), self.runner.snapshot_files)
        for artifact in ARTIFACTS:
            self.assertNotIn(str(active / artifact / "junk"), self.runner.snapshot_files)
            self.assertIn(str(active / artifact), self.runner.exclusion_text.splitlines())
        for relative in (".dotfiles/tmp", ".codex/packages", ".pi/agent/npm", ".pi/agent/git",
                         ".pi/agent/sessions", ".pi/agent/web-search-cache", "bio", "ginkgo/guild",
                         "ginkgo/code/agent-plans/deep/plan/scratch",
                         "ginkgo/code/agent-plans/deep/plan/repos"):
            self.assertIn(str(self.home / relative), self.runner.exclusion_text.splitlines())
            self.assertFalse(any(path.startswith(str(self.home / relative) + "/")
                                 for path in self.runner.snapshot_files))
        forget = self.restic_calls("forget")
        self.assertEqual(len(forget), 1)
        self.assertEqual(forget[0][-8:],
                         ["forget", "--keep-daily", "7", "--keep-weekly", "4", "--keep-monthly", "6", "--prune"])
        self.assertLess(self.runner.calls.index(backup[0]), self.runner.calls.index(forget[0]))
        report = json.loads(self.report.read_text())
        self.assertEqual(report["Snapshot Backup"]["status"], "UNVERIFIED")

    def test_run_executes_browsable_sync_with_dated_trash_rollover(self):
        self.ok("init")
        for relative in ROOTS:
            self.runner.remote[f"{BROWSABLE}/{relative}"] = {
                "document.txt": b"older version", "deleted.txt": b"deleted version",
            }
        self.file(self.home / "bio/upstream.txt")
        self.file(self.home / "ginkgo/guild/retired.txt")
        self.file(self.home / "ginkgo/code/agent-plans/spec.md")
        today = date.today()
        self.runner.trash_listing = [
            {"Name": "2000-01-01", "IsDir": True},
            {"Name": today.isoformat(), "IsDir": True},
            {"Name": (today - timedelta(days=30)).isoformat(), "IsDir": True},
            {"Name": (today - timedelta(days=31)).isoformat(), "IsDir": True},
            {"Name": "1998-01-01", "IsDir": False},
            {"Name": "1999-99-99", "IsDir": True},
            {"Name": "2000-01-01/../../browsable", "IsDir": True},
            {"Name": "operator-notes", "IsDir": True},
        ]
        self.ok("run")
        syncs = self.rclone_calls("sync")
        self.assertEqual(len(syncs), 5)
        self.assertEqual({call[4] for call in syncs}, {str(self.home / path) for path in ROOTS})
        for relative in ROOTS:
            call = next(call for call in syncs if call[4] == str(self.home / relative))
            self.assertEqual(call[5], f"{BROWSABLE}/{relative}")
            trash = f"{TRASH}/{today.isoformat()}/{relative}"
            self.assertEqual(call[call.index("--backup-dir") + 1], trash)
            self.assertEqual(self.runner.remote[trash], {
                "document.txt": b"older version", "deleted.txt": b"deleted version",
            })
            self.assertEqual(self.runner.remote[f"{BROWSABLE}/{relative}"], {"document.txt": relative.encode()})
        self.assertEqual([call[4] for call in self.rclone_calls("purge")], [
            f"{TRASH}/2000-01-01", f"{TRASH}/{(today - timedelta(days=31)).isoformat()}",
        ])
        self.runner.fail_listing = True
        self.ok("run")  # A first-run missing trash directory is not a backup failure.

    def test_verify_and_retire_legacy_backup_gate(self):
        self.assertNotEqual(self.cli("retire-legacy-backup")[0], 0)
        self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
        self.assertEqual(self.rclone_calls("purge"), [])
        self.ok("init")
        self.ok("run")
        self.runner.calls.clear()
        report = self.ok("verify")
        self.assertEqual(json.loads(self.report.read_text()), report)
        self.assertEqual(stat.S_IMODE(self.report.stat().st_mode), 0o600)
        for tier in ("Snapshot Backup", "Browsable Sync"):
            self.assertEqual(report[tier]["status"], "VERIFIED")
        self.assertEqual(len(self.restic_calls("check")), 1)
        self.assertEqual(len(self.restic_calls("snapshots")), 1)
        checks = self.rclone_calls("check")
        self.assertEqual(len(checks), 5)
        for relative in ROOTS:
            call = next(call for call in checks if call[4] == str(self.home / relative))
            self.assertEqual(call[5], f"{BROWSABLE}/{relative}")
            self.assertIn("--one-way", call)
        for flags in ((), ("--execute",), ("--confirm",)):
            self.assertNotEqual(self.cli("retire-legacy-backup", *flags)[0], 0)
        self.ok("retire-legacy-backup", "--execute", "--confirm", "--dry-run")
        self.assertEqual(self.rclone_calls("purge"), [])

        for tier in ("Snapshot Backup", "Browsable Sync"):
            invalid = json.loads(json.dumps(report))
            invalid[tier]["status"] = "UNVERIFIED"
            self.report.write_text(json.dumps(invalid))
            self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
        self.report.write_text("malformed JSON")
        self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
        self.report.write_text(json.dumps(report))
        self.ok("retire-legacy-backup", "--execute", "--confirm")
        self.assertEqual([call[4] for call in self.rclone_calls("purge")], [LEGACY])

    def test_failed_verification_revokes_previous_success_and_blocks_retirement(self):
        self.ok("init")
        self.ok("run")
        self.ok("verify")
        for failure in ("check", "empty-snapshots", "browsable", "missing-local-root"):
            with self.subTest(failure=failure):
                self.runner.fail_restic = "check" if failure == "check" else None
                self.runner.snapshots = [] if failure == "empty-snapshots" else [{"id": "a" * 64}]
                self.runner.fail_check = str(self.home / "personal") if failure == "browsable" else None
                missing = self.home / "Documents"
                if failure == "missing-local-root":
                    missing.rename(self.home / "Documents-away")
                try:
                    status, stdout, stderr = self.cli("verify")
                    self.assertEqual(status, 1, stderr)
                    self.assertNotIn("private", stdout + stderr)
                    report = json.loads(self.report.read_text())
                    tier = "Snapshot Backup" if failure in ("check", "empty-snapshots") else "Browsable Sync"
                    self.assertEqual(report[tier]["status"], "UNVERIFIED")
                    self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
                finally:
                    if failure == "missing-local-root":
                        (self.home / "Documents-away").rename(missing)
        self.assertEqual(self.rclone_calls("purge"), [])

    def test_readonly_rclone_config_staging_is_shared_with_restic(self):
        original = tempfile.TemporaryFile

        def readonly(*args, **kwargs):
            if Path(kwargs.get("dir", "/")) == self.config.parent:
                raise OSError(errno.EROFS, "Read-only filesystem")
            return original(*args, **kwargs)

        with patch("dotfiles_migrate.runner.tempfile.TemporaryFile", side_effect=readonly):
            self.ok("init")
        observed = self.runner.config_observations
        self.assertTrue(observed)
        staged_config, mode, content = observed[0]
        self.assertNotEqual(staged_config, self.config)
        self.assertEqual(mode, 0o600)
        self.assertEqual(content, self.config.read_text())
        self.assertFalse(staged_config.exists())
        restic = self.restic_calls("init")[0]
        self.assertIn(str(staged_config), restic[restic.index("-o") + 1])

    def test_dry_runs_do_not_write_or_invoke_external_commands(self):
        for command in ("stage-corp-stack", "init", "run", "verify"):
            self.assertEqual(self.ok(command, "--dry-run")["status"], "DRY_RUN")
        self.assertFalse(self.state.exists())
        self.assertFalse(self.password.exists())
        self.assertEqual(self.runner.calls, [])

    def test_stage_missing_assets_and_password_failures_stop_backup(self):
        (self.system / "opt/cisco/secureclient/bin/vpnui").unlink()
        self.assertNotEqual(self.cli("stage-corp-stack")[0], 0)
        self.assertFalse(self.stage.exists())
        self.assertNotEqual(self.cli("run")[0], 0)
        self.runner.fail_pass = True
        status, stdout, stderr = self.cli("init")
        self.assertEqual(status, 1)
        self.assertNotIn("private pass failure", stdout + stderr)
        self.assertFalse(self.password.exists())
        self.assertEqual(self.restic_calls("init"), [])
        self.file(self.password, "existing-password\n")
        self.password.chmod(0o644)
        self.assertNotEqual(self.cli("run")[0], 0)
        self.assertEqual(self.restic_calls("backup"), [])

    def test_missing_browsable_root_and_restic_failure_do_not_sync_or_prune(self):
        self.ok("init")
        self.ok("run")
        self.ok("verify")
        self.runner.calls.clear()
        (self.home / "Documents/document.txt").unlink()
        (self.home / "Documents").rmdir()
        self.assertNotEqual(self.cli("run")[0], 0)
        self.assertEqual(self.rclone_calls("sync"), [])
        self.assertEqual(json.loads(self.report.read_text())["Snapshot Backup"]["status"], "UNVERIFIED")
        self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
        self.file(self.home / "Documents/document.txt")
        self.runner.fail_restic = "backup"
        self.assertNotEqual(self.cli("run")[0], 0)
        self.assertEqual(self.restic_calls("forget"), [])
        self.assertEqual(self.rclone_calls("sync"), [])
        self.assertEqual(self.rclone_calls("purge"), [])

    def test_retirement_rejects_report_from_another_home_or_incomplete_roots(self):
        self.ok("init")
        self.ok("run")
        report = self.ok("verify")
        report["Browsable Sync"]["roots"].pop()
        self.report.write_text(json.dumps(report))
        self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
        self.assertEqual(self.rclone_calls("purge"), [])

    def test_exclusions_escape_literal_glob_characters(self):
        self.ok("init")
        self.file(self.home / "ginkgo/code/active/main.py", timestamp=NEW)
        self.file(self.home / "ginkgo/code/active/[important]/target/junk")
        self.ok("run")
        self.assertIn("/\\[important\\]/target", self.runner.exclusion_text)


if __name__ == "__main__":
    unittest.main()
