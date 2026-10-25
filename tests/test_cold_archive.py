import contextlib
import errno
import fnmatch
import io
import json
import os
from pathlib import Path
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))

from dotfiles_migrate import cold_archive
from dotfiles_migrate.cold_archive import main


# Keep acceptance expectations independent of the production policy catalog.
COLD_ARCHIVE_REMOTE = "gdrive:bchapman-backup/cold-archive"
CUTOFF_TS = 1767225600  # 2026-01-01T00:00:00Z
LEGACY_GINKGO_DIRS = (
    "projects", "ferment", "ghcode", "hts", "joyn", "omics", "results", "reviews", "servers",
)
REGENERABLE_NAMES = {
    ".venv", "venv", ".pixi", "node_modules", "target", "__pycache__", ".mypy_cache",
    ".pytest_cache", ".ruff_cache", ".next", ".tox", ".nox", "data-frames",
}
OLD = CUTOFF_TS - 86400


class OfflineRunner:
    """Model command effects, not engine internals; never contact a remote.

    Tar uses an uncompressed test container so the suite needs no zstd binary.
    A separate optional integration test exercises actual GNU tar/zstd commands.
    """

    def __init__(self):
        self.calls = []
        self.git = {}
        self.objects = {}
        self.directories = {}
        self.config_observations = []
        self.packed_members = {}
        self.corrupt_downloads = False
        self.fail_checks = False

    def run(self, args):
        args = [str(arg) for arg in args]
        self.calls.append(args)
        if args[0] == "git":
            data = self.git[args[2]]
            operation = args[3]
            if operation == "rev-parse" and data["head"] is None:
                raise subprocess.CalledProcessError(128, args, stderr="unborn HEAD")
            value = {"rev-parse": data["head"], "log": str(data["timestamp"]),
                     "config": data["remote"]}[operation]
            return subprocess.CompletedProcess(args, 0, value, "")
        if args[0] == "tar":
            return self.tar(args)
        if args[0] != "rclone":
            raise AssertionError(f"Unexpected command: {args}")
        config = Path(args[args.index("--config") + 1])
        self.config_observations.append((config, stat.S_IMODE(config.stat().st_mode), config.read_text()))
        operation, source = args[3:5]
        if operation == "copyto":
            destination = args[5]
            if source.startswith("gdrive:"):
                content = self.objects[source]
                Path(destination).write_bytes(content + (b"corrupted" if self.corrupt_downloads else b""))
            else:
                self.objects[destination] = Path(source).read_bytes()
            output = ""
        elif operation == "copy":
            destination = args[5]
            if source.startswith("gdrive:"):
                for name, content in self.directories[source].items():
                    if "--links" in args and name.endswith(".rclonelink"):
                        target = Path(destination) / name[:-len(".rclonelink")]
                        target.parent.mkdir(parents=True, exist_ok=True)
                        target.symlink_to(os.fsdecode(content))
                    else:
                        target = Path(destination) / name
                        target.parent.mkdir(parents=True, exist_ok=True)
                        target.write_bytes(content)
            else:
                self.directories[destination] = self.local_files(Path(source), args)
            output = ""
        elif operation == "check":
            expected = self.local_files(Path(source), args)
            actual = self.directories[args[5]]
            if self.fail_checks or any(actual.get(name) != content for name, content in expected.items()):
                raise subprocess.CalledProcessError(1, args, stderr="remote mismatch")
            output = ""
        elif operation == "lsjson":
            if source in self.objects:
                output = json.dumps({"Size": len(self.objects[source]), "IsDir": False})
            elif source in self.directories:
                output = json.dumps({"IsDir": True, "Size": -1} if "--stat" in args else [
                    {"Path": name, "Size": len(content), "IsDir": False}
                    for name, content in self.directories[source].items()
                ])
            else:
                raise subprocess.CalledProcessError(3, args, stderr="remote not found")
        else:
            raise AssertionError(f"Unexpected rclone operation: {args}")
        return subprocess.CompletedProcess(args, 0, output, "")

    def local_files(self, source, args):
        patterns = [args[index + 1] for index, arg in enumerate(args) if arg == "--exclude"]
        files = {}
        for path in source.rglob("*"):
            relative = path.relative_to(source).as_posix()
            if any(fnmatch.fnmatchcase(relative, pattern) or fnmatch.fnmatchcase(relative, f"*/{pattern}")
                   for pattern in patterns):
                continue
            if path.is_symlink():
                if "--links" in args:
                    files[f"{relative}.rclonelink"] = os.fsencode(os.readlink(path))
            elif path.is_file():
                files[relative] = path.read_bytes()
        return files

    def tar(self, args):
        destination = Path(args[args.index("-C") + 1])
        if "-cf" in args:
            archive = Path(args[args.index("-cf") + 1])
            names = Path(args[args.index("-T") + 1]).read_bytes().split(b"\0")[:-1]
            self.packed_members[destination.name] = [os.fsdecode(name) for name in names]
            with tarfile.open(archive, "w") as handle:
                for name in names:
                    name = os.fsdecode(name)
                    handle.add(destination / name, arcname=name, recursive=False)
        else:
            archive = Path(args[args.index("-xf") + 1])
            with tarfile.open(archive) as handle:
                if hasattr(tarfile, "data_filter"):
                    handle.extractall(destination, filter="data")
                else:
                    handle.extractall(destination)
        return subprocess.CompletedProcess(args, 0, "", "")


class RealPackagingRunner(OfflineRunner):
    def tar(self, args):
        return subprocess.run(args, check=True, capture_output=True, text=True)


class TestColdArchive(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="test-cold-archive-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.home = self.root / "home"
        self.code = self.home / "ginkgo/code"
        self.code.mkdir(parents=True)
        self.work = self.root / "mnt/work"
        self.receipt = self.root / "state/verified-archives.json"
        self.config = self.home / ".config/rclone/rclone.conf"
        self.config.parent.mkdir(parents=True)
        self.config.write_text("[gdrive]\ntoken = test-only-token\n")
        self.config.chmod(0o600)
        self.runner = OfflineRunner()

    def file(self, path, content="payload", timestamp=OLD):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        os.utime(path, (timestamp, timestamp))
        return path

    def project(self, name, timestamp=OLD, git=False):
        project = self.code / name
        self.file(project / "README.md", timestamp=timestamp)
        if git:
            self.file(project / ".git/HEAD", "ref: refs/heads/main\n")
            self.file(project / ".git/config", "test remote")
            self.runner.git[str(project)] = {
                "head": "a" * 40, "timestamp": OLD, "remote": "git@example.com:team/project.git",
            }
        return project

    def cli(self, *args):
        stdout, stderr = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            code = main([
                "--home", str(self.home), "--work-root", str(self.work),
                "--receipt", str(self.receipt), "--rclone-config", str(self.config),
                *map(str, args),
            ], runner=self.runner)
        return code, stdout.getvalue(), stderr.getvalue()

    def ok(self, *args):
        code, stdout, stderr = self.cli(*args)
        self.assertEqual(code, 0, stderr)
        return json.loads(stdout)

    def receipts(self):
        return json.loads(self.receipt.read_text())["items"]

    def test_scan_classifies_cutoff_legacy_roots_and_allowlist(self):
        self.project("old-repo", git=True)
        self.project("boundary-file", timestamp=CUTOFF_TS)
        self.project("boundary-commit", git=True)
        self.runner.git[str(self.code / "boundary-commit")]["timestamp"] = CUTOFF_TS
        self.project("old-commit-new-file", timestamp=CUTOFF_TS + 100, git=True)
        old_with_caches = self.project("old-with-caches")
        for name in REGENERABLE_NAMES:
            self.file(old_with_caches / name / "new.bin", timestamp=CUTOFF_TS + 100)
        self.project("pinned-reference", git=True)
        allowlist = self.file(self.root / "allowlist", "# local pins\npinned-reference # retained\n")
        for path in (self.home / "mail", self.work, *(
            self.home / "ginkgo" / name for name in LEGACY_GINKGO_DIRS
        )):
            self.file(path / "legacy.txt")
        self.file(self.home / "bio/upstream/README")
        self.file(self.home / "ginkgo/guild/retired.txt")
        items = self.ok("scan", "--allowlist", allowlist)
        by_name = {item["name"]: item for item in items}
        for name in ("old-repo", "old-with-caches"):
            self.assertEqual(by_name[name]["classification"], "Inactive Project")
            self.assertTrue(by_name[name]["archive"])
        for name in ("boundary-file", "boundary-commit", "old-commit-new-file", "pinned-reference"):
            self.assertEqual(by_name[name]["classification"], "Active Workspace")
            self.assertFalse(by_name[name]["archive"])
        self.assertTrue(by_name["pinned-reference"]["pinned"])
        self.assertGreater(by_name["old-with-caches"]["total_bytes"], by_name["old-with-caches"]["clean_bytes"])
        archived_sources = {item["source"] for item in items if item["archive"]}
        for path in (self.home / "mail", self.work, *(
            self.home / "ginkgo" / name for name in LEGACY_GINKGO_DIRS
        )):
            self.assertIn(str(path), archived_sources)
        self.assertFalse(any("/bio/" in item["source"] or item["source"].endswith("/guild") for item in items))
        self.assertFalse(self.receipt.exists())
        self.assertFalse(any(call[0] != "git" for call in self.runner.calls))

    def test_scan_tolerates_unborn_head_and_permission_denied(self):
        unborn = self.project("unborn", git=True)
        self.runner.git[str(unborn)]["head"] = None
        unreadable = self.project("unreadable")
        blocked = unreadable / ".cache/private"
        self.file(blocked / "important.txt")
        pinned = self.project("pinned-broken", git=True)
        healthy = self.project("healthy", git=True)
        allowlist = self.file(self.root / "allowlist", "pinned-broken\n")
        real_scandir = os.scandir
        real_stats = cold_archive.tree_stats
        real_metadata = cold_archive.git_metadata

        def scandir(path):
            if not isinstance(path, int) and Path(path) in (blocked, pinned):
                raise PermissionError(errno.EACCES, "Permission denied", str(path))
            return real_scandir(path)

        def stats(source):
            self.assertNotEqual(source, pinned, "allowlisted tree must not be traversed")
            return real_stats(source)

        def metadata(source, runner):
            self.assertNotEqual(source, pinned, "allowlisted Git metadata must not be read")
            return real_metadata(source, runner)

        with patch("os.scandir", side_effect=scandir), \
                patch.object(cold_archive, "tree_stats", side_effect=stats), \
                patch.object(cold_archive, "git_metadata", side_effect=metadata):
            code, output, warnings = self.cli("scan", "--allowlist", allowlist)
            self.assertEqual(code, 0, warnings)
            items = {item["name"]: item for item in json.loads(output)}
            self.assertEqual(set(items), {"unborn", "unreadable", "pinned-broken", "healthy"})
            self.assertIn("warning", warnings)
            self.assertIn(str(blocked), warnings)
            self.assertIn(str(unborn), warnings)
            for name in ("unborn", "unreadable", "pinned-broken"):
                self.assertFalse(items[name]["archive"])
                self.assertEqual(items[name]["classification"], "Active Workspace")
            self.assertIsNone(items["unborn"]["git_head"])
            self.assertGreaterEqual(items["unborn"]["activity_ts"], CUTOFF_TS)
            self.assertTrue(items["unreadable"]["scan_errors"])
            self.assertTrue(items["pinned-broken"]["pinned"])
            self.assertIsNone(items["pinned-broken"]["latest_mtime"])
            self.assertTrue(items["healthy"]["archive"])
            candidates = self.ok("upload-and-verify", "--dry-run", "--allowlist", allowlist)
            self.assertEqual([item["name"] for item in candidates], [healthy.name])
            for name in ("unborn", "unreadable", "pinned-broken"):
                code, _, error = self.cli("upload-and-verify", name, "--allowlist", allowlist)
                self.assertNotEqual(code, 0, error)
            self.assertFalse(self.receipt.exists())
            uploaded = self.ok("upload-and-verify", "--allowlist", allowlist)
            self.assertEqual([item["name"] for item in uploaded], [healthy.name])
            self.assertEqual(uploaded[0]["status"], "VERIFIED")
            self.assertTrue(all(path.exists() for path in (unborn, unreadable, pinned, healthy)))
        unborn_calls = [call for call in self.runner.calls if call[2] == str(unborn)]
        self.assertTrue(unborn_calls)
        self.assertTrue(all(call[3:] == ["rev-parse", "HEAD"] for call in unborn_calls))
        self.assertFalse(any(call[2] == str(pinned) for call in self.runner.calls))

    def test_unreadable_archived_tree_blocks_entire_prune_batch(self):
        unchanged = self.project("a-unchanged")
        unreadable = self.project("z-unreadable")
        blocked = unreadable / "private"
        self.file(blocked / "important.txt")
        self.ok("upload-and-verify")
        real_scandir = os.scandir

        def scandir(path):
            if not isinstance(path, int) and Path(path) == blocked:
                raise PermissionError(errno.EACCES, "Permission denied", str(path))
            return real_scandir(path)

        with patch("os.scandir", side_effect=scandir):
            code, _, error = self.cli("prune-local", "--execute", "--confirm")
        self.assertNotEqual(code, 0)
        self.assertIn("unreadable", error)
        self.assertTrue(unchanged.exists())
        self.assertTrue((blocked / "important.txt").exists())

    def test_scan_does_not_hide_other_io_or_git_errors(self):
        project = self.project("broken", git=True)
        real_scandir = os.scandir

        def scandir(path):
            if not isinstance(path, int) and Path(path) == project:
                raise OSError(errno.EIO, "I/O error", str(path))
            return real_scandir(path)

        with patch("os.scandir", side_effect=scandir):
            code, _, error = self.cli("scan")
        self.assertNotEqual(code, 0)
        self.assertIn("I/O error", error)
        with patch.object(self.runner, "run", side_effect=subprocess.CalledProcessError(1, "git")):
            code, _, error = self.cli("scan")
        self.assertNotEqual(code, 0)
        self.assertIn("exit status 1", error)
        self.assertTrue(project.exists())

    def test_upload_and_verify_hybrid_packaging_and_pibox_rclone_config(self):
        project = self.project("repo", git=True)
        secret = self.file(project / "private.txt", "permissions retained")
        secret.chmod(0o600)
        (project / "readme-link").symlink_to("README.md")
        os.utime(project / "readme-link", (OLD, OLD), follow_symlinks=False)
        self.file(project / "-odd\nname", "safe null-delimited name")
        self.file(project / ".git/refs/heads/target", "a" * 40)
        for name in REGENERABLE_NAMES:
            self.file(project / name / "skip", "cache", timestamp=CUTOFF_TS + 100)
        self.project("documents")
        many = self.code / "many-files"
        for index in range(501):
            self.file(many / f"{index}.txt", "x")
        boundary = self.code / "exactly-500"
        for index in range(500):
            self.file(boundary / f"{index}.txt", "x")
        for name in REGENERABLE_NAMES:
            self.file(boundary / name / "cache.bin", "excluded")
        self.file(self.home / "mail/inbox/message", "mail")
        self.assertEqual(len(self.ok("upload-and-verify", "--dry-run")), 5)
        self.assertFalse(self.receipt.exists())
        with patch("dotfiles_migrate.runner.tempfile.TemporaryFile", side_effect=OSError(errno.EROFS, "Read-only file system")):
            items = self.ok("upload-and-verify")
        by_name = {item["name"]: item for item in items}
        for name in ("repo", "many-files", "legacy-mail"):
            self.assertEqual(by_name[name]["mode"], "tar.zst")
            self.assertTrue(by_name[name]["remote"].endswith(".tar.zst"))
        for name in ("documents", "exactly-500"):
            self.assertEqual(by_name[name]["mode"], "rclone")
            checks = [call for call in self.runner.calls if call[0] == "rclone" and call[3] == "check" and call[4] == str(self.code / name)]
            self.assertEqual(len(checks), 1)
            self.assertIn("--one-way", checks[0])
            for excluded_name in REGENERABLE_NAMES:
                self.assertIn(f"{excluded_name}/**", checks[0])
        for item in items:
            self.assertEqual(item["status"], "VERIFIED")
            self.assertEqual(len(item["sha256"]), 64)
            self.assertTrue(item["remote"].startswith(COLD_ARCHIVE_REMOTE + "/"))
            self.assertIn("git_head", item)
            self.assertIn("git_remote", item)
            self.assertIn("latest_mtime", item)
        self.assertEqual(by_name["repo"]["git_head"], "a" * 40)
        self.assertEqual(by_name["repo"]["git_remote"], "git@example.com:team/project.git")
        members = self.runner.packed_members["repo"]
        self.assertIn(".git/HEAD", members)
        self.assertIn(".git/refs/heads/target", members)
        self.assertIn("readme-link", members)
        self.assertIn("-odd\nname", members)
        for name in members:
            if ".git" not in Path(name).parts:
                self.assertFalse(set(Path(name).parts) & REGENERABLE_NAMES)
        archive = self.root / "inspect.tar"
        archive.write_bytes(self.runner.objects[by_name["repo"]["remote"]])
        with tarfile.open(archive) as handle:
            self.assertEqual(handle.getmember("private.txt").mode, 0o600)
            self.assertTrue(handle.getmember("readme-link").issym())
        self.assertTrue(project.exists(), "upload must not delete sources")
        self.assertEqual(self.config.read_text(), "[gdrive]\ntoken = test-only-token\n")
        self.assertTrue(self.runner.config_observations)
        for staged, mode, content in self.runner.config_observations:
            self.assertTrue(str(staged).startswith("/tmp/cold-archive-rclone-"))
            self.assertNotEqual(staged, self.config)
            self.assertEqual(mode, 0o600)
            self.assertEqual(content, self.config.read_text())
            self.assertFalse(staged.exists(), "temporary credentials must be removed")
        self.assertEqual(self.receipts(), items)
        manifest = (self.receipt.parent / "MANIFEST.md").read_text()
        for key in ("VERIFIED", "sha256", "git_head", "git_remote", "latest_mtime"):
            self.assertIn(key, manifest)
        self.assertIn(f"{COLD_ARCHIVE_REMOTE}/MANIFEST.md", self.runner.objects)
        self.assertIn(f"{COLD_ARCHIVE_REMOTE}/verified-archives.json", self.runner.objects)

    def test_restore_extracts_tar_zst_and_rclone_entries(self):
        project = self.project("repo", git=True)
        (project / "link").symlink_to("README.md")
        os.utime(project / "link", (OLD, OLD), follow_symlinks=False)
        self.file(project / "private", "secret").chmod(0o600)
        docs = self.project("docs")
        (docs / "doc-link").symlink_to("README.md")
        os.utime(docs / "doc-link", (OLD, OLD), follow_symlinks=False)
        self.ok("upload-and-verify")
        tar_target = self.root / "restore/repo"
        docs_target = self.root / "restore/docs"
        self.ok("restore", "repo", tar_target)
        self.ok("restore", "docs", docs_target)
        self.assertEqual((tar_target / "README.md").read_text(), "payload")
        self.assertEqual((tar_target / ".git/HEAD").read_text(), "ref: refs/heads/main\n")
        self.assertTrue((tar_target / "link").is_symlink())
        self.assertEqual(stat.S_IMODE((tar_target / "private").stat().st_mode), 0o600)
        self.assertEqual((docs_target / "README.md").read_text(), "payload")
        self.assertTrue((docs_target / "doc-link").is_symlink())
        extract = [call for call in self.runner.calls if call[0] == "tar" and "-xf" in call]
        self.assertEqual(len(extract), 1)
        self.assertEqual(extract[0][1:3], ["-I", "zstd"])
        self.assertEqual(extract[0][-2:], ["-C", str(tar_target)])
        self.assertTrue(any(call[0] == "rclone" and call[3] == "copyto" and call[4].endswith("repo.tar.zst") for call in self.runner.calls))
        self.assertTrue(any(call[0] == "rclone" and call[3:6] == ["copy", f"{COLD_ARCHIVE_REMOTE}/docs", str(docs_target)] for call in self.runner.calls))
        code, _, error = self.cli("restore", "unknown", self.root / "absent")
        self.assertNotEqual(code, 0)
        self.assertIn("No VERIFIED receipt", error)
        code, _, error = self.cli("restore", "repo", tar_target)
        self.assertNotEqual(code, 0)
        self.assertIn("must be empty", error)

    def test_prune_local_enforces_receipt_and_drift_gates(self):
        project = self.project("repo", git=True)
        self.ok("upload-and-verify")
        original = self.receipt.read_text()
        remote = self.receipts()[0]["remote"]
        archive = self.runner.objects[remote]
        for args in (("prune-local", "repo"), ("prune-local", "repo", "--dry-run")):
            preview = self.ok(*args)
            self.assertTrue(preview["dry_run"])
            self.assertTrue(project.exists())
        for flags in (("--execute",), ("--confirm",)):
            code, _, error = self.cli("prune-local", "repo", *flags)
            self.assertNotEqual(code, 0)
            self.assertIn("both --execute and --confirm", error)
            self.assertTrue(project.exists())

        def refuse():
            code, _, error = self.cli("prune-local", "repo", "--execute", "--confirm")
            self.assertNotEqual(code, 0, error)
            self.assertTrue(project.exists(), error)
            self.assertTrue((project / "README.md").exists())
            return error

        self.receipt.unlink()
        self.assertIn("No VERIFIED receipt", refuse())
        self.receipt.write_text(original)
        for status in ("UNVERIFIED", "UPLOADED", "FAILED"):
            document = json.loads(original)
            document["items"][0]["status"] = status
            self.receipt.write_text(json.dumps(document))
            self.assertIn("No VERIFIED receipt", refuse())
        self.receipt.write_text(original)
        self.runner.git[str(project)]["head"] = "b" * 40
        self.assertIn("git_head changed", refuse())
        self.runner.git[str(project)]["head"] = "a" * 40
        readme = project / "README.md"
        os.utime(readme, (OLD + 100, OLD + 100))
        self.assertIn("latest_mtime changed", refuse())
        os.utime(readme, (OLD, OLD))
        del self.runner.objects[remote]
        refuse()
        self.runner.objects[remote] = archive + b"extra byte"
        self.assertIn("Remote size mismatch", refuse())
        self.runner.objects[remote] = archive
        allowlist = self.file(self.root / "allowlist", "repo\n")
        code, _, _ = self.cli("prune-local", "repo", "--allowlist", allowlist, "--execute", "--confirm")
        self.assertNotEqual(code, 0)
        self.assertTrue(project.exists())
        result = self.ok("prune-local", "repo", "--execute", "--confirm")
        self.assertFalse(result["dry_run"])
        self.assertFalse(project.exists())
        self.assertIn(str(project), result["deleted"])
        self.assertTrue(any(call[0] == "rclone" and call[3:5] == ["lsjson", remote] for call in self.runner.calls))

    def test_prune_caches_removes_only_regenerable_cache_and_scratch_trees(self):
        plans = self.code / "agent-plans"
        targets = [self.home / ".dotfiles/tmp"]
        for project in ("one", "two"):
            targets += [plans / project / "ticket/scratch", plans / project / "ticket/repos"]
        for target in targets:
            self.file(target / "nested/cache.bin", "regenerate me")
        protected = [
            plans / "one/ticket/spec.md", plans / "one/ticket/CONTEXT.md",
            plans / "one/ticket/docs/adr/0001.md", plans / "two/ticket/DECISIONS.md",
            self.home / ".dotfiles/bashrc", self.home / ".cache/other/retained",
            self.home / ".cache/uv/nested/cache.bin", self.home / ".uv/cache/nested/cache.bin",
        ]
        for path in protected:
            self.file(path, "keep byte-for-byte")
        before = {path: path.read_bytes() for path in protected}
        for flags in ((), ("--dry-run",)):
            preview = self.ok("prune-caches", *flags)
            self.assertTrue(preview["dry_run"])
            self.assertEqual(set(preview["targets"]), set(map(str, targets)))
            self.assertTrue(all(path.exists() for path in targets))
        code, _, _ = self.cli("prune-caches", "--execute")
        self.assertNotEqual(code, 0)
        self.assertTrue(all(path.exists() for path in targets))
        result = self.ok("prune-caches", "--execute", "--confirm")
        self.assertFalse(result["dry_run"])
        self.assertTrue(all(not path.exists() for path in targets))
        self.assertEqual(before, {path: path.read_bytes() for path in protected})
        self.assertIn("free_bytes_before", result)
        self.assertIn("free_bytes_after", result)
        self.assertFalse(self.runner.calls)

    def test_prune_batch_refuses_every_deletion_when_one_item_drifted(self):
        unchanged = self.project("a-unchanged", git=True)
        drifted = self.project("z-drifted", git=True)
        self.ok("upload-and-verify")
        os.utime(drifted / "README.md", (CUTOFF_TS, CUTOFF_TS))
        code, _, error = self.cli("prune-local", "--execute", "--confirm")
        self.assertNotEqual(code, 0)
        self.assertIn("Active or allowlisted", error)
        self.assertTrue(unchanged.exists())
        self.assertTrue(drifted.exists())
        os.utime(drifted / "README.md", (OLD, OLD))
        remote = next(item["remote"] for item in self.receipts() if item["name"] == "z-drifted")
        del self.runner.objects[remote]
        code, _, _ = self.cli("prune-local", "--execute", "--confirm")
        self.assertNotEqual(code, 0)
        self.assertTrue(unchanged.exists())
        self.assertTrue(drifted.exists())

    def test_browsable_prune_requires_live_file_count_and_size(self):
        source = self.project("docs")
        self.ok("upload-and-verify")
        item = self.receipts()[0]
        remote = item["remote"]
        original = dict(self.runner.directories[remote])
        self.runner.directories[remote]["unexpected"] = b"extra"
        code, _, error = self.cli("prune-local", "docs", "--execute", "--confirm")
        self.assertNotEqual(code, 0)
        self.assertIn("file count mismatch", error)
        self.assertTrue(source.exists())
        self.runner.directories[remote] = original
        self.runner.directories[remote]["README.md"] += b"larger"
        code, _, error = self.cli("prune-local", "docs", "--execute", "--confirm")
        self.assertNotEqual(code, 0)
        self.assertIn("Remote size mismatch", error)
        self.assertTrue(source.exists())
        self.runner.directories[remote]["README.md"] = b"payload"
        self.ok("prune-local", "docs", "--execute", "--confirm")
        self.assertFalse(source.exists())

    def test_failed_verification_never_writes_verified_receipt(self):
        self.project("repo", git=True)
        self.runner.corrupt_downloads = True
        code, _, error = self.cli("upload-and-verify")
        self.assertNotEqual(code, 0)
        self.assertIn("SHA-256 mismatch", error)
        self.assertFalse(self.receipt.exists())
        self.assertTrue((self.code / "repo").exists())

    def test_browsable_check_failure_never_writes_verified_receipt(self):
        self.project("docs")
        self.runner.fail_checks = True
        code, _, _ = self.cli("upload-and-verify")
        self.assertNotEqual(code, 0)
        self.assertFalse(self.receipt.exists())

    def test_symlink_cache_targets_are_refused_without_touching_destination(self):
        outside = self.root / "important"
        self.file(outside / "spec.md", "keep")
        target = self.home / ".dotfiles/tmp"
        target.parent.mkdir(parents=True)
        target.symlink_to(outside, target_is_directory=True)
        code, _, error = self.cli("prune-caches", "--execute", "--confirm")
        self.assertNotEqual(code, 0)
        self.assertIn("symlinked deletion target", error)
        self.assertEqual((outside / "spec.md").read_text(), "keep")
        self.assertTrue(target.is_symlink())

    @unittest.skipUnless(shutil.which("tar") and shutil.which("zstd"), "GNU tar/zstd integration requires binaries")
    def test_real_tar_zstd_preserves_git_symlinks_permissions_and_odd_names(self):
        self.runner = RealPackagingRunner()
        source = self.project("repo", git=True)
        self.file(source / "-odd\nname", "odd")
        self.file(source / "private", "private").chmod(0o600)
        self.file(source / ".git/refs/heads/target", "a" * 40)
        for name in REGENERABLE_NAMES:
            self.file(source / name / "excluded", "cache")
        (source / "link").symlink_to("README.md")
        os.utime(source / "link", (OLD, OLD), follow_symlinks=False)
        self.ok("upload-and-verify")
        destination = self.root / "restored"
        self.ok("restore", "repo", destination)
        self.assertTrue((destination / ".git/HEAD").exists())
        self.assertEqual((destination / ".git/refs/heads/target").read_text(), "a" * 40)
        self.assertEqual((destination / "-odd\nname").read_text(), "odd")
        self.assertEqual(stat.S_IMODE((destination / "private").stat().st_mode), 0o600)
        self.assertTrue((destination / "link").is_symlink())
        self.assertTrue(all(not (destination / name).exists() for name in REGENERABLE_NAMES))


if __name__ == "__main__":
    unittest.main()
