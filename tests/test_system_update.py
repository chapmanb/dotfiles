"""Offline observable CLI contract tests; no live Nix, services, git, or network."""

import contextlib
from datetime import datetime, timedelta, timezone
import io
import json
import os
import shutil
import zipfile
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))
from dotfiles_migrate import system_update
from dotfiles_migrate.daily_backup import BROWSABLE_REMOTE, BROWSABLE_ROOTS, RESTIC_REPOSITORY
from dotfiles_migrate.gatekeeper import CLIENT_ASSETS, MIGRATED_BINARIES


ROOT = Path(__file__).resolve().parents[1]


class OfflineRunner:
    def __init__(self, root, flake, source, activation):
        self.root, self.flake = root, flake
        self.source, self.activation = source, activation
        self.calls = []
        self.branch = "master"
        self.dirty = ""
        self.drivestrike = "active"
        self.fail_build = ""
        self.fail_probe = ""
        self.fail_switch = False
        self.fail_commit = False
        self.reject_source = False
        self.unchanged_lock = False
        self.candidate_track = "stable"
        self.branches = "abc\trefs/heads/nixos-26.05\n"
        self.locked = {
            name: {"pname": name, "version": "1.0", "drvPath": f"/nix/store/old-{name}.drv",
                   "patches": [], "knownVulnerabilities": []}
            for name in system_update.UNSTABLE_PACKAGES + system_update.CRITICAL_LIBRARIES
        }
        self.upstream = json.loads(json.dumps(self.locked))
        self.running_daemon = "2.35.4"
        self.candidate_daemon = "2.35.3"
        self.apt_simulation = "Inst curl [1.0] (1.1 Ubuntu)\n"
        self.daemon_failed = False
        self.sudo_failed = False
        self.visudo_failed = False
        self.post_upgrade_unhealthy = False
        self.untrusted_daemon = False
        self.root_output = None

    def run(self, args):
        args = list(map(str, args))
        self.calls.append(args)
        output = ""
        if args[:2] == ["sudo", "visudo"]:
            if self.visudo_failed:
                raise subprocess.CalledProcessError(1, args)
            if "NOPASSWD: /usr/local/sbin/dotfiles-system-update" not in Path(args[-1]).read_text():
                raise AssertionError("Unexpected sudoers content")
        elif args[:2] == ["sudo", "install"]:
            destination = Path(args[-1])
            self.assert_sandbox(destination)
            if "-d" in args:
                missing = []
                parent = destination
                while not parent.exists():
                    missing.append(parent)
                    parent = parent.parent
                destination.mkdir(parents=True, exist_ok=True)
                for directory in missing:
                    directory.chmod(0o755)
            else:
                shutil.copyfile(args[-2], destination)
            destination.chmod(int(args[args.index("-m") + 1], 8))
        elif args[0] == "sudo":
            if self.sudo_failed:
                raise subprocess.CalledProcessError(1, args)
            if args not in (["sudo", "-n", "/usr/local/sbin/dotfiles-system-update"],
                            ["sudo", "/usr/local/sbin/dotfiles-system-update"]):
                raise AssertionError(args)
            output = json.dumps(self.root_output)
        elif args[0] == "runuser":
            if args[1:5] != ["-u", "bchapman", "--", "env"] or "-i" not in args:
                raise AssertionError(args)
            command = args[args.index("NIX_CONFIG=experimental-features = nix-command flakes") + 1:]
            if command[:3] == ["nix", "eval", "--raw"]:
                output = "nix-daemon-profile-" + self.candidate_daemon
            elif command[:2] == ["nix", "build"]:
                output = str(self.root / "system/nix/store/daemon")
            elif command[-1] == "--version":
                output = "nix-daemon (Nix) " + self.candidate_daemon
            else:
                raise AssertionError(args)
        elif args[-1] == "--version" and args[0] == str(self.root / "system/nix/store/daemon/bin/nix-daemon"):
            output = "nix-daemon (Nix) " + self.candidate_daemon
        elif args == ["nix-daemon", "--version"]:
            output = "nix-daemon (Nix) " + self.running_daemon
        elif args == ["apt-get", "-s", "upgrade", "--with-new-pkgs"]:
            output = self.apt_simulation
        elif args == ["apt-get", "update"]:
            pass
        elif args[:4] == ["env", "DEBIAN_FRONTEND=noninteractive", "apt-get", "upgrade"]:
            if self.post_upgrade_unhealthy:
                self.drivestrike = "unknown"
        elif args[:2] in (["systemctl", "restart"], ["systemctl", "daemon-reload"]):
            pass
        elif args[:2] == ["systemctl", "is-active"]:
            if args[-1] == "nix-daemon" and self.daemon_failed:
                raise subprocess.CalledProcessError(3, args, output="failed")
            output = self.drivestrike if args[-1] == "drivestrike" else "active"
            if output != "active":
                raise subprocess.CalledProcessError(3, args, output=output)
        elif args[:3] == ["git", "-C", str(self.flake)]:
            if args[3:] == ["rev-parse", "--abbrev-ref", "HEAD"]:
                output = self.branch + "\n"
            elif args[3:] == ["status", "--porcelain"]:
                output = self.dirty
            elif args[3:] == ["commit", "-m", "Update flake inputs", "flake.lock"]:
                if self.fail_commit:
                    raise subprocess.CalledProcessError(1, args)
            else:
                raise AssertionError(args)
        elif args[:2] == ["git", "ls-remote"]:
            output = self.branches
        elif args[:2] == ["nix", "eval"]:
            if "--json" in args:
                packages = self.upstream if system_update.UPSTREAM_UNSTABLE in args[-1] else self.locked
                output = json.dumps(packages)
            else:
                output = str(self.source)
                if self.reject_source:
                    (self.source / "tmp").mkdir(exist_ok=True)
        elif args[:3] == ["nix", "flake", "update"]:
            self.assert_sandbox(Path(args[4]))
            self.candidate_track = "unstable" if "nixpkgs-unstable" in args[5:] else "stable"
            if not self.unchanged_lock:
                lock = self.flake / "flake.lock"
                data = json.loads(lock.read_text())
                # Retrying stable-only must start from the original lock, not the failed bump.
                if self.candidate_track == "stable":
                    if data["nodes"]["nixpkgs-unstable"]["locked"]["rev"] != "old-unstable":
                        raise AssertionError("Unstable lock was not restored before fallback")
                data["nodes"]["nixpkgs"]["locked"]["rev"] = "new-stable"
                data["nodes"]["home-manager"]["locked"]["rev"] = "new-home"
                if self.candidate_track == "unstable":
                    data["nodes"]["nixpkgs-unstable"]["locked"]["rev"] = "new-unstable"
                lock.write_text(json.dumps(data))
        elif args[:2] == ["nix", "build"] and "--expr" in args:
            if "github:NixOS/nixpkgs/" not in args[-1] or str(self.flake) in args[-1]:
                raise AssertionError("Root must build only a trusted official Nixpkgs expression")
            output = str(self.root / ("system/nix/store/different" if self.untrusted_daemon
                                      else "system/nix/store/daemon"))
        elif args[:2] == ["nix", "build"]:
            if self.fail_build in ("all", self.candidate_track):
                raise subprocess.CalledProcessError(1, args, stderr="private build output")
            output = str(self.activation) + "\n"
        elif args[:2] == ["xvfb-run", "-d"]:
            if self.fail_probe in ("all", self.candidate_track):
                raise subprocess.CalledProcessError(1, args, stderr="private probe output")
        elif args[:2] == ["home-manager", "switch"]:
            if self.fail_switch:
                raise subprocess.CalledProcessError(1, args)
        elif args[0] in ("nix-env", "nix-collect-garbage"):
            if "-p" in args:
                self.assert_sandbox(Path(args[args.index("-p") + 1]))
        else:
            raise AssertionError(f"Unexpected external command: {args}")
        return subprocess.CompletedProcess(args, 0, output, "")

    def assert_sandbox(self, path):
        if not path.is_relative_to(self.root):
            raise AssertionError(f"Refusing live-host fixture mutation: {path}")


class TestSystemUpdate(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.home = self.root / "home"
        self.flake = self.home / ".dotfiles"
        self.flake.mkdir(parents=True)
        self.system = self.root / "system"
        for asset in CLIENT_ASSETS:
            path = self.system / "opt/cisco/secureclient" / asset
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("fixture\n")
            path.chmod(0o755 if asset.startswith("bin/") else 0o644)
        ca = self.system / "usr/local/share/ca-certificates/ginkgo.crt"
        ca.parent.mkdir(parents=True)
        ca.write_text("fixture certificate\n")
        self.distrust = self.system / "etc/ca-certificates.conf"
        self.distrust.parent.mkdir(parents=True)
        self.distrust.write_text(system_update.EMUDHRA_DISTRUST + "\n")
        for directory in (self.system, *self.system.rglob("*")):
            if directory.is_dir():
                directory.chmod(0o755)
        self.backup = self.home / ".local/state/daily-backup/verification.json"
        self.backup.parent.mkdir(parents=True)
        self.backup_document = {
            "version": 1, "repository": RESTIC_REPOSITORY, "browsable_remote": BROWSABLE_REMOTE,
            "verified_at": datetime.now(timezone.utc).isoformat(),
            "Snapshot Backup": {"status": "VERIFIED", "snapshot_count": 1},
            "Browsable Sync": {"status": "VERIFIED", "roots": [
                {"source": str(self.home / name), "remote": f"{BROWSABLE_REMOTE}/{name}", "status": "VERIFIED"}
                for name in BROWSABLE_ROOTS]},
        }
        self.save_backup()
        lock = {"version": 7, "root": "root", "nodes": {
            "root": {"inputs": {name: name for name in ("nixpkgs", "home-manager", "nixpkgs-unstable")}},
            "nixpkgs": {"original": {"ref": "nixos-26.05"}, "locked": {
                "rev": "old-stable", "lastModified": int(datetime.now(timezone.utc).timestamp())}},
            "home-manager": {"locked": {"rev": "old-home"}},
            "nixpkgs-unstable": {"locked": {"rev": "old-unstable"}},
        }}
        self.lock = self.flake / "flake.lock"
        self.lock.write_text(json.dumps(lock))
        self.original_lock = self.lock.read_bytes()
        self.source = self.root / "store/source"
        self.source.mkdir(parents=True)
        self.activation = self.root / "store/activation"
        for binary in MIGRATED_BINARIES:
            path = self.activation / "home-path/bin" / binary
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("#!/bin/sh\nexit 0\n")
            path.chmod(0o755)
        self.runner = OfflineRunner(self.root, self.flake, self.source, self.activation)
        self.feed = {}
        self.fetch_calls = []

    def save_backup(self):
        self.backup.write_text(json.dumps(self.backup_document))

    def fetch(self, packages):
        self.fetch_calls.append(packages)
        return [{"vulns": self.feed.get(package["pname"], [])} for package in packages]

    def invoke(self, *options, command="run", fetcher=None):
        output, errors = io.StringIO(), io.StringIO()
        argv = [command, "--home", str(self.home), "--system-root", str(self.system), *options]
        with contextlib.redirect_stdout(output), contextlib.redirect_stderr(errors):
            code = system_update.main(argv, runner=self.runner,
                                      advisory_fetcher=fetcher if fetcher is not None else self.fetch)
        report_path = (self.system / "var/lib/system-update/report.json" if command == "run-root"
                       else self.home / ".local/state/system-update/report.json")
        report = json.loads(report_path.read_text())
        self.assertEqual(json.loads(output.getvalue()), report)
        self.assertEqual(stat.S_IMODE(report_path.stat().st_mode), 0o600)
        return code, report

    def updates(self):
        return [args for args in self.runner.calls if args[:3] == ["nix", "flake", "update"]]

    def assert_no_mutations(self):
        self.assertEqual(self.lock.read_bytes(), self.original_lock)
        self.assertFalse(self.updates())
        self.assertFalse(any(args[0] in ("home-manager", "nix-env", "nix-collect-garbage")
                             or args[0] == "xvfb-run"
                             or (args[0] == "git" and "commit" in args) for args in self.runner.calls))

    def test_cli_dry_run_backup_and_corporate_gates(self):
        entrypoint = ROOT / "bin/system-update"
        self.assertTrue(entrypoint.stat().st_mode & stat.S_IXUSR)
        help_result = subprocess.run([str(entrypoint), "--help"], capture_output=True, text=True)
        self.assertEqual(help_result.returncode, 0)
        for command in ("check", "status", "run"):
            for flags in ((), ("--execute",), ("--confirm",), ("--execute", "--confirm", "--dry-run")):
                with self.subTest(command=command, flags=flags):
                    code, report = self.invoke(*flags, command=command)
                    self.assertEqual(code, 0)
                    self.assertEqual(report["status"], "DRY_RUN")
                    self.assertEqual(report["daily_backup"], "VERIFIED")
                    self.assertEqual(report["corporate_endpoint_stack"], "VERIFIED")
                    self.assert_no_mutations()
        # Even execute flags on check/status remain read-only.
        for command in ("check", "status"):
            code, report = self.invoke("--execute", "--confirm", command=command)
            self.assertEqual(report["status"], "DRY_RUN")
            self.assert_no_mutations()
        self.backup_document["verified_at"] = (datetime.now(timezone.utc) - timedelta(hours=48)).isoformat()
        self.save_backup()
        code, report = self.invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertEqual(report["status"], "FAILED")
        self.assertIn("48 hours", report["error"])
        self.assert_no_mutations()
        self.backup_document["verified_at"] = datetime.now(timezone.utc).isoformat()
        self.backup_document["Browsable Sync"]["roots"].pop()
        self.save_backup()
        self.assertEqual(self.invoke("--execute", "--confirm")[0], 1)
        self.assert_no_mutations()
        # Restore coverage and prove unverified evidence also blocks mutation.
        self.backup_document["Browsable Sync"]["roots"] = [
            {"source": str(self.home / name), "remote": f"{BROWSABLE_REMOTE}/{name}", "status": "VERIFIED"}
            for name in BROWSABLE_ROOTS]
        self.backup_document["Snapshot Backup"]["status"] = "UNVERIFIED"
        self.save_backup()
        self.assertEqual(self.invoke("--execute", "--confirm")[0], 1)
        self.assert_no_mutations()
        self.backup_document["Snapshot Backup"]["status"] = "VERIFIED"
        self.save_backup()
        self.distrust.write_text("#" + system_update.EMUDHRA_DISTRUST + "\n")
        self.assertEqual(self.invoke("--execute", "--confirm")[0], 1)
        self.assert_no_mutations()
        self.distrust.write_text(system_update.EMUDHRA_DISTRUST + "\n")
        for state in ("inactive", "failed", "activating", "deactivating"):
            self.runner.drivestrike = state
            self.assertEqual(self.invoke("--execute", "--confirm")[0], 1)
            code, report = self.invoke("--allow-inactive-drivestrike", command="check")
            self.assertEqual(code, 0)
            self.assertEqual(report["corporate_endpoint_stack"], "VERIFIED_WITH_EXCEPTIONS")
            self.assertEqual(report["corporate_service_exceptions"], {"drivestrike": state})
        self.assert_no_mutations()
        self.runner.drivestrike = "unknown"
        self.assertEqual(self.invoke("--allow-inactive-drivestrike", "--execute", "--confirm")[0], 1)
        self.assert_no_mutations()

    def test_closure_advisory_scan_and_split_cadence_lock_update(self):
        self.runner.locked["uv"]["knownVulnerabilities"] = ["CVE-2026-10001", "CVE-2026-10002"]
        self.runner.locked["uv"]["patches"] = ["/nix/store/fix-CVE-2026-10002.patch"]
        self.runner.upstream["uv"]["drvPath"] = "/nix/store/patched-uv.drv"
        self.feed["uv"] = [{"id": "GHSA-patched", "aliases": ["CVE-2026-10002"]}]
        self.feed["ruff"] = [{"id": "CVE-2026-10003"}]
        self.runner.upstream["brave"]["version"] = "1.1"
        self.runner.upstream["brave"]["drvPath"] = "/nix/store/new-brave.drv"
        self.runner.branches += "def\trefs/heads/nixos-26.11\n"
        code, report = self.invoke("--execute", "--confirm")
        self.assertEqual(code, 0)
        track = report["tracks"]["nix_user"]
        self.assertEqual({entry["id"] for entry in track["actionable_advisories"]},
                         {"CVE-2026-10001", "BRAVE_VERSION_BUMP"})
        self.assertEqual([entry["id"] for entry in track["unpatched_advisories"]], ["CVE-2026-10003"])
        self.assertEqual(track["unpatched_advisories"][0]["status"], "UNPATCHED_UPSTREAM")
        self.assertEqual(track["stable_branch_upgrade_available"]["newer_branch"], "nixos-26.11")
        self.assertEqual({package["pname"] for package in self.fetch_calls[0]},
                         set(system_update.UNSTABLE_PACKAGES + system_update.CRITICAL_LIBRARIES))
        self.assertEqual(self.updates(), [["nix", "flake", "update", "--flake", str(self.flake),
                                          "nixpkgs", "home-manager", "nixpkgs-unstable"]])
        # An unchanged upstream must not produce futile unstable churn.
        self.lock.write_bytes(self.original_lock)
        self.runner.calls.clear()
        self.runner.upstream = json.loads(json.dumps(self.runner.locked))
        code, report = self.invoke("--execute", "--confirm")
        self.assertEqual(code, 0)
        track = report["tracks"]["nix_user"]
        self.assertEqual(track["actionable_advisories"], [])
        self.assertEqual(len(track["unpatched_advisories"]), 2)
        self.assertEqual(self.updates()[0][-2:], ["nixpkgs", "home-manager"])
        self.assertFalse(track["unstable_updated"])
        self.lock.write_bytes(self.original_lock)
        self.runner.calls.clear()
        self.invoke("--execute", "--confirm", "--include-unstable")
        self.assertEqual(self.updates()[0][-1], "nixpkgs-unstable")
        # Offline fallback still uses knownVulnerabilities and Brave; stale pins are reported.
        self.lock.write_bytes(self.original_lock)
        data = json.loads(self.lock.read_text())
        data["nodes"]["nixpkgs"]["locked"]["lastModified"] = 1
        self.lock.write_text(json.dumps(data))
        self.runner.branches = ""
        def offline(packages):
            raise OSError("network unavailable")
        code, report = self.invoke(command="check", fetcher=offline)
        self.assertEqual(code, 0)
        self.assertTrue(report["warnings"])
        self.assertTrue(report["tracks"]["nix_user"]["stable_branch_upgrade_available"]["stale"])
        self.assertEqual(len(report["tracks"]["nix_user"]["unpatched_advisories"]), 1)

    def test_user_flake_guards_fallback_activation_and_user_gc(self):
        for branch, dirty in (("feature", ""), ("master", " M flake.nix\n"), ("HEAD", "")):
            self.runner.branch, self.runner.dirty = branch, dirty
            self.runner.calls.clear()
            code, report = self.invoke("--execute", "--confirm")
            self.assertEqual(code, 0)
            self.assertEqual(report["tracks"]["nix_user"]["status"], "SKIPPED_DIRTY_WORKTREE")
            self.assertFalse(self.updates())
            self.assertEqual(self.lock.read_bytes(), self.original_lock)
            self.assert_gc(report)
        self.runner.branch, self.runner.dirty = "master", ""
        for failure in ("build", "probe"):
            with self.subTest(failure=failure):
                self.lock.write_bytes(self.original_lock)
                self.runner.calls.clear()
                self.runner.fail_build = "unstable" if failure == "build" else ""
                self.runner.fail_probe = "unstable" if failure == "probe" else ""
                code, report = self.invoke("--execute", "--confirm", "--include-unstable")
                self.assertEqual(code, 0)
                track = report["tracks"]["nix_user"]
                self.assertEqual(track["status"], "UNSTABLE_BLOCKED_BY_PREFLIGHT")
                self.assertTrue(track["stable_updated"])
                self.assertFalse(track["unstable_updated"])
                self.assertEqual(len(self.updates()), 2)
                self.assertNotIn("nixpkgs-unstable", self.updates()[1])
                self.assertEqual(json.loads(self.lock.read_text())["nodes"]["nixpkgs-unstable"]["locked"]["rev"],
                                 "old-unstable")
                activation = ["home-manager", "switch", "--flake",
                              f"git+{self.flake.as_uri()}#bchapman@bchapman-ubnt"]
                commit = ["git", "-C", str(self.flake), "commit", "-m", "Update flake inputs", "flake.lock"]
                self.assertIn(activation, self.runner.calls)
                self.assertIn(commit, self.runner.calls)
                self.assertLess(self.runner.calls.index(activation), self.runner.calls.index(commit))
                self.assertFalse(any("push" in args for args in self.runner.calls))
                self.assertTrue(any("ghostty_child_probe.py" in " ".join(args) for args in self.runner.calls))
                self.assert_gc(report)
        # Failed stable fallback restores the exact original lock and never activates/commits/GCs.
        self.lock.write_bytes(self.original_lock)
        self.runner.calls.clear()
        self.runner.fail_build, self.runner.fail_probe = "all", ""
        code, report = self.invoke("--execute", "--confirm", "--include-unstable")
        self.assertEqual(code, 1)
        self.assertEqual(report["tracks"]["nix_user"]["status"], "BLOCKED_BY_PREFLIGHT")
        self.assertEqual(self.lock.read_bytes(), self.original_lock)
        self.assertFalse(any(args[0] in ("home-manager", "nix-env", "nix-collect-garbage")
                             or "commit" in args for args in self.runner.calls))
        # Unchanged locks do not activate or commit; unchanged Ghostty does not run a probe.
        self.runner.calls.clear()
        self.runner.fail_build = ""
        self.runner.unchanged_lock = True
        active = self.home / ".nix-profile/bin/ghostty"
        active.parent.mkdir(parents=True)
        active.symlink_to(self.activation / "home-path/bin/ghostty")
        code, report = self.invoke("--execute", "--confirm")
        self.assertEqual(code, 0)
        self.assertEqual(report["tracks"]["nix_user"]["status"], "UP_TO_DATE")
        self.assertFalse(any(args[0] in ("home-manager", "xvfb-run") or "commit" in args
                             for args in self.runner.calls))
        self.assert_gc(report)

    def assert_gc(self, report):
        for profile in (self.home / ".nix-profile", self.home / ".local/state/nix/profiles/home-manager"):
            self.assertIn(["nix-env", "-p", str(profile), "--delete-generations", "14d"], self.runner.calls)
        self.assertIn(["nix-collect-garbage", "--delete-older-than", "14d"], self.runner.calls)
        self.assertEqual(report["tracks"]["store_retention"]["user_gc"], "COMPLETED")

    def test_preflight_rejects_scratch_source_and_missing_executables(self):
        for failure in ("scratch", "missing", "not-executable"):
            with self.subTest(failure=failure):
                self.runner.calls.clear()
                self.runner.reject_source = failure == "scratch"
                binary = self.activation / "home-path/bin/mysql"
                if failure == "missing":
                    binary.unlink()
                elif failure == "not-executable":
                    binary.write_text("not executable")
                    binary.chmod(0o644)
                code, report = self.invoke("--execute", "--confirm")
                self.assertEqual(code, 1)
                self.assertEqual(report["tracks"]["nix_user"]["status"], "BLOCKED_BY_PREFLIGHT")
                self.assertEqual(self.lock.read_bytes(), self.original_lock)
                self.assertFalse(any(args[0] == "home-manager" or "commit" in args for args in self.runner.calls))
                if (self.source / "tmp").exists():
                    (self.source / "tmp").rmdir()

    def test_osv_request_is_bounded_and_aliases_are_filtered(self):
        self.runner.locked["uv"]["patches"] = ["fix-CVE-2026-12345.patch"]
        def response(document):
            return io.StringIO(json.dumps(document))
        results = [{"vulns": [{"id": "GHSA-aliased"}]}] + [
            {} for _ in range(len(self.runner.locked) - 1)]
        # Attach the vulnerability to uv rather than relying on dictionary ordering.
        results[0], results[1] = results[1], results[0]
        with patch("dotfiles_migrate.system_update.urllib.request.urlopen", side_effect=[
            response({"results": results}), response({"id": "GHSA-aliased", "aliases": ["CVE-2026-12345"]})
        ]) as request:
            code, report = self.invoke(command="check", fetcher=system_update.fetch_advisories)
        self.assertEqual(code, 0)
        self.assertEqual(report["tracks"]["nix_user"]["actionable_advisories"], [])
        self.assertEqual(report["tracks"]["nix_user"]["unpatched_advisories"], [])
        batch = request.call_args_list[0]
        self.assertEqual(batch.args[0].full_url, "https://api.osv.dev/v1/querybatch")
        self.assertEqual(batch.args[0].get_method(), "POST")
        self.assertEqual(batch.kwargs["timeout"], 10)
        self.assertLessEqual(request.call_args_list[1].kwargs["timeout"], 10)
        queries = json.loads(batch.args[0].data)["queries"]
        self.assertEqual(queries[0]["package"],
                         {"name": "https://github.com/neovim/neovim", "ecosystem": "GIT"})
        self.assertEqual(queries[1]["package"], {"name": "uv", "ecosystem": "PyPI"})

    def test_report_write_failure_prevents_execution_and_preview_prints_json(self):
        for command, options, expected in (("check", [], 0), ("run", ["--execute", "--confirm"], 1)):
            with self.subTest(command=command):
                output, errors = io.StringIO(), io.StringIO()
                with patch("dotfiles_migrate.system_update.write_private", side_effect=PermissionError), \
                        contextlib.redirect_stdout(output), contextlib.redirect_stderr(errors):
                    code = system_update.main(
                        ["--home", str(self.home), command, "--system-root", str(self.system), *options],
                        runner=self.runner, advisory_fetcher=self.fetch)
                self.assertEqual(code, expected)
                self.assertEqual(json.loads(output.getvalue())["status"], "DRY_RUN" if command == "check" else "FAILED")
                self.assert_no_mutations()

    def test_activation_and_commit_failures_are_reported_without_gc(self):
        self.runner.fail_switch = True
        code, report = self.invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertEqual(report["status"], "FAILED")
        self.assertEqual(report["tracks"]["nix_user"]["status"], "BLOCKED_BY_ACTIVATION")
        self.assertEqual(self.lock.read_bytes(), self.original_lock)
        self.assertFalse(any("commit" in args for args in self.runner.calls))
        self.runner.calls.clear()
        self.runner.fail_switch, self.runner.fail_commit = False, True
        code, report = self.invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertEqual(report["status"], "FAILED")
        self.assertEqual(report["tracks"]["nix_user"]["commit_status"], "FAILED")
        self.assertNotEqual(self.lock.read_bytes(), self.original_lock)
        self.assertFalse(any(args[0] == "nix-collect-garbage" for args in self.runner.calls))

    def install_fixture_helper(self):
        with patch.object(system_update, "ROOT_UID", os.getuid()):
            code, report = self.invoke("--execute", "--confirm", command="install-root-helper")
        self.assertEqual(code, 0, report)
        return report

    def root_invoke(self, *flags):
        with patch.object(system_update, "ROOT_UID", os.getuid()), \
                patch.object(system_update.os, "geteuid", return_value=0):
            return self.invoke(*flags, command="run-root")

    def test_install_root_helper_and_nightly_delegation(self):
        helper = self.system / system_update.ROOT_HELPER
        sudoers = self.system / system_update.SUDOERS
        for flags in ((), ("--execute",), ("--confirm",), ("--execute", "--confirm", "--dry-run")):
            code, report = self.invoke(*flags, command="install-root-helper")
            self.assertEqual(code, 0)
            self.assertEqual(report["installation"]["status"], "DRY_RUN")
            self.assertFalse(helper.exists())
            self.assertFalse(sudoers.exists())
            self.assertFalse(any(call[0] == "sudo" for call in self.runner.calls))
        self.runner.visudo_failed = True
        with patch.object(system_update, "ROOT_UID", os.getuid()):
            code, report = self.invoke("--execute", "--confirm", command="install-root-helper")
        self.assertEqual(code, 1)
        self.assertFalse(helper.exists())
        self.assertFalse(sudoers.exists())
        self.runner.visudo_failed = False
        report = self.install_fixture_helper()
        self.assertEqual(report["installation"]["status"], "INSTALLED")
        self.assertEqual(stat.S_IMODE(helper.stat().st_mode), 0o755)
        self.assertEqual(stat.S_IMODE(sudoers.stat().st_mode), 0o440)
        self.assertEqual(sudoers.read_text(),
                         "bchapman ALL=(root) NOPASSWD: /usr/local/sbin/dotfiles-system-update\n")
        installs = [call for call in self.runner.calls if call[:2] == ["sudo", "install"] and "-T" in call]
        self.assertEqual([call[call.index("-o") + 1] for call in installs], ["root", "root"])
        self.assertEqual([call[call.index("-g") + 1] for call in installs], ["root", "root"])
        validation = next(i for i, call in enumerate(self.runner.calls) if call[:3] == ["sudo", "visudo", "-cf"])
        placement = next(i for i, call in enumerate(self.runner.calls) if call in installs)
        self.assertLess(validation, placement)
        with zipfile.ZipFile(helper) as archive:
            self.assertIn("dotfiles_migrate/system_update.py", archive.namelist())
            self.assertNotIn("/home/bchapman/.dotfiles", archive.read("__main__.py").decode())
        # Run the installed archive under the interpreter: arbitrary flags are
        # rejected before it can execute any privileged tracks.
        result = subprocess.run(["/usr/bin/python3", "-I", str(helper), "--help"],
                                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("accepts no arguments", result.stderr)
        self.runner.calls.clear()
        self.runner.branch = "feature"  # Skip user flake mutations, still GC then delegate.
        self.runner.root_output = {
            "status": "UPDATED", "warnings": [], "corporate_endpoint_stack": "VERIFIED_WITH_EXCEPTIONS",
            "corporate_service_exceptions": {"drivestrike": "inactive"}, "tracks": {
                "nix_daemon": {"status": "UP_TO_DATE"},
                "ubuntu_apt": {"status": "UPDATED", "reboot_required": True},
                "store_retention": {"system_gc": "COMPLETED"}}}
        code, report = self.invoke("--execute", "--confirm", "--nightly")
        self.assertEqual(code, 0)
        delegation = ["sudo", "-n", "/usr/local/sbin/dotfiles-system-update"]
        self.assertIn(delegation, self.runner.calls)
        self.assertLess(self.runner.calls.index(["nix-collect-garbage", "--delete-older-than", "14d"]),
                        self.runner.calls.index(delegation))
        self.assertTrue(report["tracks"]["ubuntu_apt"]["reboot_required"])
        self.assertEqual(report["tracks"]["store_retention"]["system_gc"], "COMPLETED")
        self.runner.calls.clear()
        self.invoke("--execute", "--confirm")
        self.assertIn(["sudo", "/usr/local/sbin/dotfiles-system-update"], self.runner.calls)
        self.runner.sudo_failed = True
        code, report = self.invoke("--execute", "--confirm", "--nightly")
        self.assertEqual(code, 0)
        self.assertEqual(report["tracks"]["nix_daemon"]["status"], "DEFERRED_SUDO_UNAVAILABLE")
        helper.unlink()
        self.runner.calls.clear()
        code, report = self.invoke("--execute", "--confirm", "--nightly")
        self.assertEqual(code, 0)
        for name in ("nix_daemon", "ubuntu_apt"):
            self.assertEqual(report["tracks"][name]["status"], "DEFERRED_ROOT_HELPER_NOT_INSTALLED")
        self.assertEqual(report["tracks"]["store_retention"]["system_gc"], "DEFERRED_ROOT_HELPER_NOT_INSTALLED")
        self.assertFalse(any(call[0] == "sudo" for call in self.runner.calls))

    def test_root_daemon_monotonic_guard_apt_zero_removal_and_system_gc(self):
        self.install_fixture_helper()
        data = json.loads(self.lock.read_text())
        for name, revision in (("nixpkgs", "a" * 40), ("nixpkgs-unstable", "b" * 40)):
            data["nodes"][name]["locked"].update(type="github", owner="NixOS", repo="nixpkgs", rev=revision)
        self.lock.write_text(json.dumps(data))
        for suffix in ("service", "socket"):
            unit = self.system / f"etc/systemd/system/nix-daemon.{suffix}"
            unit.parent.mkdir(parents=True, exist_ok=True)
            unit.write_text("[Unit]\nDescription=fixture\n")
        candidate = self.system / "nix/store/daemon"
        candidate.mkdir(parents=True)
        candidate.chmod(0o555)
        reboot = self.system / "var/run/reboot-required"
        reboot.parent.mkdir(parents=True)
        reboot.touch()
        profile = str(self.system / system_update.SYSTEM_PROFILE.lstrip("/"))
        setting = ["nix-env", "-p", profile, "--set", str(candidate)]
        gc = ["nix-env", "-p", profile, "--delete-generations", "14d"]
        upgrade = ["env", "DEBIAN_FRONTEND=noninteractive", "apt-get", "upgrade", "--with-new-pkgs",
                   "--yes", "-o", "Dpkg::Options::=--force-confdef", "-o",
                   "Dpkg::Options::=--force-confold", "-o", "DPkg::Lock::Timeout=120"]
        self.runner.calls.clear()
        code, report = self.root_invoke("--execute", "--confirm")
        self.assertEqual(code, 0, report)
        daemon, apt = report["tracks"]["nix_daemon"], report["tracks"]["ubuntu_apt"]
        self.assertEqual(daemon["status"], "UP_TO_DATE")
        self.assertEqual((daemon["running_version"], daemon["candidate_version"]), ("2.35.4", "2.35.3"))
        self.assertNotIn(setting, self.runner.calls)
        self.assertNotIn(["systemctl", "restart", "nix-daemon.service"], self.runner.calls)
        self.assertEqual(apt["upgraded_packages"], ["curl"])
        self.assertTrue(apt["reboot_required"])
        self.assertIn(upgrade, self.runner.calls)
        self.assertLess(self.runner.calls.index(["apt-get", "update"]),
                        self.runner.calls.index(["apt-get", "-s", "upgrade", "--with-new-pkgs"]))
        self.assertLess(self.runner.calls.index(["apt-get", "-s", "upgrade", "--with-new-pkgs"]),
                        self.runner.calls.index(upgrade))
        service = ["systemctl", "is-active", "drivestrike"]
        self.assertGreater(max(i for i, call in enumerate(self.runner.calls) if call == service),
                           self.runner.calls.index(upgrade))
        self.assertIn(gc, self.runner.calls)
        self.assertIn(["nix-collect-garbage", "--delete-older-than", "14d"], self.runner.calls)
        self.assertEqual(report["tracks"]["store_retention"]["system_gc"], "COMPLETED")
        for version, advisory, updated in (("2.36.0", False, True), ("2.35.3", True, True),
                                           ("2.35.4", True, False), ("2.9.9", False, False)):
            with self.subTest(version=version, advisory=advisory):
                self.runner.calls.clear()
                self.runner.candidate_daemon = version
                self.feed["nix"] = [{"id": "CVE-2026-12345"}] if advisory else []
                code, report = self.root_invoke("--execute", "--confirm")
                self.assertEqual(code, 0, report)
                self.assertEqual(setting in self.runner.calls, updated)
                self.assertEqual(["systemctl", "restart", "nix-daemon.service"] in self.runner.calls, updated)
                if updated:
                    for command in (["systemctl", "daemon-reload"], ["systemctl", "is-active", "nix-daemon"]):
                        self.assertIn(command, self.runner.calls)
        self.runner.candidate_daemon = "2.36.0"
        self.runner.untrusted_daemon = True
        self.runner.calls.clear()
        code, report = self.root_invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertIn("trusted Nixpkgs", report["tracks"]["nix_daemon"]["error"])
        self.assertNotIn(setting, self.runner.calls)
        self.runner.untrusted_daemon = False
        data["nodes"]["nixpkgs-unstable"]["locked"]["owner"] = "untrusted-user"
        self.lock.write_text(json.dumps(data))
        self.runner.calls.clear()
        code, report = self.root_invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertIn("official revision-pinned", report["tracks"]["nix_daemon"]["error"])
        self.assertNotIn(setting, self.runner.calls)
        data["nodes"]["nixpkgs-unstable"]["locked"]["owner"] = "NixOS"
        self.lock.write_text(json.dumps(data))
        self.runner.daemon_failed = True
        self.runner.calls.clear()
        code, report = self.root_invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertEqual(report["tracks"]["nix_daemon"]["status"], "ROLLED_BACK")
        rollback = ["nix-env", "--rollback", "-p", profile]
        restart = ["systemctl", "restart", "nix-daemon.service"]
        self.assertIn(rollback, self.runner.calls)
        self.assertEqual(self.runner.calls.count(restart), 2)
        self.assertLess(self.runner.calls.index(rollback), max(i for i, call in enumerate(self.runner.calls) if call == restart))
        self.assertNotIn(gc, self.runner.calls)
        self.assertIn(upgrade, self.runner.calls)  # A daemon failure does not block host maintenance.
        self.runner.daemon_failed = False
        self.runner.candidate_daemon = "2.35.3"
        self.feed["nix"] = []
        for operation, package, guard in (("Remv", "unprotected-tool", "Zero-Removal"),
                                          ("Purg", "unprotected-tool", "Zero-Removal"),
                                          ("Remv", "drivestrike", "Protected Host Package"),
                                          ("Purg", "linux-image-custom:amd64", "Protected Host Package")):
            self.runner.calls.clear()
            self.runner.apt_simulation = f"{operation} {package} [1.0]\n"
            code, report = self.root_invoke("--execute", "--confirm")
            self.assertEqual(code, 1)
            self.assertEqual(report["tracks"]["ubuntu_apt"]["status"], "BLOCKED_BY_GUARD")
            self.assertIn(guard, report["tracks"]["ubuntu_apt"]["error"])
            self.assertNotIn(upgrade, self.runner.calls)
        self.runner.apt_simulation = ""
        self.runner.calls.clear()
        code, report = self.root_invoke("--execute", "--confirm", "--dry-run")
        self.assertEqual(code, 0)
        self.assertEqual(report["status"], "DRY_RUN")
        for command in (upgrade, gc, setting, ["apt-get", "update"]):
            self.assertNotIn(command, self.runner.calls)
        self.runner.post_upgrade_unhealthy = True
        code, report = self.root_invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertEqual(report["tracks"]["ubuntu_apt"]["status"], "BLOCKED_BY_GUARD")
        self.assertEqual(report["corporate_endpoint_stack"], "UNVERIFIED")
        self.runner.post_upgrade_unhealthy = False
        self.runner.drivestrike = "active"
        self.backup_document["verified_at"] = (datetime.now(timezone.utc) - timedelta(hours=49)).isoformat()
        self.save_backup()
        self.runner.calls.clear()
        self.assertEqual(self.root_invoke("--execute", "--confirm")[0], 1)
        self.assertFalse(self.runner.calls)
        self.backup_document["verified_at"] = datetime.now(timezone.utc).isoformat()
        self.save_backup()
        self.runner.calls.clear()
        with patch.object(system_update, "ROOT_UID", os.getuid()), \
                patch.object(system_update.os, "geteuid", return_value=12345):
            code, report = self.invoke("--execute", "--confirm", command="run-root")
        self.assertEqual(code, 1)
        self.assertIn("requires root", report["error"])
        self.assertFalse(self.runner.calls)
        helper = self.system / system_update.ROOT_HELPER
        helper.chmod(0o777)
        self.runner.calls.clear()
        code, report = self.root_invoke("--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertIn("ownership invariant", report["error"])
        self.assertFalse(any(call[0] in ("apt-get", "nix-env") for call in self.runner.calls))

    def test_version_only_advisory_change_and_partially_patched_advisory(self):
        self.feed["uv"] = [{"id": "GHSA-multi", "aliases": ["CVE-2026-11111", "CVE-2026-22222"]}]
        self.runner.locked["uv"]["patches"] = ["fix-CVE-2026-11111.patch"]
        self.runner.upstream["uv"]["version"] = "1.1"
        code, report = self.invoke(command="check")
        self.assertEqual(code, 0)
        self.assertEqual(report["tracks"]["nix_user"]["actionable_advisories"][0]["id"], "GHSA-multi")
        self.assert_no_mutations()


if __name__ == "__main__":
    unittest.main()
