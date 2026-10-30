"""Offline CLI contracts for the separate, deliberately bounded duplicate purge."""

import contextlib
from datetime import datetime, timedelta, timezone
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))
from dotfiles_migrate import gatekeeper
from tests.test_nixos_stage2_and_gatekeeper import OfflineRunner


ROOT = Path(__file__).resolve().parents[1]
APPROVED = {
    "openjdk-11-jre", "openjdk-11-jre-headless", "mysql-client", "mysql-client-8.0",
    "mysql-client-core-8.0", "datamash", "ffmpeg", "graphviz", "imagemagick",
    "imagemagick-6.q16", "ncdu", "parallel", "qpdf", "sqlite3", "tree", "vim",
    "vim-runtime", "docker-compose",
}
RETAINED = (
    "openjdk-17-jre:amd64", "openjdk-17-jre-headless:amd64", "vim-tiny",
    "gcc", "gcc-9", "g++", "gfortran-9", "clang", "build-essential", "make",
    "libgcc-s1:i386", "docker-ce", "docker-compose-plugin", "ubuntu-desktop",
    "gnome-shell", "python3", "linux-image-generic-hwe-20.04", "drivestrike",
    "connectwisecontrol-a8194903625f6c0d", "firefox-locale-en", "gnome-tweak-tool",
    "xsel", "python3-mysqldb",
)


class DuplicateRunner(OfflineRunner):
    def __init__(self, root, source, activation, system):
        super().__init__(root, source, activation)
        self.system = system
        self.java_file = "/usr/lib/jvm/java-17-openjdk-amd64/bin/java"
        self.simulations = 0
        self.late_output = None
        self.compose_output = "Docker Compose version v2.35.1\n"
        self.java_version = 'openjdk version "17.0.15"\n'
        self.post_java_version = self.java_version
        self.fail_native = None

    def run(self, args):
        args = list(map(str, args))
        output = None
        if args[:2] == ["dpkg-query", "-L"]:
            output = self.java_file + "\n"
        elif args[:3] == ["apt-get", "-s", "purge"]:
            self.simulations += 1
            output = self.late_output if self.simulations > 1 and self.late_output is not None else self.purge_output
        elif args[0].startswith(str(self.system)):
            if self.fail_native and self.fail_native in args[0]:
                self.calls.append(args)
                raise subprocess.CalledProcessError(1, args, stderr="native failed")
            if args[1:] == ["-version"]:
                output = self.post_java_version if self.purged else self.java_version
                self.calls.append(args)
                return subprocess.CompletedProcess(args, 0, "", output)
            if args[1:] == ["--version"] and args[0].endswith("vim.tiny"):
                output = "VIM - Vi IMproved 8.1\n"
            elif args[1:] == ["compose", "version"]:
                output = self.compose_output
        elif args and args[0] == "sudo":
            # A fixture never executes sudo, even if implementation regresses.
            if args[:4] != ["sudo", "apt-get", "--yes", "purge"] or len(args) < 5:
                raise AssertionError(f"Forbidden host command: {args}")
            self.purged = True
            output = ""
        if output is not None:
            self.calls.append(args)
            return subprocess.CompletedProcess(args, 0, output, "")
        return super().run(args)


class TestUbuntuDuplicates(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="test-ubuntu-duplicates-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.home = self.root / "home"
        self.home.mkdir()
        self.system = self.root / "system"
        self.source = self.root / "source"
        self.source.mkdir()
        self.activation = self.root / "built"
        self.active = self.system / "nix/store/active-profile"
        self.active.mkdir(parents=True)
        (self.home / ".nix-profile").symlink_to(self.active, target_is_directory=True)
        self.report = self.root / "verification.json"
        self.good_report = {
            "version": 1, "repository": gatekeeper.RESTIC_REPOSITORY,
            "browsable_remote": gatekeeper.BROWSABLE_REMOTE,
            "verified_at": datetime.now(timezone.utc).isoformat(),
            "Snapshot Backup": {"status": "VERIFIED", "encrypted": True, "snapshot_count": 1},
            "Browsable Sync": {"status": "VERIFIED", "roots": [
                {"source": str(self.home / root),
                 "remote": f"{gatekeeper.BROWSABLE_REMOTE}/{root}", "status": "VERIFIED"}
                for root in gatekeeper.BROWSABLE_ROOTS
            ]},
        }
        self.report.write_text(json.dumps(self.good_report))
        for asset in gatekeeper.CLIENT_ASSETS:
            self.make_file(self.system / "opt/cisco/secureclient" / asset)
        self.make_file(self.system / "usr/local/share/ca-certificates/ginkgo.crt")
        for name in (*gatekeeper.MIGRATED_BINARIES, "nvim"):
            self.make_file(self.activation / "home-path/bin" / name)
            self.make_file(self.active / "bin" / name)
        self.runner = DuplicateRunner(self.root, self.source, self.activation, self.system)
        self.java = self.make_file(self.system / self.runner.java_file.lstrip("/"))
        self.default_java = self.system / "usr/bin/java"
        self.default_java.parent.mkdir(parents=True, exist_ok=True)
        self.default_java.symlink_to(self.java)
        self.make_file(self.system / "usr/bin/vim.tiny")
        self.make_file(self.system / "usr/bin/docker")
        self.select(["tree:amd64", "openjdk-11-jre:amd64"])

    def make_file(self, path, content="fixture\n"):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        path.chmod(0o755)
        return path

    def select(self, candidates):
        self.runner.packages = "".join(f"{name}\tii \n" for name in (*candidates, *RETAINED))
        self.runner.purge_output = "".join(f"Purg {name} [1.0]\n" for name in candidates)

    def invoke(self, *flags):
        self.runner.calls.clear()
        self.runner.simulations = 0
        self.runner.purged = False
        output, errors = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(output), contextlib.redirect_stderr(errors):
            code = gatekeeper.main([
                "prune-ubuntu-duplicates", "--home", str(self.home), "--flake", str(ROOT),
                "--backup-report", str(self.report), "--system-root", str(self.system), *flags,
            ], runner=self.runner)
        return code, output.getvalue(), errors.getvalue()

    def assert_no_mutation(self):
        self.assertFalse(any(call[0] in ("sudo", "home-manager", "snap") for call in self.runner.calls))
        self.assertFalse(any("autoremove" in call or "update-alternatives" in call for call in self.runner.calls))

    def assert_refusal(self, message=None, *flags):
        code, _, errors = self.invoke("--execute", "--confirm", *flags)
        self.assertEqual(code, 1, errors)
        if message:
            self.assertIn(message, errors)
        self.assert_no_mutation()

    def test_preview_selects_only_installed_approved_duplicates(self):
        self.assertEqual(gatekeeper.APPROVED_UBUNTU_DUPLICATES, APPROVED)
        self.assertEqual(len(APPROVED), 18)
        for candidates in (
            sorted(APPROVED),
            [name + (":amd64" if i % 2 else ":i386") for i, name in enumerate(sorted(APPROVED))],
            ["tree:amd64", "mysql-client"],
            [],
        ):
            with self.subTest(candidates=candidates):
                self.select(candidates)
                self.runner.packages += "absent-config\trc \nvim\trc \nvim-runtime\tun \n"
                # The separate route must never delegate to historical cleanup.
                with patch.object(gatekeeper, "simplify_ubuntu_host", side_effect=AssertionError("old route")):
                    code, output, errors = self.invoke()
                self.assertEqual(code, 0, errors)
                data = json.loads(output)
                self.assertEqual(data["status"], "DRY_RUN")
                self.assertEqual(data["purge_packages"], candidates)
                self.assert_no_mutation()
                simulations = [call for call in self.runner.calls if call[0] == "apt-get"]
                self.assertEqual(simulations, [["apt-get", "-s", "purge", *candidates]] if candidates else [])
                self.assertEqual(data["commands"], [["sudo", "apt-get", "--yes", "purge", *candidates]] if candidates else [])
        # Native-arch APT output can omit the selected architecture suffix.
        self.select(["tree:amd64"])
        self.runner.purge_output = "Purg tree [1.8]\n"
        self.assertEqual(self.invoke()[0], 0)
        self.assert_no_mutation()
        self.runner.purge_output += (
            "The following packages were automatically installed and are no longer required:\n"
            "  orphan-library\nUse 'apt autoremove' to remove them.\n"
        )
        code, output, errors = self.invoke()
        self.assertEqual(code, 0, errors)
        self.assertIn("orphan-library", json.loads(output)["apt_simulation"])
        self.assertEqual(json.loads(output)["purge_packages"], ["tree:amd64"])
        self.assert_no_mutation()

    def test_prune_refuses_unsafe_or_unready_host(self):
        for raw in ("{", "[]", "null", "{}"):
            with self.subTest(backup=raw):
                self.report.write_text(raw)
                self.assert_refusal()
                self.assertEqual(self.runner.calls, [])
        for bad in (
            {"repository": "wrong"}, {"verified_at": "invalid"},
            {"verified_at": datetime.now().isoformat()},
            {"verified_at": (datetime.now(timezone.utc) - timedelta(hours=49)).isoformat()},
            {"verified_at": (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat()},
            {"Snapshot Backup": {"status": "UNVERIFIED"}},
            {"Browsable Sync": {"status": "VERIFIED", "roots": []}},
        ):
            with self.subTest(backup=bad):
                self.report.write_text(json.dumps({**self.good_report, **bad}))
                self.assert_refusal()
                self.assertEqual(self.runner.calls, [])
        self.report.write_text(json.dumps(self.good_report))

        for profile in (self.activation / "home-path", self.active):
            for name in (*gatekeeper.MIGRATED_BINARIES, "nvim"):
                for mode in ("absent", "not-executable"):
                    with self.subTest(profile=profile, missing=name, mode=mode):
                        binary = profile / "bin" / name
                        if mode == "absent":
                            binary.unlink()
                        else:
                            binary.chmod(0o644)
                        self.assert_refusal(name)
                        self.make_file(binary)
        active_link = self.home / ".nix-profile"
        active_link.unlink()
        self.assert_refusal("Active Nix profile")
        active_link.symlink_to(self.active, target_is_directory=True)
        active_mysql = self.active / "bin/mysql"
        active_mysql.unlink()
        active_mysql.symlink_to(self.make_file(self.root / "not-nix/mysql"))
        self.assert_refusal("mysql")
        active_mysql.unlink()
        self.make_file(active_mysql)
        self.runner.fail_build = True
        self.assert_refusal("External preflight")
        self.runner.fail_build = False
        (self.source / "tmp").mkdir()
        self.assert_refusal("tmp")
        (self.source / "tmp").rmdir()

        flag = "--allow-inactive-drivestrike"
        for service in gatekeeper.CORPORATE_SERVICES:
            self.runner.inactive_service = service
            self.assert_refusal(service)
            if service != "drivestrike":
                self.assert_refusal(service, flag)
        self.runner.inactive_service = "drivestrike"
        for state in ("inactive", "failed", "activating", "deactivating"):
            self.runner.inactive_state = state
            code, output, errors = self.invoke(flag)
            self.assertEqual(code, 0, errors)
            self.assertEqual(json.loads(output)["corporate_service_exceptions"], {"drivestrike": state})
            self.assert_no_mutation()
        for state, exit_code in (("unknown", 4), ("unknown", 3), ("failed", 1), ("", 3)):
            self.runner.inactive_state = state
            self.runner.service_exit_code = exit_code
            self.assert_refusal(None, flag)
        self.runner.service_exit_code = 3
        self.runner.inactive_state = "failed"
        code, output, errors = self.invoke("--execute", "--confirm", flag)
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["corporate_service_exceptions"], {"drivestrike": "failed"})
        self.assertEqual([call for call in self.runner.calls if call[0] == "sudo"], [
            ["sudo", "apt-get", "--yes", "purge", "tree:amd64", "openjdk-11-jre:amd64"],
        ])
        self.runner.inactive_service = None
        for path in (
            *(self.system / "opt/cisco/secureclient" / asset for asset in gatekeeper.CLIENT_ASSETS),
            self.system / "usr/local/share/ca-certificates/ginkgo.crt",
        ):
            path.unlink()
            self.assert_refusal(None, flag)
            self.make_file(path)

        original = self.runner.packages
        for package in ("openjdk-17-jre:amd64", "openjdk-17-jre-headless:amd64", "vim-tiny"):
            self.runner.packages = original.replace(f"{package}\tii \n", "")
            self.assert_refusal("Missing retained native")
        self.runner.packages = original
        for path in (self.java, self.system / "usr/bin/vim.tiny", self.system / "usr/bin/docker"):
            for mode in ("absent", "not-executable"):
                if mode == "absent":
                    path.unlink()
                else:
                    path.chmod(0o644)
                self.assert_refusal()
                self.make_file(path)
        for version in ('openjdk version "11.0.27"', 'openjdk version "170.1"', "garbage"):
            self.runner.java_version = version
            self.assert_refusal("major 17")
        self.runner.java_version = 'openjdk version "17.0.15"\n'
        self.runner.compose_output = "docker-compose version 1.25.0"
        self.assert_refusal("Compose v2")
        self.runner.compose_output = "Docker Compose version v2.35.1\n"
        for binary in ("java", "vim.tiny", "docker"):
            self.runner.fail_native = binary
            self.assert_refusal()
        self.runner.fail_native = None

        selected_output = self.runner.purge_output
        forbidden = (*RETAINED, "libsurprise", "vim", "tree:i386", "mysql-client:arm64")
        for action in ("Remv", "Purg"):
            for name in forbidden:
                for phase in ("initial", "late"):
                    with self.subTest(action=action, name=name, phase=phase):
                        bad = selected_output + f"{action} {name} [1.0]\n"
                        self.runner.purge_output = bad if phase == "initial" else selected_output
                        self.runner.late_output = bad if phase == "late" else None
                        self.assert_refusal(name, flag)
        for row in ("Inst libsurprise (1.0)", "Inst gcc [9] (10)", "Conf libsurprise (1.0)", "Purg"):
            for phase in ("initial", "late"):
                self.runner.purge_output = selected_output + row if phase == "initial" else selected_output
                self.runner.late_output = selected_output + row if phase == "late" else None
                self.assert_refusal()
        incomplete = (
            ("empty", "", "omitted selected packages"),
            ("no-removal-rows", "Reading package lists...\n0 to remove.\n", "omitted selected packages"),
            ("unrelated-only", "Purg libsurprise [1.0]\n", "unselected removal"),
            ("missing-java11", "Purg tree:amd64 [1.0]\n", "omitted selected packages: openjdk-11-jre:amd64"),
            ("missing-tree", "Purg openjdk-11-jre [1.0]\n", "omitted selected packages: tree:amd64"),
        )
        for case, bad, message in incomplete:
            for phase in ("initial", "late"):
                with self.subTest(coverage=case, phase=phase):
                    self.runner.purge_output = bad if phase == "initial" else selected_output
                    self.runner.late_output = bad if phase == "late" else None
                    self.assert_refusal(message, flag)
                    self.assertEqual(self.runner.simulations, 1 if phase == "initial" else 2)
        self.runner.purge_output = selected_output
        self.runner.late_output = None

    def test_execution_confirmation_idempotency_and_postchecks(self):
        self.assertEqual(self.invoke("--execute")[0], 1)
        self.assertEqual(self.runner.calls, [])
        for flags in ((), ("--confirm",), ("--execute", "--dry-run"),
                      ("--execute", "--confirm", "--dry-run")):
            code, output, errors = self.invoke(*flags)
            self.assertEqual(code, 0, errors)
            self.assertEqual(json.loads(output)["status"], "DRY_RUN")
            self.assert_no_mutation()
        candidates = ["tree:amd64", "openjdk-11-jre:amd64"]
        code, output, errors = self.invoke("--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["default_java17"], "VERIFIED")
        purge = ["sudo", "apt-get", "--yes", "purge", *candidates]
        self.assertEqual([call for call in self.runner.calls if call[0] == "sudo"], [purge])
        index = self.runner.calls.index(purge)
        self.assertEqual(self.runner.calls[index - 1], ["apt-get", "-s", "purge", *candidates])
        self.assertEqual(self.runner.simulations, 2)
        self.assertEqual(self.runner.calls[index + 1:], [[str(self.default_java), "-version"]])
        # Exact allowlist of command vectors: no historical or implicit extra operations.
        allowed = [
            ["dpkg-query", "-W", "-f=${binary:Package}\t${db:Status-Abbrev}\\n"],
            ["dpkg-query", "-L", "openjdk-17-jre-headless:amd64"],
            [str(self.java), "-version"], [str(self.default_java), "-version"],
            [str(self.system / "usr/bin/vim.tiny"), "--version"],
            [str(self.system / "usr/bin/docker"), "compose", "version"],
            ["apt-get", "-s", "purge", *candidates], purge,
            *([["systemctl", "is-active", service] for service in gatekeeper.CORPORATE_SERVICES]),
        ]
        uri = "git+" + ROOT.as_uri()
        allowed.extend([
            ["nix", "eval", "--raw", "--impure", "--expr", f"(builtins.getFlake {json.dumps(uri)}).outPath"],
            ["nix", "build", "--no-link", "--print-out-paths", f'{uri}#homeConfigurations."bchapman@bchapman-ubnt".activationPackage'],
        ])
        for call in self.runner.calls:
            self.assertIn(call, allowed)

        for failure in ("wrong-target", "missing-target", "wrong-version", "command-failure"):
            with self.subTest(postcheck=failure):
                if failure in ("wrong-target", "missing-target"):
                    self.default_java.unlink()
                    target = self.root / "java11" if failure == "wrong-target" else self.root / "absent"
                    if failure == "wrong-target":
                        self.make_file(target)
                    self.default_java.symlink_to(target)
                elif failure == "wrong-version":
                    self.runner.post_java_version = 'openjdk version "11.0.27"'
                else:
                    # Only fail the post-purge default path, not native Java17.
                    self.runner.fail_native = "usr/bin/java"
                code, _, errors = self.invoke("--execute", "--confirm")
                self.assertEqual(code, 1)
                self.assertIn("Purge completed", errors)
                self.assertIn("no repair or restart", errors)
                self.assertEqual([call for call in self.runner.calls if call[0] == "sudo"], [purge])
                for call in self.runner.calls:
                    self.assertIn(call, allowed)
                self.default_java.unlink()
                self.default_java.symlink_to(self.java)
                self.runner.post_java_version = self.runner.java_version
                self.runner.fail_native = None

        self.select([])
        self.runner.packages += "tree\trc \nopenjdk-11-jre\tun \n"
        for _ in range(2):
            code, output, errors = self.invoke("--execute", "--confirm")
            self.assertEqual(code, 0, errors)
            self.assertEqual(json.loads(output)["purge_packages"], [])
            self.assertEqual(json.loads(output)["commands"], [])
            self.assertFalse(any("purge" in call for call in self.runner.calls))
            self.assert_no_mutation()


if __name__ == "__main__":
    unittest.main()
