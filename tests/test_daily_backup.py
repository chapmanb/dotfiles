import contextlib
from datetime import date, timedelta
import errno
import fnmatch
import io
import hashlib
import json
import os
from pathlib import Path
import shlex
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))

from dotfiles_migrate.daily_backup import main
from dotfiles_migrate import system_update


# Independent expectations: policy changes must not silently change acceptance.
REPOSITORY = "rclone:gdrive:bchapman-backup/restic-repo"
BROWSABLE = "gdrive:bchapman-backup/browsable"
TRASH = "gdrive:bchapman-backup/browsable-trash"
LEGACY = "gdrive:bchapman-ubnt-backup"
ROOTS = ("personal", "Documents")
GINKGO = ("ginkgo/analyses", "ginkgo/methods", "ginkgo/notes")
SNAPSHOT_PATHS = (
    ".dotfiles", ".ssh", ".gnupg", ".password-store", ".aws", ".kube", ".vpn",
    ".snowflake", ".dbt", ".pypirc", ".bashrc.local", ".bashrc.ginkgo", "secrets.env",
    "todo.txt", "notes.txt", ".spacemacs.env", ".agents", ".claude",
    ".claude.json", ".codex", ".pi/agent", ".gemini", ".config/herdr", ".config/nvim",
    ".config/msmtp", ".config/gspread", ".config/rclone", ".config/autostart",
    ".local/bin", ".config/monitors.xml", ".config/dconf", ".config/gh",
    ".config/glab-cli", ".config/gcloud", ".docker/config.json", ".local/share/keyrings",
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
        self.restic_outputs = {}
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
        self.protected_stage = None
        self.fail_sudo = False
        self.corrupt_digest = False
        self.fail_inventory = None
        self.inventory_outputs = {
            ("dpkg-query", "-W", "-f=${binary:Package}\\t${Version}\\t${Status}\\n"):
                "libxml2\t1.0\tinstall ok installed\n",
            ("dpkg", "--get-selections"): "libxml2\tinstall\n",
            ("apt-mark", "showmanual"): "libxml2\nlibwebkit2gtk-4.0-37\n",
            ("apt-mark", "showhold"): "",
            ("systemctl", "list-unit-files", "--type=service", "--no-pager"): "vpnagentd.service enabled\n",
            ("lsblk", "--json", "--output", "NAME,TYPE,FSTYPE,UUID,PARTUUID,SIZE,MOUNTPOINT"):
                '{"blockdevices": []}\n',
            ("findmnt", "--json", "--output", "TARGET,SOURCE,FSTYPE"): '{"filesystems": []}\n',
        }

    def run(self, args):
        args = list(map(str, args))
        self.calls.append(args)
        output = ""
        if args == ["sudo", "-n", "/usr/local/sbin/dotfiles-backup-stage"]:
            if self.fail_sudo:
                raise subprocess.CalledProcessError(1, args, stderr="private sudo failure")
            if self.protected_stage is None:
                raise AssertionError("Unexpected root backup helper invocation")
            archive = self.protected_stage / "protected-host-config.tar.gz"
            with tarfile.open(archive, "w:gz") as bundle:
                data = b"private host configuration"
                member = tarfile.TarInfo("etc/NetworkManager/NetworkManager.conf")
                member.size = len(data)
                bundle.addfile(member, io.BytesIO(data))
            archive.chmod(0o600)
            digest = "0" * 64 if self.corrupt_digest else hashlib.sha256(archive.read_bytes()).hexdigest()
            manifest = self.protected_stage / "protected-host-config.SHA256SUMS"
            manifest.write_text(f"{digest}  protected-host-config.tar.gz\n# source: etc/NetworkManager/NetworkManager.conf\n")
            manifest.chmod(0o600)
        elif tuple(args) in self.inventory_outputs:
            if args[0] == self.fail_inventory:
                raise subprocess.CalledProcessError(1, args)
            output = self.inventory_outputs[tuple(args)]
        elif args[0] == "pass":
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
                output = '{"version": 2, "id": "test-repository"}'
            elif operation == "key":
                if args[operation_index + 1:] != ["list", "--json"]:
                    raise AssertionError(args)
                output = '[{"id": "test-key"}]'
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
                self.snapshots = [{"id": "b" * 64, "paths": sources}]
            elif operation == "snapshots":
                if args[operation_index + 1:] != ["--json"]:
                    raise AssertionError(args)
                output = json.dumps(self.snapshots)
            elif operation not in ("check", "forget"):
                raise AssertionError(args)
            output = self.restic_outputs.get(operation, output)
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
        scratch = Path(__file__).resolve().parents[1] / "tmp"
        scratch.mkdir(exist_ok=True)
        self.temporary = tempfile.TemporaryDirectory(prefix="test-daily-backup-", dir=scratch)
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
        for relative in ROOTS + GINKGO:
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

    def test_stage_protected_creates_verified_archive_and_enforces_ownership_invariants(self):
        self.state.mkdir(mode=0o700)
        helper = self.file(self.system / "usr/local/sbin/dotfiles-backup-stage")
        rule = self.file(self.system / "etc/sudoers.d/dotfiles-backup-stage")
        helper.chmod(0o755)
        rule.chmod(0o440)
        contents = {
            "etc/NetworkManager/NetworkManager.conf": "[main]\nplugins=keyfile\n",
            "etc/NetworkManager/system-connections/Ginkgo Corporate.nmconnection":
                "[802-1x]\nclient-cert=/var/centrify/net/certs/auto_GinkgoComputerAuthenticationv2.crt\n"
                "private-key=/var/centrify/net/certs/auto_GinkgoComputerAuthenticationv2.key\n"
                "ca-cert=/usr/local/share/ca-certificates/GinkgoBioworks_Root_CA.crt\n",
            "etc/NetworkManager/system-connections/Ginkgo Internet.nmconnection": "internet",
            "etc/NetworkManager/system-connections/Ginkgo Visitor.nmconnection": "visitor",
            "etc/NetworkManager/system-connections/Other.nmconnection": "other",
            "var/centrify/net/certs/auto_GinkgoComputerAuthenticationv2.crt": "client certificate bytes",
            "var/centrify/net/certs/auto_GinkgoComputerAuthenticationv2.key": "private key bytes",
            "usr/local/share/ca-certificates/GinkgoBioworks_Root_CA.crt": "CA target bytes",
            "etc/sudoers": "root ALL=(ALL:ALL) ALL\n",
            "etc/sudoers.d/dotfiles-backup-stage": "payload",
            "etc/sudoers.d/corporate-policy": "corporate policy",
        }
        for name, content in contents.items():
            path = self.system / name
            if "certs/auto_" in name or name.endswith("GinkgoBioworks_Root_CA.crt"):
                target = self.file(path.with_name(path.name + ".target"), content)
                path.symlink_to(target.name)
            elif path != rule:
                self.file(path, content)
        for path in (self.system, *self.system.rglob("*")):
            if path.is_dir():
                path.chmod(0o755)
        archive = self.stage / "protected-host-config.tar.gz"
        manifest = self.stage / "protected-host-config.SHA256SUMS"

        def stage(*args, euid=0):
            with patch.object(system_update, "ROOT_UID", os.getuid()), \
                    patch("os.geteuid", return_value=euid):
                return self.cli("stage-protected", *args)

        # Homes may use Ubuntu's default 0750 or the operator's existing 0755.
        for mode in (0o700, 0o750, 0o755):
            with self.subTest(home_mode=oct(mode)):
                self.home.chmod(mode)
                status, stdout, stderr = stage()
                self.assertEqual(status, 0, stderr)
                self.assertEqual(json.loads(stdout)["status"], "STAGED")
        owner = self.home.stat()
        for path in (archive, manifest):
            info = path.stat()
            self.assertEqual(stat.S_IMODE(info.st_mode), 0o600)
            self.assertEqual((info.st_uid, info.st_gid), (owner.st_uid, owner.st_gid))
        self.assertEqual(stat.S_IMODE(self.stage.stat().st_mode), 0o700)
        digest = hashlib.sha256(archive.read_bytes()).hexdigest()
        self.assertEqual(manifest.read_text().splitlines()[0],
                         digest + "  protected-host-config.tar.gz")
        with tarfile.open(archive, "r:gz") as bundle:
            self.assertEqual(set(bundle.getnames()), set(contents))
            for member in bundle.getmembers():
                self.assertTrue(member.isfile(), member.name)
                self.assertEqual(bundle.extractfile(member).read(), contents[member.name].encode())
                self.assertIn("# source: " + member.name, manifest.read_text())
        self.assertFalse(self.runner.calls, "Protected staging must not invoke sudo or rclone")
        previous = archive.read_bytes()
        previous_manifest = manifest.read_bytes()
        self.assertEqual(stage(euid=12345)[0], 1)
        self.assertEqual(archive.read_bytes(), previous)
        key = self.system / "var/centrify/net/certs/auto_GinkgoComputerAuthenticationv2.key"
        target = key.resolve()
        saved_target = target.with_name(target.name + ".saved")
        target.rename(saved_target)
        self.assertEqual(stage()[0], 1)
        self.assertEqual(archive.read_bytes(), previous)
        self.assertEqual(manifest.read_bytes(), previous_manifest)
        saved_target.rename(target)
        archive.unlink()
        manifest.unlink()

        def rejected():
            status, _, stderr = stage()
            self.assertEqual(status, 1, stderr)
            self.assertFalse(archive.exists())
            self.assertFalse(manifest.exists())
            self.assertFalse(list(self.stage.glob(".protected-*")))

        # Every required fixed source must be present, including all cert targets.
        for name in contents:
            if name.endswith("Other.nmconnection") or name.endswith("corporate-policy"):
                continue  # Additional profiles/policies are included when present.
            path = self.system / name
            saved = path.with_name(path.name + ".saved")
            path.rename(saved)
            with self.subTest(missing=name):
                rejected()
            saved.rename(path)
        key.unlink()
        key.symlink_to(self.file(self.root / "outside-system.key", "not an allowed target"))
        rejected()
        key.unlink()
        key.symlink_to(target.name)
        for path in (helper, rule):
            with self.subTest(untrusted_install=str(path)):
                mode = stat.S_IMODE(path.stat().st_mode)
                path.chmod(0o777)
                rejected()
                path.chmod(mode)
        with patch.object(system_update, "ROOT_UID", os.getuid() + 1), \
                patch("os.geteuid", return_value=0):
            self.assertEqual(self.cli("stage-protected")[0], 1)
        for directory in (self.state, self.stage):
            for mode in (0o750, 0o755):
                with self.subTest(wrong_mode=str(directory), mode=oct(mode)):
                    directory.chmod(mode)
                    rejected()
                    directory.chmod(0o700)
        for directory in (self.home, self.state, self.stage):
            with self.subTest(symlink=str(directory)):
                saved = directory.with_name(directory.name + "-real")
                directory.rename(saved)
                directory.symlink_to(saved, target_is_directory=True)
                rejected()
                directory.unlink()
                saved.rename(directory)
        home_parent = self.root / "linked-home-parent"
        home_parent.symlink_to(self.root, target_is_directory=True)
        self.assertEqual(stage("--home", home_parent / self.home.name)[0], 1)
        self.assertFalse(archive.exists())
        self.assertFalse(manifest.exists())
        home_parent.unlink()
        # Symlinked ancestors below home must not bypass the directory checks.
        parent = self.home / "state-parent"
        parent.mkdir()
        nested = parent / "state"
        self.state.rename(nested)
        parent.rename(self.home / "real-parent")
        parent.symlink_to("real-parent", target_is_directory=True)
        with patch.object(system_update, "ROOT_UID", os.getuid()), \
                patch("os.geteuid", return_value=0):
            self.assertEqual(self.cli("stage-protected", "--state-dir", nested)[0], 1)
        parent.unlink()
        (self.home / "real-parent/state").rename(self.state)
        for path in (archive, manifest, self.system / "etc/NetworkManager/NetworkManager.conf"):
            with self.subTest(symlink_file=str(path)):
                saved = path.with_name(path.name + ".real")
                existed = path.exists()
                if existed:
                    path.rename(saved)
                else:
                    self.file(saved, "must remain untouched")
                path.symlink_to(saved.name)
                status, _, _ = stage()
                self.assertEqual(status, 1)
                self.assertEqual(saved.read_text(), contents.get(
                    str(path.relative_to(self.system)) if path.is_relative_to(self.system) else "",
                    "must remain untouched"))
                path.unlink()
                if existed:
                    saved.rename(path)
                else:
                    saved.unlink()
        self.assertFalse(archive.exists())
        self.assertFalse(manifest.exists())

    def ubuntu_host(self):
        names = (
            "etc/os-release", "etc/apparmor.d/nix-userns", "etc/bash.bashrc",
            "etc/tmpfiles.d/nix-opengl-driver.conf", "opt/.cisco/certificates/device.pem",
            "opt/connectwisecontrol-test/ClientLaunchParameters.txt", "etc/centrifydc/centrifydc.conf",
            "etc/apparmor.d/abstractions/centrifydc",
            "etc/apparmor.d/abstractions/authentication.d/centrifydc",
            "etc/apparmor.d/abstractions/nameservice.d/centrifydc", "etc/apparmor.d/local/custom",
            "etc/sysctl.d/ipv6.conf", "etc/brave/policies/managed/extensions.json",
            "etc/opt/chrome/policies/managed/extensions.json", "etc/apt/sources.list.d/vendor.sources",
            "etc/apt/preferences.d/snapd", "etc/apt/keyrings/vendor.gpg",
            "etc/apt/trusted.gpg.d/vendor.gpg", "usr/share/keyrings/oem-archive-keyring.gpg",
            "usr/share/keyrings/google-chrome.gpg", "etc/fstab", "etc/crypttab",
            "etc/default/grub", "etc/default/grub.d/custom.cfg", "etc/default/kdump-tools",
            "etc/pam.d/common-auth", "etc/nsswitch.conf", "etc/ssh/sshd_config",
            "etc/ssh/sshd_config.d/custom.conf", "etc/systemd/system/vpn.service.d/override.conf",
        )
        for name in names:
            self.file(self.system / name, name + "\n")
        (self.system / "etc/systemd/system/masked.service").symlink_to("/dev/null")
        (self.system / "etc/systemd/system/multi-user.target.wants").mkdir()
        (self.system / "etc/systemd/system/multi-user.target.wants/vpn.service").symlink_to("../vpn.service")
        (self.home / ".nix-profile").symlink_to("/nix/var/nix/profiles/per-user/test/profile")
        profiles = self.system / "nix/var/nix/profiles"
        profiles.mkdir(parents=True)
        (profiles / "default").symlink_to("default-1-link")
        (profiles / "default-1-link").symlink_to("/nix/store/test-profile")
        installer = self.file(self.home / ".vpn/installers/cisco-secure-client-linux64-5.1.18.314-predeploy-deb-k9.tgz")
        installer.parent.chmod(0o700)
        installer.chmod(0o600)
        self.runner.protected_stage = self.stage
        return names, installer

    def test_stage_ubuntu_host_recovery_bundle_overlay_and_inventory(self):
        names, installer = self.ubuntu_host()
        self.ok("init")
        for command in ("stage-corp-stack", "run"):
            with self.subTest(command=command):
                self.runner.calls.clear()
                self.ok(command)
                self.assertIn(["sudo", "-n", "/usr/local/sbin/dotfiles-backup-stage"], self.runner.calls)
                self.assertEqual(stat.S_IMODE(self.stage.stat().st_mode), 0o700)
                for name in names:
                    self.assertEqual((self.stage / name).read_bytes(), (self.system / name).read_bytes())
                for name in ("etc/systemd/system/masked.service",
                             "etc/systemd/system/multi-user.target.wants/vpn.service",
                             "opt/cisco/secureclient/lib/libvpn.so"):
                    self.assertTrue((self.stage / name).is_symlink())
                    self.assertEqual(os.readlink(self.stage / name), os.readlink(self.system / name))
                for args, output in self.runner.inventory_outputs.items():
                    self.assertIn(list(args), self.runner.calls)
                inventory = self.stage / "host-inventory"
                expected = {
                    "packages.tsv": "libxml2\t1.0\tinstall ok installed\n",
                    "package-selections.txt": "libxml2\tinstall\n",
                    "manual-packages.txt": "libxml2\nlibwebkit2gtk-4.0-37\n",
                    "held-packages.txt": "",
                    "service-unit-files.txt": "vpnagentd.service enabled\n",
                    "storage.json": '{"blockdevices": []}\n',
                    "mounts.json": '{"filesystems": []}\n',
                }
                self.assertEqual({path.name for path in inventory.iterdir()},
                                 set(expected) | {"nix-profile-references.txt"})
                for name, output in expected.items():
                    self.assertEqual((inventory / name).read_text(), output)
                references = (inventory / "nix-profile-references.txt").read_text()
                self.assertIn(str(self.home / ".nix-profile") + " -> /nix/var/nix/profiles/per-user/test/profile", references)
                self.assertIn("default-1-link -> /nix/store/test-profile", references)
                self.assertFalse(any(call[0] == "nix-env" for call in self.runner.calls))
                archive = self.stage / "protected-host-config.tar.gz"
                manifest = self.stage / "protected-host-config.SHA256SUMS"
                for path in (archive, manifest, installer):
                    self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o600)
                self.assertEqual(manifest.read_text().splitlines()[0],
                                 hashlib.sha256(archive.read_bytes()).hexdigest() + "  protected-host-config.tar.gz")
                if command == "run":
                    for path in (archive, manifest, inventory / "packages.tsv", installer):
                        self.assertIn(str(path), self.runner.snapshot_files)

        # Each failure must preserve every byte, mode and symlink in the last stage.
        def stage_contents():
            return {str(path.relative_to(self.stage)): (
                stat.S_IMODE(path.lstat().st_mode),
                os.readlink(path) if path.is_symlink() else path.read_bytes(),
            ) for path in self.stage.rglob("*") if path.is_symlink() or path.is_file()}

        previous = stage_contents()
        for command in ("stage-corp-stack", "run"):
            for failure in ("sudo", "digest", "inventory", "installer-mode", "installer-empty",
                            "installer-missing", "installer-symlink", "etc/apparmor.d/nix-userns",
                            "etc/bash.bashrc", "etc/tmpfiles.d/nix-opengl-driver.conf",
                            "etc/ca-certificates.conf", "opt/cisco/secureclient/bin/vpnui"):
                with self.subTest(command=command, failure=failure):
                    self.runner.calls.clear()
                    self.runner.fail_sudo = failure == "sudo"
                    self.runner.corrupt_digest = failure == "digest"
                    self.runner.fail_inventory = "dpkg-query" if failure == "inventory" else None
                    removed = None
                    saved = None
                    if "/" in failure and not failure.startswith("installer"):
                        removed = self.system / failure
                    elif failure in ("installer-missing", "installer-symlink"):
                        removed = installer
                    if removed:
                        saved = removed.with_name(removed.name + ".saved")
                        removed.rename(saved)
                        if failure == "installer-symlink":
                            removed.symlink_to(saved.name)
                    if failure == "installer-mode":
                        installer.chmod(0o644)
                    if failure == "installer-empty":
                        installer.write_bytes(b"")
                    try:
                        status, stdout, stderr = self.cli(command)
                        self.assertEqual(status, 1, stderr)
                        self.assertNotIn("private sudo failure", stdout + stderr)
                        self.assertEqual(stage_contents(), previous)
                        self.assertEqual(self.restic_calls("backup"), [])
                        self.assertEqual(self.rclone_calls("sync"), [])
                        self.assertFalse(list(self.state.glob(".corp-*")))
                        if command == "run":
                            self.assertEqual(json.loads(self.report.read_text())["Snapshot Backup"]["status"], "UNVERIFIED")
                    finally:
                        self.runner.fail_sudo = False
                        self.runner.corrupt_digest = False
                        self.runner.fail_inventory = None
                        if saved:
                            if removed.is_symlink():
                                removed.unlink()
                            saved.rename(removed)
                        installer.chmod(0o600)
                        installer.write_text("payload")

        # Ubuntu also ships os-release as a symlink; archive its target as well.
        release = self.system / "etc/os-release"
        release.unlink()
        self.file(self.system / "usr/lib/os-release", "ID=ubuntu\n")
        release.symlink_to("../usr/lib/os-release")
        optional = self.file(self.system / "etc/centrifydc/private.conf", "not readable")
        original_access = os.access

        def access(path, mode):
            return False if Path(path) == optional else original_access(path, mode)

        with patch("os.access", side_effect=access):
            self.ok("run")
        self.assertFalse((self.stage / "etc/centrifydc/private.conf").exists())
        self.assertEqual(os.readlink(self.stage / "etc/os-release"), "../usr/lib/os-release")
        self.assertEqual((self.stage / "usr/lib/os-release").read_text(), "ID=ubuntu\n")
        self.ok("verify")
        (self.stage / "usr/lib/os-release").unlink()
        status, stdout, stderr = self.cli("verify")
        self.assertEqual(status, 1, stderr)
        self.assertEqual(json.loads(stdout)["Snapshot Backup"]["status"], "UNVERIFIED")

    def test_verify_enforces_staged_host_recovery_and_snapshot_stage_path(self):
        _, installer = self.ubuntu_host()
        self.ok("init")
        self.ok("run")
        self.runner.calls.clear()
        self.ok("verify")
        required = (
            "protected-host-config.tar.gz", "protected-host-config.SHA256SUMS",
            "etc/apparmor.d/nix-userns", "etc/bash.bashrc", "etc/tmpfiles.d/nix-opengl-driver.conf",
            "etc/ca-certificates.conf", "opt/cisco/secureclient/bin/vpnui",
            "usr/local/share/ca-certificates/Ginkgo_Root.crt",
            "usr/local/share/ca-certificates/trust_custom.crt",
            "host-inventory/packages.tsv", "host-inventory/package-selections.txt",
            "host-inventory/manual-packages.txt", "host-inventory/held-packages.txt",
            "host-inventory/service-unit-files.txt", "host-inventory/storage.json",
            "host-inventory/mounts.json", "host-inventory/nix-profile-references.txt",
        )

        def unverified():
            status, stdout, stderr = self.cli("verify")
            self.assertEqual(status, 1, stderr)
            report = json.loads(stdout)
            self.assertEqual(report, json.loads(self.report.read_text()))
            self.assertEqual(report["Snapshot Backup"]["status"], "UNVERIFIED")
            self.assertNotIn("encrypted", report["Snapshot Backup"])
            self.assertEqual(report["Browsable Sync"]["status"], "VERIFIED")

        # Individual CA files may vary, but at least one certificate is required.
        certs = self.stage / "usr/local/share/ca-certificates"
        saved_certs = certs.with_name("saved-certs")
        certs.rename(saved_certs)
        unverified()
        saved_certs.rename(certs)
        for path in [self.stage / name for name in required if not name.endswith(".crt")] + [installer]:
            with self.subTest(missing=str(path)):
                self.ok("verify")
                saved = path.with_name(path.name + ".saved")
                path.rename(saved)
                try:
                    unverified()
                finally:
                    saved.rename(path)
        for path in (self.stage / "protected-host-config.tar.gz",
                     self.stage / "protected-host-config.SHA256SUMS", installer):
            for failure in ("mode", "empty", "symlink", "corrupt"):
                if path == installer and failure == "corrupt":
                    continue  # The installer has no independent checksum manifest.
                with self.subTest(path=str(path), failure=failure):
                    self.ok("verify")
                    original = path.read_bytes()
                    saved = path.with_name(path.name + ".saved")
                    try:
                        if failure == "mode":
                            path.chmod(0o644)
                        elif failure == "symlink":
                            path.rename(saved)
                            path.symlink_to(saved.name)
                        else:
                            path.write_bytes(b"" if failure == "empty" else b"corrupt")
                        unverified()
                    finally:
                        if path.is_symlink():
                            path.unlink()
                            saved.rename(path)
                        path.write_bytes(original)
                        path.chmod(0o600)
        for name in ("packages.tsv", "manual-packages.txt", "nix-profile-references.txt"):
            path = self.stage / "host-inventory" / name
            original = path.read_text()
            path.write_text("\n")
            unverified()
            path.write_text(original)
        self.ok("verify")
        paths = self.runner.snapshots[-1]["paths"]
        paths.remove(str(self.stage))
        unverified()
        paths.append(str(self.stage))
        self.ok("verify")
        self.assertFalse(any(call[0] == "sudo" for call in self.runner.calls))

    def test_expanded_user_space_snapshot_roots_and_gcloud_exclusions(self):
        expanded = (
            ".local/bin", ".config/monitors.xml", ".config/dconf", ".config/gh",
            ".config/glab-cli", ".config/gcloud", ".docker/config.json", ".local/share/keyrings",
        )
        included = []
        for name in expanded:
            path = self.home / name
            if name in (".config/monitors.xml", ".docker/config.json"):
                included.append(self.file(path, name))
            else:
                included.append(self.file(path / "state", name))
        (self.home / ".local/bin/launcher").symlink_to("state")
        excluded = [self.file(self.home / ".config/gcloud" / name / "noise")
                    for name in ("logs", "cache")]
        self.ok("init")
        result = self.ok("run")
        for name in expanded:
            self.assertIn(str(self.home / name), result["sources"])
            self.assertIn(str(self.home / name), self.runner.snapshots[-1]["paths"])
        for path in included:
            self.assertIn(str(path), self.runner.snapshot_files)
        self.assertTrue((self.home / ".local/bin/launcher").is_symlink())
        for path in excluded:
            self.assertNotIn(str(path), self.runner.snapshot_files)
            self.assertIn(str(path.parent), self.runner.exclusion_text.splitlines())

    def test_ginkgo_roots_routed_to_restic_and_password_dir_excluded(self):
        from dotfiles_migrate import daily_backup

        self.assertEqual(daily_backup.BROWSABLE_ROOTS, ROOTS)
        self.assertEqual(daily_backup.GINKGO_ROOTS, GINKGO)
        self.assertIn(".config/restic", daily_backup.PRIVATE_EXCLUSIONS)
        preserved = []
        artifacts = []
        for relative in GINKGO:
            root = self.home / relative
            for name in ("old-project/pre-2026.txt", "old-project/.git/HEAD",
                         "old-project/.git/refs/heads/target", "old-project/.git/objects/venv/blob"):
                preserved.append(self.file(root / name, timestamp=OLD))
            for name in ARTIFACTS:
                artifacts.append(self.file(root / "old-project" / name / "junk"))
            for marker in ("pyvenv.cfg", "CACHEDIR.TAG"):
                directory = root / "old-project" / ("marked-" + marker)
                self.file(directory / marker)
                artifacts.append(self.file(directory / "junk"))
        self.ok("init")
        self.ok("run")
        syncs = self.rclone_calls("sync")
        self.assertEqual(len(syncs), 2)
        self.assertEqual([(call[4], call[5]) for call in syncs], [
            (str(self.home / relative), f"{BROWSABLE}/{relative}") for relative in ROOTS
        ])
        backup = self.restic_calls("backup")[0]
        sources = backup[backup.index("--exclude-file") + 2:]
        for relative in GINKGO:
            self.assertIn(str(self.home / relative), sources)
        for path in preserved:
            self.assertLess(path.stat().st_mtime, NEW - 86400)
            self.assertIn(str(path), self.runner.snapshot_files)
        for path in artifacts:
            self.assertNotIn(str(path), self.runner.snapshot_files)
            self.assertIn(str(path.parent), self.runner.exclusion_text.splitlines())
        self.assertIn(str(self.password.parent), self.runner.exclusion_text.splitlines())
        self.assertNotIn(str(self.password), self.runner.snapshot_files)
        self.assertFalse(any(call[0] == "git" for call in self.runner.calls))

        staged = self.file(self.stage / "previous-stage", "unchanged")
        for relative in GINKGO + ROOTS:
            for failure in ("missing", "symlink"):
                with self.subTest(root=relative, failure=failure):
                    root = self.home / relative
                    saved = root.with_name(root.name + "-saved")
                    root.rename(saved)
                    if failure == "symlink":
                        root.symlink_to(saved, target_is_directory=True)
                    self.runner.calls.clear()
                    try:
                        self.assertNotEqual(self.cli("run")[0], 0)
                        self.assertEqual(self.restic_calls("backup"), [])
                        self.assertEqual(self.restic_calls("forget"), [])
                        self.assertEqual(self.rclone_calls("sync"), [])
                        self.assertEqual(self.rclone_calls("purge"), [])
                        self.assertEqual(staged.read_text(), "unchanged")
                        self.assertEqual(json.loads(self.report.read_text())["Snapshot Backup"]["status"],
                                         "UNVERIFIED")
                    finally:
                        if root.is_symlink():
                            root.unlink()
                        saved.rename(root)

    def test_verify_enforces_restic_encryption_keys_and_ginkgo_snapshot_paths(self):
        self.ok("init")
        self.ok("run")
        good_snapshots = [
            {"id": "old-snapshot"},
            {"id": "latest-snapshot", "paths": [str(self.home / relative) for relative in GINKGO] + [str(self.stage)]},
        ]
        self.runner.snapshots = good_snapshots
        self.runner.calls.clear()
        report = self.ok("verify")
        self.assertEqual(report["Snapshot Backup"], {
            "status": "VERIFIED", "encrypted": True, "snapshot_count": 2,
        })
        self.assertEqual(json.loads(self.report.read_text()), report)
        operations = [call[call.index("-o") + 2:] for call in self.runner.calls if call[0] == "restic"]
        self.assertEqual(operations, [["check"], ["cat", "config"], ["key", "list", "--json"],
                                      ["snapshots", "--json"]])
        self.assertEqual([call[4] for call in self.rclone_calls("check")],
                         [str(self.home / relative) for relative in ROOTS])

        invalid_outputs = []
        for config in (None, [], {}, {"version": 1, "id": "repo"},
                       {"version": "2", "id": "repo"}, {"version": 2.0, "id": "repo"},
                       {"version": True, "id": "repo"}, {"version": 2},
                       {"version": 2, "id": ""}, {"version": 2, "id": 42}):
            invalid_outputs.append(("cat", json.dumps(config)))
        for keys in (None, {}, [], [None], [{}], [{"id": ""}], [{"id": 42}],
                     [{"id": "valid"}, {}]):
            invalid_outputs.append(("key", json.dumps(keys)))
        for snapshots in (None, {}, [], [None], [{}], [{"id": ""}], [{"id": 42}],
                          [{"id": "snapshot"}], [{"id": "snapshot", "paths": None}],
                          [{"id": "snapshot", "paths": " ".join(good_snapshots[-1]["paths"])}]):
            invalid_outputs.append(("snapshots", json.dumps(snapshots)))
        for relative in GINKGO:
            invalid_outputs.append(("snapshots", json.dumps([
                good_snapshots[-1], {"id": "latest", "paths": [
                    str(self.home / root) for root in GINKGO if root != relative
                ] + [str(self.stage)]},
            ])))
        invalid_outputs.append(("snapshots", json.dumps([{
            "id": "latest", "paths": [str(self.home / root) for root in GINKGO],
        }])))
        invalid_outputs.extend((operation, "not JSON") for operation in ("cat", "key", "snapshots"))
        for operation, output in invalid_outputs:
            with self.subTest(operation=operation, output=output):
                self.ok("verify")  # A failure must revoke a previously verified report.
                self.runner.restic_outputs[operation] = output
                try:
                    status, stdout, stderr = self.cli("verify")
                    self.assertEqual(status, 1, stderr)
                    failed = json.loads(stdout)
                    self.assertEqual(failed, json.loads(self.report.read_text()))
                    self.assertEqual(failed["Snapshot Backup"]["status"], "UNVERIFIED")
                    self.assertNotIn("encrypted", failed["Snapshot Backup"])
                    self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
                finally:
                    self.runner.restic_outputs.clear()
        for operation in ("check", "cat", "key", "snapshots"):
            with self.subTest(command_failure=operation):
                self.ok("verify")
                self.runner.fail_restic = operation
                try:
                    status, stdout, stderr = self.cli("verify")
                    self.assertEqual(status, 1, stderr)
                    self.assertNotIn("private restic failure", stdout + stderr)
                    self.assertEqual(json.loads(self.report.read_text())["Snapshot Backup"]["status"],
                                     "UNVERIFIED")
                finally:
                    self.runner.fail_restic = None
        for relative in GINKGO + ROOTS:
            for failure in ("missing", "symlink"):
                with self.subTest(root=relative, failure=failure):
                    self.ok("verify")
                    root = self.home / relative
                    saved = root.with_name(root.name + "-saved")
                    root.rename(saved)
                    if failure == "symlink":
                        root.symlink_to(saved, target_is_directory=True)
                    self.runner.calls.clear()
                    try:
                        self.assertEqual(self.cli("verify")[0], 1)
                        tier = "Snapshot Backup" if relative in GINKGO else "Browsable Sync"
                        self.assertEqual(json.loads(self.report.read_text())[tier]["status"], "UNVERIFIED")
                        if relative in GINKGO:
                            self.assertFalse(any(call[0] == "restic" for call in self.runner.calls))
                        self.assertEqual(self.restic_calls("forget"), [])
                        self.assertEqual(self.rclone_calls("sync"), [])
                        self.assertEqual(self.rclone_calls("purge"), [])
                    finally:
                        if root.is_symlink():
                            root.unlink()
                        saved.rename(root)

    def test_retired_wezterm_config_is_not_a_snapshot_source(self):
        retired = self.file(self.home / ".wezterm.lua", "retired terminal config")
        active = self.file(self.home / ".config/nvim/init.lua", "active editor config")
        self.ok("init")
        self.ok("run")
        backup = self.restic_calls("backup")[0]
        sources = backup[backup.index("--exclude-file") + 2:]
        self.assertNotIn(str(retired), sources)
        self.assertNotIn(str(retired), self.runner.snapshot_files)
        self.assertIn(str(active), self.runner.snapshot_files)

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
        self.assertEqual(len(self.rclone_calls("sync")), 6)

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
        self.assertEqual(len(self.rclone_calls("sync")), 2)

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
                         ".pi/agent/sessions", ".pi/agent/web-search-cache", "bio", "ginkgo/guild",
                         "ginkgo/code/acs-mirror-local", "ginkgo/code/hamilton-ultimate"):
            self.file(self.home / relative / "junk", timestamp=NEW)
        self.file(self.home / ".claude.backup")

        result = self.ok("run")
        self.assertEqual(result["status"], "BACKED_UP")
        backup = self.restic_calls("backup")
        self.assertEqual(len(backup), 1)
        call = backup[0]
        sources = set(call[call.index("--exclude-file") + 2:])
        expected = {str(self.home / path) for path in SNAPSHOT_PATHS + GINKGO}
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
                         "ginkgo/code/acs-mirror-local", "ginkgo/code/hamilton-ultimate",
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
        self.assertEqual(len(syncs), 2)
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
        self.assertEqual(len(checks), 2)
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
                self.runner.snapshots = [] if failure == "empty-snapshots" else [{
                    "id": "a" * 64, "paths": [str(self.home / relative) for relative in GINKGO] + [str(self.stage)],
                }]
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
            result = self.ok(command, "--dry-run")
            self.assertEqual(result["status"], "DRY_RUN")
            self.assertEqual(result["browsable_roots"], ["personal", "Documents"])
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

    def test_retirement_requires_encrypted_snapshot_and_two_browsable_roots(self):
        self.ok("init")
        self.ok("run")
        report = self.ok("verify")
        invalid_reports = []
        for encrypted in (None, False, 1, "true"):
            invalid = json.loads(json.dumps(report))
            if encrypted is None:
                invalid["Snapshot Backup"].pop("encrypted")
            else:
                invalid["Snapshot Backup"]["encrypted"] = encrypted
            invalid_reports.append(invalid)
        invalid = json.loads(json.dumps(report))
        invalid["Snapshot Backup"]["snapshot_count"] = 0
        invalid_reports.append(invalid)
        for missing in ROOTS:
            invalid = json.loads(json.dumps(report))
            invalid["Browsable Sync"]["roots"] = [
                root for root in invalid["Browsable Sync"]["roots"]
                if root["source"] != str(self.home / missing)
            ]
            invalid_reports.append(invalid)
        invalid = json.loads(json.dumps(report))
        invalid["Browsable Sync"]["roots"].extend([
            {"source": str(self.home / root), "remote": f"{BROWSABLE}/{root}", "status": "VERIFIED"}
            for root in GINKGO
        ])
        invalid_reports.append(invalid)
        for invalid in invalid_reports:
            with self.subTest(report=invalid):
                self.report.write_text(json.dumps(invalid))
                self.assertNotEqual(self.cli("retire-legacy-backup", "--execute", "--confirm")[0], 0)
                self.assertEqual(self.rclone_calls("purge"), [])
        self.report.write_text(json.dumps(report))
        self.ok("retire-legacy-backup", "--execute", "--confirm")
        self.assertEqual([call[4] for call in self.rclone_calls("purge")], [LEGACY])

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
