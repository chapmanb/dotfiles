"""Offline CLI safety checks and structural NixOS checks, not a Nix build."""

import contextlib
from datetime import datetime, timedelta, timezone
import errno
import io
import json
from pathlib import Path
import re
import ssl
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch
import shutil

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))
from dotfiles_migrate import gatekeeper
from dotfiles_migrate.gatekeeper import main


ROOT = Path(__file__).resolve().parents[1]
CERTIFICATES = (
    "GinkgoBioworks_Root_CA.crt",
    "trust_913614DCD865B47873BD546CD7B31865810DA7B5.crt",
    "trust_A12886C085A0A8F4D234CED936BDEC1767C3F914.crt",
    "trust_C5091132E9ADF8AD3E33932AE60A5C8FA939E824.crt",
)
WORKSPACES = (
    "ginkgo", "personal", "Documents", ".dotfiles", ".ssh", ".gnupg",
    ".password-store", ".aws", ".kube", ".vpn", ".snowflake", ".dbt",
    ".pi", ".claude", ".codex", ".agents", ".gemini",
)
BROWSABLE = ("personal", "Documents")
MYSQL_CLIENT_BINARIES = (
    "mysql", "mysqladmin", "mysqlcheck", "mysqldump", "mysqlimport",
    "mysqlshow", "mysqlslap", "mysql_config_editor", "mysqldumpslow",
)
ASSETS = ("bin/vpnagentd", "bin/vpn", "bin/vpnui", "bin/acwebhelper",
          "vpn/profile/ginkgo-corp-profile.xml", "AnyConnectLocalPolicy.xml")


class OfflineRunner:
    def __init__(self, root, source, activation):
        self.root = root
        self.source = source
        self.activation = activation
        self.calls = []
        self.fail_build = False
        self.inactive_service = None
        self.inactive_state = "inactive"
        self.service_exit_code = 3
        self.purge_output = ""
        self.autoremove_output = ""
        self.late_autoremove_output = ""
        self.purged = False
        self.packages = "snapd\tii \nmysql-server-8.0\tii \ngh\tii \nold-config\trc \nubuntu-desktop\tii \n"

    def run(self, args):
        args = list(map(str, args))
        self.calls.append(args)
        output = ""
        if args[:2] == ["nix", "eval"]:
            output = str(self.source)
        elif args[:2] == ["nix", "build"]:
            if self.fail_build:
                raise subprocess.CalledProcessError(1, args, stderr="private failure")
            output = str(self.activation) + "\n"
        elif args[:2] == ["systemctl", "is-active"]:
            output = self.inactive_state + "\n" if args[2] == self.inactive_service else "active\n"
            if args[2] == self.inactive_service and self.service_exit_code:
                raise subprocess.CalledProcessError(self.service_exit_code, args,
                                                    output=output, stderr="private service detail")
        elif args[0] == "dpkg-query":
            output = self.packages
        elif args[:2] == ["apt-get", "-s"]:
            if args[2] == "purge":
                output = self.purge_output
            elif args[2] == "autoremove":
                output = self.late_autoremove_output if self.purged else self.autoremove_output
            elif args[2] != "install":
                raise AssertionError(args)
        elif args == ["snap", "list", "--all"]:
            output = ("Name Version Rev Tracking Publisher Notes\n"
                      "core20 1 12 latest canonical base\n"
                      "snapd 1 13 latest canonical snapd\n"
                      "firefox 1 14 latest mozilla -\n"
                      "firefox 1 15 latest mozilla disabled\n"
                      "gnome-3-38-2004 1 16 latest canonical -\n")
        elif args[:2] == ["systemctl", "list-units"]:
            output = "mysql.service loaded active running MySQL\nvpnagentd.service loaded active running Cisco\n"
        elif args[:2] == ["sudo", "install"]:
            source, destination = map(Path, args[-2:])
            self.assert_sandbox(destination)
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, destination)
        elif args[:2] == ["sudo", "rm"]:
            for name in args[args.index("--") + 1:]:
                path = Path(name)
                self.assert_sandbox(path)
                if path.is_symlink() or not path.is_dir():
                    path.unlink(missing_ok=True)
                else:
                    shutil.rmtree(path)
        elif args[:3] == ["sudo", "apt-get", "--yes"]:
            if "purge" in args:
                self.purged = True
        elif args[:3] in (["sudo", "systemctl", "stop"], ["sudo", "snap", "remove"]):
            pass
        elif args[:2] == ["sudo", "systemd-tmpfiles"]:
            self.assert_sandbox(Path(args[-1]))
        elif args == ["sudo", "apt-get", "update"] or args[:2] == ["home-manager", "switch"]:
            pass
        else:
            raise AssertionError(f"Unexpected external command: {args}")
        return subprocess.CompletedProcess(args, 0, output, "")

    def assert_sandbox(self, path):
        if not path.is_relative_to(self.root):
            raise AssertionError(f"Refusing live-host fixture mutation: {path}")


class TestNixosStage2AndGatekeeper(unittest.TestCase):
    def test_nixos_user_and_group_uid_10377(self):
        host = (ROOT / "hosts/bchapman-ubnt/default.nix").read_text()
        user = re.search(r"users\.users\.bchapman\s*=\s*\{([^}]+)\};", host)
        self.assertIsNotNone(user)
        self.assertRegex(user.group(1), r"\buid\s*=\s*10377\s*;")
        self.assertRegex(user.group(1), r'\bgroup\s*=\s*"bchapman"\s*;')
        self.assertRegex(host, r"users\.groups\.bchapman\.gid\s*=\s*10377\s*;")

    def test_nixos_host_config_luks_gnome_nix_ld_and_pki(self):
        host = (ROOT / "hosts/bchapman-ubnt/default.nix").read_text()
        flake = (ROOT / "flake.nix").read_text()
        self.assertIn("./hosts/bchapman-ubnt", flake)
        for declaration in (
            "./cisco-vpn.nix", "./bind-mounts.nix",
            "boot.initrd.luks.devices.nvme0n1p3_crypt.device",
            "d03d0617-22d4-4e07-9db5-7fce1563fcc9",
            'fileSystems."/" = { device = "/dev/vgubuntu/nixos";',
            'fileSystems."/mnt/ubuntu"', 'device = "/dev/vgubuntu/root"',
            'options = [ "rw" "nofail" ]', 'device = "/dev/vgubuntu/swap_1"',
            'fileSystems."/boot"', 'fileSystems."/boot/efi"',
            "hardware.cpu.intel.updateMicrocode = true;",
            "services.xserver.displayManager.gdm.enable = true;",
            "services.xserver.desktopManager.gnome.enable = true;",
            'options = "ctrl:nocaps";', "services.pipewire = {", "pulse.enable = true;",
            "virtualisation.docker.enable = true;",
            "[ brave google-chrome zoom-us ]",
            "programs.nix-ld.enable = true;", "services.envfs.enable = true;",
            "stdenv.cc.cc zlib bzip2 xz openssl glib libuuid icu libffi",
            "security.pki.certificateFiles", "security.pki.caCertificateBlacklist",
            '"emSign Root CA - G1"', '"emSign ECC Root CA - G3"',
            'path = "/boot/nixos";', 'efiBootloaderId = "NixOS";',
            "efiInstallAsRemovable = false;", "chainloader /EFI/ubuntu/shimx64.efi",
        ):
            with self.subTest(declaration=declaration):
                self.assertIn(declaration, host)
        self.assertNotIn("snapd", host)
        for certificate in CERTIFICATES:
            self.assertIn(f"../../certs/{certificate}", host)
            data = (ROOT / "certs" / certificate).read_text()
            self.assertNotIn("PRIVATE KEY", data)
            self.assertTrue(ssl.PEM_cert_to_DER_cert(data))
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
            context.load_verify_locations(cafile=str(ROOT / "certs" / certificate))

    def test_cisco_fhs_requires_absolute_browser_launcher(self):
        vpn = (ROOT / "hosts/bchapman-ubnt/cisco-vpn.nix").read_text()
        target = re.search(r"targetPkgs\s*=\s*p:\s*with p;\s*\[(.*?)\];", vpn, re.S)
        self.assertIsNotNone(target)
        packages = re.sub(r"#.*", "", target.group(1)).split()
        self.assertIn("xdg-utils", packages)
        self.assertIn('test -x "$out/usr/bin/xdg-open"', vpn)
        self.assertIn("/opt/.cisco/certificates/ca", vpn)

    def test_cisco_vpn_module_and_workspace_bind_mounts(self):
        vpn = (ROOT / "hosts/bchapman-ubnt/cisco-vpn.nix").read_text()
        for declaration in (
            'boot.kernelModules = [ "tun" ];', "pkgs.buildFHSEnv", "Cisco Secure Client 5.1",
            "/mnt/ubuntu/opt/cisco/secureclient",
            "/var/lib/cisco-secureclient-staging/opt/cisco/secureclient",
            "ginkgo-corp-profile.xml", "AnyConnectLocalPolicy.xml", "acwebhelper",
            "systemd.services.vpnagentd", "/opt/cisco/secureclient/bin/vpnagentd -execv_instance",
            'pkgs.writeShellScriptBin "vpn"', 'pkgs.writeShellScriptBin "vpnui"',
            "pkgs.makeDesktopItem", 'unitConfig.RequiresMountsFor = [ "/mnt/ubuntu" ];',
            'requires = [ "cisco-secureclient-hydrate.service" ];',
            "systemd.tmpfiles.rules", 'cp -a "$source"', 'chown -R root:root',
        ):
            with self.subTest(declaration=declaration):
                self.assertIn(declaration, vpn)
        self.assertNotRegex(vpn, r"(?m)^\s*requiresMountsFor\s*=")
        mounts = (ROOT / "hosts/bchapman-ubnt/bind-mounts.nix").read_text()
        directories = re.search(r"directories = \[(.*?)\];", mounts, re.S).group(1)
        self.assertEqual(set(re.findall(r'"([^"]+)"', directories)), set(WORKSPACES))
        self.assertIn('"/home/bchapman/${dir}"', mounts)
        self.assertIn('"/mnt/ubuntu/home/bchapman/${baseNameOf target}"', mounts)
        self.assertIn('fsType = "none";', mounts)
        self.assertIn('"bind" "x-systemd.requires-mounts-for=/mnt/ubuntu"', mounts)
        self.assertIn('depends = [ "/mnt/ubuntu" ];', mounts)

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="test-gatekeeper-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.home = self.root / "home"
        self.home.mkdir()
        self.system = self.root / "system"
        self.source = self.root / "store-source"
        self.source.mkdir()
        self.activation = self.root / "activation"
        self.report = self.root / "verification.json"
        self.good_report = {
            "version": 1, "repository": "rclone:gdrive:bchapman-backup/restic-repo",
            "browsable_remote": "gdrive:bchapman-backup/browsable",
            "verified_at": datetime.now(timezone.utc).isoformat(),
            "Snapshot Backup": {"status": "VERIFIED", "encrypted": True, "snapshot_count": 1},
            "Browsable Sync": {"status": "VERIFIED", "roots": [
                {"source": str(self.home / root),
                 "remote": f"gdrive:bchapman-backup/browsable/{root}", "status": "VERIFIED"}
                for root in BROWSABLE
            ]},
        }
        self.report.write_text(json.dumps(self.good_report))
        for asset in ASSETS:
            self.make_file(self.system / "opt/cisco/secureclient" / asset)
        self.make_file(self.system / "usr/local/share/ca-certificates/ginkgo.crt")
        for binary in gatekeeper.MIGRATED_BINARIES:
            self.make_file(self.activation / "home-path/bin" / binary)
        self.runner = OfflineRunner(self.root, self.source, self.activation)

    def make_file(self, path, content="test-asset\n"):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        path.chmod(0o755)
        return path

    def invoke(self, command, *extra):
        output, errors = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(output), contextlib.redirect_stderr(errors):
            code = main([
                "--home", str(self.home), "--flake", str(ROOT),
                "--backup-report", str(self.report), "--system-root", str(self.system),
                command, *extra,
            ], runner=self.runner)
        return code, output.getvalue(), errors.getvalue()

    def assert_no_host_mutations(self):
        self.assertFalse(any(call[0] in ("sudo", "home-manager") for call in self.runner.calls))

    def test_gatekeeper_requires_encrypted_snapshot_backup_and_two_browsable_roots(self):
        self.assertEqual(gatekeeper.BROWSABLE_ROOTS, ("personal", "Documents"))
        code, output, errors = self.invoke("preflight-ubuntu")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["daily_backup"], "VERIFIED")
        invalid_reports = []
        for encrypted in (None, False, 1, "true"):
            report = json.loads(json.dumps(self.good_report))
            if encrypted is None:
                report["Snapshot Backup"].pop("encrypted")
            else:
                report["Snapshot Backup"]["encrypted"] = encrypted
            invalid_reports.append(report)
        for missing in BROWSABLE:
            report = json.loads(json.dumps(self.good_report))
            report["Browsable Sync"]["roots"] = [
                root for root in report["Browsable Sync"]["roots"]
                if root["source"] != str(self.home / missing)
            ]
            invalid_reports.append(report)
        report = json.loads(json.dumps(self.good_report))
        report["Browsable Sync"]["roots"].extend([
            {"source": str(self.home / root),
             "remote": f"gdrive:bchapman-backup/browsable/{root}", "status": "VERIFIED"}
            for root in ("ginkgo/analyses", "ginkgo/methods", "ginkgo/notes")
        ])
        invalid_reports.append(report)
        for report in invalid_reports:
            for command in ("preflight-ubuntu", "clean-user-space", "activate-host-bridge",
                            "simplify-ubuntu-host", "prune-ubuntu-duplicates"):
                with self.subTest(report=report, command=command):
                    self.report.write_text(json.dumps(report))
                    self.runner.calls.clear()
                    self.assertEqual(self.invoke(command, "--execute", "--confirm")[0], 1)
                    self.assertEqual(self.runner.calls, [])

    def test_preflight_ubuntu_verifies_git_flake_source_corp_services_and_nix_binaries(self):
        self.assertEqual(set(gatekeeper.MIGRATED_BINARIES), {
            "ghostty", "brave", "libreoffice", "inkscape", "redshift", "redshift-gtk",
            "gcloud", "gh", "task", "glow", "lazygit", "gitu", "mdbook", "pandoc",
            "clj-kondo", "clojure-lsp", "kubectl", "helm", "k9s", "argo", "terraform",
            "terraform-docs", "pre-commit", "ffmpeg", "dot", "magick", "qpdf", "sqlite3",
            "ncdu", "parallel", "datamash", "tree", "fx", *MYSQL_CLIENT_BINARIES,
        })
        self.assertEqual(gatekeeper.CORPORATE_SERVICES, (
            "centrifydc", "cisco-amp", "vpnagentd", "ir_agent", "drivestrike",
            "connectwisecontrol-a8194903625f6c0d",
        ))
        for bad_report in (
            {**self.good_report, "verified_at": (datetime.now(timezone.utc) - timedelta(hours=48)).isoformat()},
            {**self.good_report, "Snapshot Backup": {"status": "UNVERIFIED"}},
            {**self.good_report, "Browsable Sync": {"status": "UNVERIFIED"}},
        ):
            self.report.write_text(json.dumps(bad_report))
            self.assertEqual(self.invoke("preflight-ubuntu")[0], 1)
            self.assertEqual(self.runner.calls, [])
        self.report.write_text(json.dumps(self.good_report))
        code, output, errors = self.invoke("preflight-ubuntu")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["status"], "DRY_RUN")
        uri = "git+" + ROOT.as_uri()
        self.assertIn(["nix", "eval", "--raw", "--impure", "--expr",
                       f'(builtins.getFlake {json.dumps(uri)}).outPath'], self.runner.calls)
        self.assertIn(["nix", "build", "--no-link", "--print-out-paths",
                       f'{uri}#homeConfigurations."bchapman@bchapman-ubnt".activationPackage'], self.runner.calls)
        self.assertEqual([call[2] for call in self.runner.calls if call[:2] == ["systemctl", "is-active"]],
                         list(gatekeeper.CORPORATE_SERVICES))
        for name in gatekeeper.MIGRATED_BINARIES:
            binary = self.activation / "home-path/bin" / name
            binary.chmod(0o644)
            with self.subTest(missing_binary=name):
                self.assertIn(name, self.invoke("preflight-ubuntu")[2])
            binary.chmod(0o755)
        scratch = self.source / "tmp"
        scratch.mkdir()
        self.runner.calls.clear()
        self.assertIn("tmp", self.invoke("preflight-ubuntu")[2])
        self.assertFalse(any(call[:2] == ["nix", "build"] for call in self.runner.calls))
        scratch.rmdir()
        scratch.symlink_to(self.source / "missing")
        self.assertIn("tmp", self.invoke("preflight-ubuntu")[2])
        scratch.unlink()
        for service in gatekeeper.CORPORATE_SERVICES:
            self.runner.inactive_service = service
            self.assertIn(service, self.invoke("preflight-ubuntu")[2])
        self.runner.inactive_service = None
        for asset in ASSETS:
            path = self.system / "opt/cisco/secureclient" / asset
            content = path.read_text()
            path.write_text("")
            self.assertIn("Cisco", self.invoke("preflight-ubuntu")[2])
            path.write_text(content)
        (self.system / "usr/local/share/ca-certificates/ginkgo.crt").unlink()
        self.assertIn("CA certificates", self.invoke("preflight-ubuntu")[2])
        self.assert_no_host_mutations()
        self.runner.fail_build = True
        code, _, errors = self.invoke("preflight-ubuntu")
        self.assertEqual(code, 1)
        self.assertNotIn("private failure", errors)

    def test_mysql_replacement_commands_required_by_preflight(self):
        self.assertEqual(len(MYSQL_CLIENT_BINARIES), 9)
        self.assertNotIn("mysqlpump", gatekeeper.MIGRATED_BINARIES)
        self.assertTrue(set(MYSQL_CLIENT_BINARIES).issubset(gatekeeper.MIGRATED_BINARIES))
        code, output, errors = self.invoke("preflight-ubuntu")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["home_manager_profile"],
                         str(self.activation / "home-path"))
        for name in MYSQL_CLIENT_BINARIES:
            binary = self.activation / "home-path/bin" / name
            for unavailable in ("missing", "not-executable"):
                with self.subTest(binary=name, unavailable=unavailable):
                    if unavailable == "missing":
                        binary.unlink()
                    else:
                        binary.chmod(0o644)
                    self.runner.calls.clear()
                    code, _, errors = self.invoke("preflight-ubuntu", "--execute", "--confirm")
                    self.assertEqual(code, 1)
                    self.assertIn(f"Missing migrated Nix binaries: {name}", errors)
                    self.assert_no_host_mutations()
                    self.make_file(binary)
        self.assertEqual(self.invoke("preflight-ubuntu")[0], 0)
        self.assert_no_host_mutations()

    def test_explicit_drivestrike_health_exception_preserves_other_gates(self):
        flag = "--allow-inactive-drivestrike"
        self.runner.inactive_service = "drivestrike"
        for state in ("inactive", "failed", "activating", "deactivating"):
            self.runner.inactive_state = state
            for command in ("preflight-ubuntu", "activate-host-bridge", "simplify-ubuntu-host"):
                with self.subTest(state=state, command=command):
                    self.assertEqual(self.invoke(command)[0], 1)
                    code, output, errors = self.invoke(command, flag)
                    self.assertEqual(code, 0, errors)
                    result = json.loads(output)
                    self.assertEqual(result["corporate_endpoint_stack"], "VERIFIED_WITH_EXCEPTIONS")
                    self.assertEqual(result["corporate_service_exceptions"], {"drivestrike": state})
                    self.assertEqual(result["daily_backup"], "VERIFIED")
        self.assert_no_host_mutations()

        for state, exit_code in (("unknown", 4), ("activating", 1), ("unknown", 3), ("", 3)):
            with self.subTest(state=state, exit_code=exit_code):
                self.runner.inactive_state = state
                self.runner.service_exit_code = exit_code
                code, _, errors = self.invoke("preflight-ubuntu", flag)
                self.assertEqual(code, 1)
                self.assertNotIn("private service detail", errors)
        self.runner.service_exit_code = 3
        self.runner.inactive_state = "inactive"
        for service in gatekeeper.CORPORATE_SERVICES:
            if service != "drivestrike":
                self.runner.inactive_service = service
                self.assertIn(service, self.invoke("preflight-ubuntu", flag)[2])

        self.runner.inactive_service = None
        for arguments in (("preflight-ubuntu", flag), (flag, "preflight-ubuntu"), ("preflight-ubuntu",)):
            code, output, errors = self.invoke(*arguments)
            self.assertEqual(code, 0, errors)
            self.assertEqual(json.loads(output)["corporate_endpoint_stack"], "VERIFIED")
            self.assertEqual(json.loads(output)["corporate_service_exceptions"], {})

        self.runner.inactive_service = "drivestrike"
        self.report.write_text(json.dumps({**self.good_report, "Snapshot Backup": {"status": "UNVERIFIED"}}))
        self.runner.calls.clear()
        self.assertEqual(self.invoke("preflight-ubuntu", flag)[0], 1)
        self.assertEqual(self.runner.calls, [])
        self.report.write_text(json.dumps(self.good_report))
        asset = self.system / "opt/cisco/secureclient/bin/vpnagentd"
        asset.write_text("")
        self.assertIn("Cisco", self.invoke("preflight-ubuntu", flag)[2])
        self.make_file(asset)
        certificate = self.system / "usr/local/share/ca-certificates/ginkgo.crt"
        certificate.unlink()
        self.assertIn("CA certificates", self.invoke("preflight-ubuntu", flag)[2])
        self.make_file(certificate)
        binary = self.activation / "home-path/bin/ghostty"
        binary.unlink()
        self.assertIn("ghostty", self.invoke("preflight-ubuntu", flag)[2])
        self.make_file(binary)

        self.runner.purge_output = "Remv drivestrike [1.0]\n"
        code, _, errors = self.invoke("simplify-ubuntu-host", flag, "--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertIn("Protected Host Package Guard refused removal of drivestrike", errors)
        self.assertIn("drivestrike.list", gatekeeper.APPROVED_APT_SOURCES)
        self.assert_no_host_mutations()

        code, output, errors = self.invoke("activate-host-bridge", flag, "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["corporate_service_exceptions"], {"drivestrike": "inactive"})
        self.assertTrue(any(call[:2] == ["home-manager", "switch"] for call in self.runner.calls))
        self.assertFalse(any(call[:2] == ["sudo", "systemctl"] for call in self.runner.calls))

    def test_all_commands_require_fresh_complete_daily_backup_before_external_commands(self):
        for bad_report in (
            {},
            {**self.good_report, "repository": "wrong"},
            {**self.good_report, "verified_at": (datetime.now(timezone.utc) - timedelta(hours=48)).isoformat()},
            {**self.good_report, "verified_at": (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat()},
            {**self.good_report, "verified_at": datetime.now().isoformat()},
            {**self.good_report, "Snapshot Backup": {"status": "UNVERIFIED"}},
            {**self.good_report, "Snapshot Backup": {"status": "VERIFIED", "snapshot_count": 0}},
            {**self.good_report, "Browsable Sync": {"status": "UNVERIFIED"}},
            {**self.good_report, "Browsable Sync": {"status": "VERIFIED", "roots": []}},
        ):
            self.report.write_text(json.dumps(bad_report))
            for command in ("preflight-ubuntu", "clean-user-space", "activate-host-bridge", "simplify-ubuntu-host"):
                with self.subTest(report=bad_report, command=command):
                    self.runner.calls.clear()
                    self.assertEqual(self.invoke(command, "--execute", "--confirm")[0], 1)
                    self.assertEqual(self.runner.calls, [])
        self.report.unlink()
        self.assertEqual(self.invoke("preflight-ubuntu")[0], 1)

    def test_clean_user_space_reclaims_legacy_dirs_and_preserves_active_launchers(self):
        self.assertEqual(gatekeeper.PRESERVED_LAUNCHERS, {
            "claude", "codex", "herdr", "pibox", "nori-auth", "skills-curl", "skills-curl.ps1",
            "catalyst-agent-hook-runner", "catalyst-agent-skills-auth", "keyring", "poetry",
            "gws", "fix_docker_vpn.sh", "fix_docker_vpn_chris.sh", "python3.12",
        })
        retired = [self.home / path for path in (
            ".nvm.retired", "install", ".local/lib/python3.8", ".local/opt/wezterm",
            ".dotfiles/tmp/cisco-vm/runtime",
        )]
        for directory in retired:
            self.make_file(directory / "data")
        disk = self.make_file(self.home / ".dotfiles/tmp/cisco-vm/cisco-probe.qcow2")
        retained = self.make_file(self.home / ".dotfiles/tmp/cisco-vm/notes")
        local_bin = self.home / ".local/bin"
        for name in gatekeeper.PRESERVED_LAUNCHERS - set(gatekeeper.RELOCATED_SCRIPTS):
            self.make_file(local_bin / name, "#!/usr/bin/python3.8\nactive launcher\n")
        stale = [self.make_file(local_bin / name, shebang) for name, shebang in (
            ("old-pip-tool", "#!/usr/bin/python3.8\n"),
            ("old-pip310", "#!/usr/bin/python3.10\n"),
            ("old-focal-pip", "#!/usr/bin/python3\n"),
            ("agy", "#!/bin/sh\n"), ("ruff", "binary\n"),
            ("wezterm", "binary\n"), ("wezterm-gui", "binary\n"),
            ("wezterm-mux-server", "binary\n"),
        )]
        custom = self.make_file(local_bin / "custom-script", "#!/bin/sh\nkeep me\n")
        new_python = self.make_file(local_bin / "new-python-tool", "#!/usr/bin/python3.12\n")
        for name in gatekeeper.RELOCATED_SCRIPTS:
            self.make_file(self.system / "usr/local/bin" / name, f"#!/bin/sh\n{name}\n")
        bashrc = self.make_file(self.home / ".bashrc.local", (
            "export KEEP=yes\nsource ~/install/system/google-cloud-sdk/path.bash.inc\n"
            "source /home/bchapman/install/system/google-cloud-sdk/completion.bash.inc\n# keep this\n"))
        code, output, errors = self.invoke("clean-user-space")
        self.assertEqual(code, 0, errors)
        self.assertEqual(len(json.loads(output)["relocations"]), 3)
        self.assertTrue(all(path.exists() for path in [*retired, disk, *stale]))
        self.assertIn("google-cloud-sdk", bashrc.read_text())
        self.assertEqual(self.invoke("clean-user-space", "--execute")[0], 1)
        self.report.write_text(json.dumps({**self.good_report, "verified_at":
            (datetime.now(timezone.utc) - timedelta(hours=49)).isoformat()}))
        self.assertEqual(self.invoke("clean-user-space", "--execute", "--confirm")[0], 1)
        self.assertTrue(all(path.exists() for path in [*retired, disk, *stale]))
        self.report.write_text(json.dumps(self.good_report))
        ghostty = self.activation / "home-path/bin/ghostty"
        ghostty.unlink()
        self.assertIn("ghostty", self.invoke("clean-user-space", "--execute", "--confirm")[2])
        self.assertTrue(all(path.exists() for path in [*retired, disk, *stale]))
        self.make_file(ghostty)
        code, _, errors = self.invoke("clean-user-space", "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertFalse(any(path.exists() for path in [*retired, disk, *stale]))
        self.assertEqual(bashrc.read_text(), "export KEEP=yes\n# keep this\n")
        for name in gatekeeper.PRESERVED_LAUNCHERS:
            self.assertTrue((local_bin / name).exists(), name)
        for name in gatekeeper.RELOCATED_SCRIPTS:
            self.assertFalse((self.system / "usr/local/bin" / name).exists())
            self.assertTrue((local_bin / name).stat().st_mode & 0o111)
        self.assertTrue(all(path.exists() for path in (custom, new_python, retained)))
        self.assertEqual(self.invoke("clean-user-space", "--execute", "--confirm")[0], 0)
        self.assert_no_host_mutations()

    def test_cleanup_preserves_existing_destinations_and_does_not_follow_retired_symlinks(self):
        source = self.make_file(self.system / "usr/local/bin/gws", "old\n")
        destination = self.make_file(self.home / ".local/bin/gws", "new\n")
        outside = self.make_file(self.root / "outside/data")
        (self.home / ".nvm.retired").symlink_to(outside.parent, target_is_directory=True)
        code, _, errors = self.invoke("clean-user-space", "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual(destination.read_text(), "new\n")
        self.assertTrue(source.exists())
        self.assertTrue(outside.exists())
        self.assertFalse((self.home / ".nvm.retired").is_symlink())

    def test_read_only_host_script_sources_are_reported_for_deferred_host_cleanup(self):
        source = self.make_file(self.system / "usr/local/bin/gws", "#!/bin/sh\nactive\n")
        unlink = Path.unlink

        def deny_source_unlink(path, *args, **kwargs):
            if path == source:
                raise PermissionError(errno.EACCES, "read-only host mount")
            return unlink(path, *args, **kwargs)

        with patch.object(Path, "unlink", deny_source_unlink):
            code, output, errors = self.invoke("clean-user-space", "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["source_removal_deferred_to_host_cleanup"], [str(source)])
        self.assertEqual((self.home / ".local/bin/gws").read_text(), source.read_text())
        self.assert_no_host_mutations()

    def test_activate_host_bridge_and_simplify_ubuntu_host_protected_package_guard(self):
        self.assertEqual(gatekeeper.APPROVED_APT_SOURCES, {
            "artifactory-centrify2.list",
            "artifactory_releng_ginkgo_zone_artifactory_ubuntu_local.list",
            "drivestrike.list", "docker.list", "google-chrome.sources",
            "oem-sutton.newell-aekerley-meta.list",
        })
        self.assertEqual(gatekeeper.PROTECTED_HOST_PACKAGES, {
            "ubuntu-desktop", "gdm3", "gnome-shell", "gnome-terminal", "network-manager",
            "docker-ce", "docker-ce-cli", "containerd.io", "docker-buildx-plugin",
            "docker-compose-plugin", "openssh-server", "google-chrome-stable", "zoom",
            "centrifydc", "centrifydc-openssl", "centrifydc-openldap", "centrifydc-curl",
            "ciscoampconnector", "cisco-orbital", "drivestrike",
            "connectwisecontrol-a8194903625f6c0d", "dev42-lx-agent", "auditd", "lvm2",
            "cryptsetup", "cryptsetup-bin", "cryptsetup-initramfs", "grub-common",
            "grub-efi-amd64-signed", "shim-signed", "linux-image-generic-hwe-20.04",
            "intel-microcode", "oem-sutton.newell-meta", "oem-sutton.newell-aekerley-meta",
        })
        config = self.system / gatekeeper.BRIDGE_CONFIG
        code, output, errors = self.invoke("activate-host-bridge")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["write"][str(config)], gatekeeper.BRIDGE_CONTENT)
        self.assertFalse(config.exists())
        self.assert_no_host_mutations()
        code, _, errors = self.invoke("activate-host-bridge", "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual(config.read_text(),
                         "L+ /run/opengl-driver - - - - /home/bchapman/.local/state/nix-opengl-driver\n")
        tmpfiles = ["sudo", "systemd-tmpfiles", "--create", str(config)]
        switch = ["home-manager", "switch", "--flake", f"git+{ROOT.as_uri()}#bchapman@bchapman-ubnt"]
        self.assertLess(self.runner.calls.index(tmpfiles), self.runner.calls.index(switch))
        self.runner.calls.clear()
        sources = self.system / "etc/apt/sources.list.d"
        for name in gatekeeper.APPROVED_APT_SOURCES:
            self.make_file(sources / name)
        retired_sources = [self.make_file(sources / f"retired-{i}.list") for i in range(18)]
        backup_source = self.make_file(sources / "docker.list.save")
        host_dirs = [self.system / path for path in (
            "home/linuxbrew.retired", "usr/local/aws-cli", "usr/local/share/Antigravity-x64",
        )]
        for directory in host_dirs:
            self.make_file(directory / "data")
        binary = self.make_file(self.system / "usr/local/bin/bb")
        dangling = self.system / "usr/local/bin/dangling"
        dangling.symlink_to(self.root / "absent")
        active_binary = self.make_file(self.system / "usr/local/bin/company-tool")
        code, output, errors = self.invoke("simplify-ubuntu-host")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["status"], "DRY_RUN")
        self.assertIn(["apt-get", "-s", "purge", "snapd", "mysql-server-8.0", "gh"], self.runner.calls)
        self.assertIn(["apt-get", "-s", "autoremove", "--purge"], self.runner.calls)
        self.assert_no_host_mutations()
        self.assertTrue(all(path.exists() for path in retired_sources + host_dirs))
        for package in gatekeeper.PROTECTED_HOST_PACKAGES:
            for action in ("Remv", "Purg"):
                for phase in ("purge_output", "autoremove_output"):
                    with self.subTest(package=package, action=action, phase=phase):
                        self.runner.calls.clear()
                        setattr(self.runner, phase, f"{action} {package}:amd64 [1.0]\n")
                        code, _, errors = self.invoke("simplify-ubuntu-host", "--execute", "--confirm")
                        self.assertEqual(code, 1)
                        self.assertIn("Protected Host Package Guard", errors)
                        self.assert_no_host_mutations()
                        setattr(self.runner, phase, "")
        self.runner.calls.clear()
        code, _, errors = self.invoke("simplify-ubuntu-host", "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual({path.name for path in sources.iterdir()}, gatekeeper.APPROVED_APT_SOURCES)
        self.assertFalse(any(path.exists() for path in [*host_dirs, binary, backup_source]))
        self.assertFalse(dangling.is_symlink())
        self.assertTrue(active_binary.exists())
        self.assertEqual((self.system / "etc/apt/preferences.d/no-snapd.pref").read_text(),
                         "Package: snapd\nPin: release *\nPin-Priority: -10\n")
        snap_calls = [call for call in self.runner.calls if call[:3] == ["sudo", "snap", "remove"]]
        self.assertEqual([call[-1] for call in snap_calls],
                         ["firefox", "gnome-3-38-2004", "core20", "snapd"])
        self.assertTrue(all("--purge" in call for call in snap_calls))
        self.assertIn(["sudo", "systemctl", "stop", "mysql.service"], self.runner.calls)
        self.assertIn(["sudo", "apt-get", "--yes", "purge", "snapd", "mysql-server-8.0", "gh"], self.runner.calls)
        self.assertIn(["sudo", "apt-get", "--yes", "install", "--allow-downgrades",
                       "git=1:2.25.1-1ubuntu3.14", "git-man=1:2.25.1-1ubuntu3.14"], self.runner.calls)
        self.assertIn(["sudo", "apt-get", "--yes", "purge", "old-config"], self.runner.calls)
        autoremove = ["sudo", "apt-get", "--yes", "autoremove", "--purge"]
        index = self.runner.calls.index(autoremove)
        self.assertEqual(self.runner.calls[index - 1], ["apt-get", "-s", "autoremove", "--purge"])

    def test_autoremove_is_reguarded_after_purge(self):
        self.runner.late_autoremove_output = "Remv docker-ce [1.0]\n"
        code, _, errors = self.invoke("simplify-ubuntu-host", "--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertIn("docker-ce", errors)
        self.assertNotIn(["sudo", "apt-get", "--yes", "autoremove", "--purge"], self.runner.calls)

    def test_protected_residual_configs_are_preserved_not_purged(self):
        protected = ["linux-image-5.14.0-1036-oem", "drivestrike", "ubuntu-desktop"]
        self.runner.packages = "old-config\trc \n" + "".join(f"{name}\trc \n" for name in protected)
        code, output, errors = self.invoke("simplify-ubuntu-host")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["preserved_protected_residuals"], protected)
        self.assertIn(["apt-get", "-s", "purge", "old-config"], self.runner.calls)
        self.assert_no_host_mutations()

        self.runner.calls.clear()
        initial = (["gh"], [protected[0], "old-config"])
        later = ([], [*protected, "old-config", "new-config"])
        with patch.object(gatekeeper, "installed_packages", side_effect=[initial, later]):
            code, output, errors = self.invoke("simplify-ubuntu-host", "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["preserved_protected_residuals"], protected)
        self.assertIn(["sudo", "apt-get", "--yes", "purge", "old-config", "new-config"], self.runner.calls)
        for call in self.runner.calls:
            if "purge" in call:
                self.assertFalse(set(protected).intersection(call), call)

        self.runner = OfflineRunner(self.root, self.source, self.activation)
        self.runner.packages = "drivestrike\trc \n"
        code, output, errors = self.invoke("simplify-ubuntu-host", "--execute", "--confirm")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["preserved_protected_residuals"], ["drivestrike"])
        self.assertFalse(any("purge" in call for call in self.runner.calls))

        self.runner = OfflineRunner(self.root, self.source, self.activation)
        self.runner.purge_output = "Purg ubuntu-desktop [1.0]\n"
        code, _, errors = self.invoke("simplify-ubuntu-host", "--execute", "--confirm")
        self.assertEqual(code, 1)
        self.assertIn("Protected Host Package Guard", errors)
        self.assert_no_host_mutations()

    def test_all_commands_default_to_dry_run_and_require_confirmation(self):
        for command in ("preflight-ubuntu", "clean-user-space", "activate-host-bridge", "simplify-ubuntu-host"):
            for flags in ((), ("--execute", "--confirm", "--dry-run")):
                with self.subTest(command=command, flags=flags):
                    self.runner.calls.clear()
                    code, output, errors = self.invoke(command, *flags)
                    self.assertEqual(code, 0, errors)
                    self.assertEqual(json.loads(output)["status"], "DRY_RUN")
                    self.assert_no_host_mutations()
            self.runner.calls.clear()
            self.assertEqual(self.invoke(command, "--execute")[0], 1)
            self.assertEqual(self.runner.calls, [])
        self.make_file(self.system / "usr/local/bin/gws")
        self.assertIn("clean-user-space", self.invoke("simplify-ubuntu-host")[2])

    def test_entrypoint_is_executable_and_retires_lvm_commands(self):
        result = subprocess.run([str(ROOT / "bin/migration-gatekeeper"), "--help"],
                                capture_output=True, text=True, check=True)
        for command in ("preflight-ubuntu", "clean-user-space", "activate-host-bridge", "simplify-ubuntu-host"):
            self.assertIn(command, result.stdout)
        self.assertNotIn("stage2", result.stdout)
        result = subprocess.run([str(ROOT / "bin/migration-gatekeeper"), "retire-ubuntu-lv"],
                                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)


if __name__ == "__main__":
    unittest.main()
