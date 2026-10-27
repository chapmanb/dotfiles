"""Offline CLI safety checks and structural NixOS checks, not a Nix build."""

import contextlib
from datetime import datetime, timedelta, timezone
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

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))
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
BROWSABLE = ("personal", "Documents", "ginkgo/analyses", "ginkgo/methods", "ginkgo/notes")
ASSETS = ("bin/vpnagentd", "bin/vpn", "bin/vpnui", "bin/acwebhelper",
          "vpn/profile/ginkgo-corp-profile.xml", "AnyConnectLocalPolicy.xml")


class OfflineRunner:
    def __init__(self):
        self.calls = []
        self.fail_build = False
        self.mounts = []

    def run(self, args):
        args = list(map(str, args))
        self.calls.append(args)
        if args[0] in ("nix", "nixos-rebuild"):
            if self.fail_build:
                raise subprocess.CalledProcessError(1, args, stderr="private failure")
            output = ""
        elif args == ["findmnt", "--json", "--output", "SOURCE,TARGET"]:
            output = json.dumps({"filesystems": self.mounts})
        elif args[0] == "mv":
            output = ""
        else:
            raise AssertionError(f"Unexpected external command: {args}")
        return subprocess.CompletedProcess(args, 0, output, "")


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
            "[ brave google-chrome wezterm zoom-us ]",
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
        self.stage = self.root / "stage"
        self.receipt = self.root / "verified-archives.json"
        self.receipt.write_text(json.dumps({"version": 1, "items": [{"name": "old", "status": "VERIFIED"}]}))
        self.report = self.root / "verification.json"
        self.good_report = {
            "version": 1, "repository": "rclone:gdrive:bchapman-backup/restic-repo",
            "browsable_remote": "gdrive:bchapman-backup/browsable",
            "verified_at": datetime.now(timezone.utc).isoformat(),
            "Snapshot Backup": {"status": "VERIFIED", "snapshot_count": 1},
            "Browsable Sync": {"status": "VERIFIED", "roots": [
                {"source": str(self.home / root),
                 "remote": f"gdrive:bchapman-backup/browsable/{root}", "status": "VERIFIED"}
                for root in BROWSABLE
            ]},
        }
        self.report.write_text(json.dumps(self.good_report))
        for asset in ASSETS:
            path = self.system / "opt/cisco/secureclient" / asset
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("test-asset")
            path.chmod(0o755)
        self.runner = OfflineRunner()

    def invoke(self, command, *extra, free=150 * 1024 ** 3):
        output, errors = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(output), contextlib.redirect_stderr(errors), patch(
            "dotfiles_migrate.gatekeeper.shutil.disk_usage"
        ) as usage:
            usage.return_value.free = free
            code = main([
                "--home", str(self.home), "--flake", str(ROOT),
                "--receipt", str(self.receipt), "--backup-report", str(self.report),
                "--system-root", str(self.system), "--corp-stage", str(self.stage),
                command, *extra,
            ], runner=self.runner)
            if command == "preflight-stage2":
                usage.assert_called_once_with("/")
        return code, output.getvalue(), errors.getvalue()

    def test_gatekeeper_preflight_checks_and_lvm_runbook(self):
        code, output, errors = self.invoke("preflight-stage2")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["status"], "DRY_RUN")
        self.assertEqual(self.runner.calls, [["nixos-rebuild", "dry-build", "--flake", f"{ROOT}#bchapman-ubnt"]])
        self.assertEqual(self.invoke("preflight-stage2", free=150 * 1024 ** 3 - 1)[0], 1)
        self.receipt.write_text('{"version": 1, "items": [{"name":"old", "status":"UNVERIFIED"}]}')
        self.assertIn("unverified", self.invoke("preflight-stage2")[2])
        self.receipt.unlink()
        self.assertEqual(self.invoke("preflight-stage2")[0], 1)
        self.receipt.write_text('{"version": 1, "items": []}')
        for bad_report in (
            {},
            {**self.good_report, "verified_at": (datetime.now(timezone.utc) - timedelta(hours=49)).isoformat()},
            {**self.good_report, "verified_at": (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat()},
            {**self.good_report, "Snapshot Backup": {"status": "UNVERIFIED"}},
            {**self.good_report, "Browsable Sync": {"status": "VERIFIED", "roots": []}},
        ):
            with self.subTest(report=bad_report):
                self.report.write_text(json.dumps(bad_report))
                self.assertEqual(self.invoke("preflight-stage2")[0], 1)
        self.report.unlink()
        self.assertEqual(self.invoke("preflight-stage2")[0], 1)
        self.report.write_text(json.dumps(self.good_report))
        profile = self.system / "opt/cisco/secureclient/vpn/profile/ginkgo-corp-profile.xml"
        profile.unlink()
        self.assertIn("Cisco", self.invoke("preflight-stage2")[2])
        # A complete restored corporate staging tree is an accepted fallback.
        for asset in ASSETS:
            path = self.stage / "opt/cisco/secureclient" / asset
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("staged-test-asset")
            path.chmod(0o755)
        self.assertEqual(self.invoke("preflight-stage2")[0], 0)
        self.runner.fail_build = True
        code, output, errors = self.invoke("preflight-stage2")
        self.assertEqual(code, 1)
        self.assertNotIn("private failure", errors)
        self.runner.fail_build = False
        code, output, errors = self.invoke("print-stage2-cutover-runbook")
        self.assertEqual(code, 0, errors)
        result = json.loads(output)
        self.assertEqual(result["status"], "DRY_RUN")
        runbook = result["runbook"]
        for command in ("e2fsck -f /dev/vgubuntu/root", "lvreduce --resizefs",
                        "lvcreate -n nixos", "nixos-install --flake", "findmnt -S",
                        "UBUNTU_SIZE_GIB:?", "NIXOS_SIZE_GIB:?", "MANUAL ONLY"):
            self.assertIn(command, runbook)
        self.assertLess(runbook.index("e2fsck -f"), runbook.index("lvreduce --resizefs"))
        self.assertFalse(any(call[0] in ("lvreduce", "lvcreate", "mkfs.ext4", "nixos-install")
                             for call in self.runner.calls))
        self.assertEqual(self.invoke("print-stage2-cutover-runbook", "--execute", "--confirm")[0], 1)

    def test_all_subcommands_default_to_dry_run_and_retirement_is_gated(self):
        for command, flags in (
            ("preflight-stage1", []),
            ("stage1-retire-legacy-managers", ["--stage1-validated"]),
            ("preflight-stage2", []),
            ("print-stage2-cutover-runbook", []),
            ("retire-ubuntu-lv", ["--vpn-validated", "--workflows-validated", "--workspace-migrated"]),
        ):
            with self.subTest(command=command):
                code, output, errors = self.invoke(command, *flags)
                self.assertEqual(code, 0, errors)
                self.assertEqual(json.loads(output)["status"], "DRY_RUN")
        self.assertEqual(self.invoke("retire-ubuntu-lv")[0], 1)
        flags = ["--vpn-validated", "--workflows-validated", "--workspace-migrated"]
        self.runner.mounts = [{"source": "/dev/mapper/vgubuntu-nixos", "target": "/",
                               "children": [{"source": "/dev/mapper/vgubuntu-root[/home/bchapman/ginkgo]",
                                             "target": "/home/bchapman/ginkgo"}]}]
        self.assertIn("still mounted", self.invoke("retire-ubuntu-lv", *flags)[2])
        self.assertEqual(self.invoke("retire-ubuntu-lv", *flags, "--execute", "--confirm")[0], 1)
        self.assertEqual(self.invoke("stage1-retire-legacy-managers", "--execute")[0], 1)
        nvm = self.home / ".nvm"
        nvm.mkdir()
        code, output, errors = self.invoke("stage1-retire-legacy-managers", "--stage1-validated")
        self.assertEqual(code, 0, errors)
        self.assertIn(["mv", "--", str(nvm), str(self.home / ".nvm.retired")],
                      json.loads(output)["quarantine_commands"])
        self.assertFalse(any(call[0] == "mv" for call in self.runner.calls))
        self.assertTrue(nvm.is_dir())
        code, output, errors = self.invoke("stage1-retire-legacy-managers", "--stage1-validated",
                                            "--execute", "--confirm", "--dry-run")
        self.assertEqual(code, 0, errors)
        self.assertEqual(json.loads(output)["status"], "DRY_RUN")
        self.assertFalse(any(call[0] == "mv" for call in self.runner.calls))

    def test_entrypoint_is_executable_and_prints_a_safe_runbook(self):
        result = subprocess.run([str(ROOT / "bin/migration-gatekeeper"), "print-stage2-cutover-runbook"],
                                capture_output=True, text=True, check=True)
        self.assertEqual(json.loads(result.stdout)["status"], "DRY_RUN")
        self.assertIn("lvreduce --resizefs", result.stdout)


if __name__ == "__main__":
    unittest.main()
