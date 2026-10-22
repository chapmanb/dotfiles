"""Read-only migration preflights and explicit, human-operated cutover runbooks."""

import argparse
from datetime import datetime, timedelta, timezone
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys

from .cold_archive import ArchiveError, read_receipts
from .daily_backup import BROWSABLE_REMOTE, BROWSABLE_ROOTS, RESTIC_REPOSITORY
from .runner import CommandRunner


MIN_FREE_BYTES = 150 * 1024 ** 3
MAX_BACKUP_AGE = timedelta(hours=48)
CLIENT_ASSETS = (
    "bin/vpnagentd", "bin/vpn", "bin/vpnui", "bin/acwebhelper",
    "vpn/profile/ginkgo-corp-profile.xml", "AnyConnectLocalPolicy.xml",
)


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
    if age < timedelta(0) or age > MAX_BACKUP_AGE:
        raise GateError("Daily-backup verification is stale or future-dated (maximum age: 48 hours)")
    snapshot = report.get("Snapshot Backup", {})
    browsable = report.get("Browsable Sync", {})
    if (snapshot.get("status") != "VERIFIED" or snapshot.get("snapshot_count", 0) < 1
            or browsable.get("status") != "VERIFIED"):
        raise GateError("Daily-backup Snapshot Backup and Browsable Sync must be VERIFIED")
    expected = {(str(home / root), f"{BROWSABLE_REMOTE}/{root}", "VERIFIED")
                for root in BROWSABLE_ROOTS}
    actual = {(root.get("source"), root.get("remote"), root.get("status"))
              for root in browsable.get("roots", [])}
    if actual != expected:
        raise GateError("Daily-backup verification does not cover every browsable root")


def require_receipts(receipt):
    if not receipt.is_file():
        raise GateError("Missing verified-archives.json")
    if any(item.get("status") != "VERIFIED" for item in read_receipts(receipt)):
        raise GateError("verified-archives.json contains unverified entries")


def require_client(candidates):
    for client in candidates:
        if all((client / asset).is_file() and (client / asset).stat().st_size > 0
               and (not asset.startswith("bin/") or os.access(client / asset, os.X_OK))
               for asset in CLIENT_ASSETS):
            return str(client)
    raise GateError("Cisco Secure Client 5.1 assets are missing; stage binaries, policy, and ginkgo-corp-profile.xml")


def preflight_stage1(args, runner):
    runner.run(["nix", "build", "--no-link",
                f"{args.flake}#homeConfigurations.bchapman@bchapman-ubnt.activationPackage"])
    return {"home_manager_build": "PASSED"}


def preflight_stage2(args, runner):
    # The gate is deliberately on the running Ubuntu root, not a configurable
    # scratch path that might reside on another filesystem.
    free = shutil.disk_usage("/").free
    if free < MIN_FREE_BYTES:
        raise GateError(f"Free space on / is below 150 GiB ({free / 1024 ** 3:.1f} GiB)")
    require_receipts(args.receipt)
    require_backup(args.backup_report, args.home)
    client = require_client([
        args.system_root / "opt/cisco/secureclient",
        args.corp_stage / "opt/cisco/secureclient",
    ])
    runner.run(["nixos-rebuild", "dry-build", "--flake", f"{args.flake}#bchapman-ubnt"])
    return {"free_bytes": free, "archives": "VERIFIED", "daily_backup": "VERIFIED",
            "cisco_assets": client, "nixos_dry_build": "PASSED"}


def cutover_runbook(args):
    flake = shlex.quote(f"{args.flake}#bchapman-ubnt")
    return f"""# MANUAL ONLY: rerun preflight-stage2 successfully on Ubuntu immediately before shutdown.
# Boot a NixOS live USB in UEFI mode; use a root HOST shell, never pibox.
# Keep an off-disk verified restore and boot/EFI backup. Do not format /boot or the ESP.
cryptsetup open /dev/disk/by-uuid/d03d0617-22d4-4e07-9db5-7fce1563fcc9 nvme0n1p3_crypt
vgchange -ay vgubuntu
lsblk -f
lvs -o lv_name,lv_size,vg_free
# STOP if /dev/vgubuntu/root is mounted anywhere (including live-USB automounts).
findmnt -S /dev/vgubuntu/root
# Inspect minimum size and choose UBUNTU_SIZE_GIB with ample headroom above used data.
# Choose NIXOS_SIZE_GIB within the resulting VG free space. No sizes are guessed here.
resize2fs -P /dev/vgubuntu/root
: "${{UBUNTU_SIZE_GIB:?Set reviewed Ubuntu target GiB}}"
: "${{NIXOS_SIZE_GIB:?Set reviewed NixOS target GiB}}"
e2fsck -f /dev/vgubuntu/root
# STOP on errors; never pass --force or --yes to lvreduce.
lvreduce --resizefs -L "${{UBUNTU_SIZE_GIB}}G" /dev/vgubuntu/root
lvcreate -n nixos -L "${{NIXOS_SIZE_GIB}}G" vgubuntu
# Confirm this is the NEW LV before formatting. Never format vgubuntu/root.
mkfs.ext4 /dev/vgubuntu/nixos
mount /dev/vgubuntu/nixos /mnt
mkdir -p /mnt/boot /mnt/mnt/ubuntu
mount /dev/disk/by-uuid/2b2de632-f678-4b85-9f70-580470793b52 /mnt/boot
mkdir -p /mnt/boot/efi /mnt/boot/nixos
mount /dev/disk/by-uuid/F64B-C7D5 /mnt/boot/efi
mount /dev/vgubuntu/root /mnt/mnt/ubuntu
# Make the flake available at the reviewed path below in the live environment.
nixos-install --flake {flake}
# Set a login password before reboot: nixos-enter --root /mnt -c 'passwd bchapman'
# Validate BOTH EFI boot entries, GNOME, VPN login/DNS/routes, uv/pixi, agents,
# bind mounts and a fresh daily-backup verify before considering Ubuntu retirement.
# Workspace data still lives on Ubuntu: migrate it and remove bind dependencies FIRST.
"""


def retirement_runbook(args, runner):
    require_backup(args.backup_report, args.home)
    if not (args.vpn_validated and args.workflows_validated and args.workspace_migrated):
        raise GateError("Ubuntu retirement requires --vpn-validated --workflows-validated --workspace-migrated")
    mounts = json.loads(runner.run(["findmnt", "--json", "--output", "SOURCE,TARGET"]).stdout)

    def uses_ubuntu(filesystems):
        for mount in filesystems:
            source = mount.get("source", "")
            if source.startswith(("/dev/mapper/vgubuntu-root", "/dev/vgubuntu/root")):
                return True
            if uses_ubuntu(mount.get("children", [])):
                return True
        return False

    if uses_ubuntu(mounts["filesystems"]):
        raise GateError("Ubuntu LV still mounted; shared workspace dependencies must be removed first")
    return "# MANUAL ONLY, live USB root shell after independent restore test and lvs review:\nlvremove /dev/vgubuntu/root\n# Never remove vgubuntu or its LUKS container.\n"


def retire_managers(args, runner, execute):
    if not args.stage1_validated:
        raise GateError("Confirm working Nix tools and switched Home Manager with --stage1-validated")
    require_backup(args.backup_report, args.home)
    preflight_stage1(args, runner)
    commands = []
    for source in (Path("/home/linuxbrew"), args.home / ".nvm"):
        destination = source.with_name(source.name + ".retired")
        if not source.exists() and not source.is_symlink():
            continue
        if source.is_symlink() or destination.exists() or destination.is_symlink():
            raise GateError(f"Refusing ambiguous legacy-manager quarantine: {source}")
        commands.append(["mv", "--", str(source), str(destination)])
    if execute:
        for command in commands:
            runner.run(command)
    return {"quarantine_commands": commands}


def main(argv=None, runner=None):
    # Shared options work both before and after the subcommand.
    common = argparse.ArgumentParser(add_help=False, argument_default=argparse.SUPPRESS)
    common.add_argument("--home", type=Path)
    common.add_argument("--flake", type=Path)
    common.add_argument("--receipt", type=Path)
    common.add_argument("--backup-report", type=Path)
    common.add_argument("--corp-stage", type=Path)
    common.add_argument("--system-root", type=Path)
    common.add_argument("--dry-run", action="store_true")
    common.add_argument("--execute", action="store_true")
    common.add_argument("--confirm", action="store_true")
    parser = argparse.ArgumentParser(description=__doc__, parents=[common])
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("preflight-stage1", "stage1-retire-legacy-managers", "preflight-stage2",
                 "print-stage2-cutover-runbook", "retire-ubuntu-lv"):
        command = commands.add_parser(name, parents=[common])
        if name == "stage1-retire-legacy-managers":
            command.add_argument("--stage1-validated", action="store_true")
        if name == "retire-ubuntu-lv":
            for flag in ("vpn-validated", "workflows-validated", "workspace-migrated"):
                command.add_argument(f"--{flag}", action="store_true")
    args = parser.parse_args(argv)
    args.home = getattr(args, "home", Path.home()).absolute()
    args.flake = getattr(args, "flake", args.home / ".dotfiles").absolute()
    args.receipt = getattr(args, "receipt", Path("verified-archives.json"))
    state = args.home / ".local/state/daily-backup"
    args.backup_report = getattr(args, "backup_report", state / "verification.json")
    args.corp_stage = getattr(args, "corp_stage", state / "corporate-access-stack")
    args.system_root = getattr(args, "system_root", Path("/"))
    execute = getattr(args, "execute", False) and not getattr(args, "dry_run", False)
    runner = runner or CommandRunner()
    try:
        if execute and not getattr(args, "confirm", False):
            raise GateError("Host execution requires --execute --confirm")
        if execute and args.command in ("print-stage2-cutover-runbook", "retire-ubuntu-lv"):
            raise GateError("LVM operations are manual-only; use the dry-run runbook in a live USB host shell")
        if args.command == "preflight-stage1":
            result = preflight_stage1(args, runner)
        elif args.command == "preflight-stage2":
            result = preflight_stage2(args, runner)
        elif args.command == "print-stage2-cutover-runbook":
            result = {"runbook": cutover_runbook(args)}
        elif args.command == "retire-ubuntu-lv":
            result = {"runbook": retirement_runbook(args, runner)}
        else:
            result = retire_managers(args, runner, execute)
        print(json.dumps({"status": "EXECUTED" if execute else "DRY_RUN", **result}, indent=2))
        return 0
    except (GateError, ArchiveError, OSError, ValueError, TypeError, KeyError, AttributeError,
            subprocess.CalledProcessError) as error:
        message = "External preflight/host command failed" if isinstance(error, subprocess.CalledProcessError) else str(error)
        print(f"migration-gatekeeper: {message}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
