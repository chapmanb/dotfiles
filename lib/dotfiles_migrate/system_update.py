"""Backup-gated user, daemon, and Ubuntu updates with preview-first execution."""

import argparse
from datetime import datetime, timedelta, timezone
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import time
import zipfile
import urllib.parse
import urllib.request

from .cold_archive import write_private
from .gatekeeper import (GateError, flake_uri, require_backup,
                         require_corporate_stack, require_nix_profile, protected_package)
from .runner import CommandRunner


UNSTABLE_PACKAGES = (
    "neovim", "uv", "pixi", "bun", "nodejs_24", "python315", "deno", "pnpm",
    "ruff", "pyright", "rclone", "restic", "glab", "brave", "google-chrome",
    "ghostty", "snowflake-cli", "zstd", "mesa",
)
CRITICAL_LIBRARIES = ("openssl", "curl", "libssh2")
HOME_CONFIGURATION = "bchapman@bchapman-ubnt"
EMUDHRA_DISTRUST = "!mozilla/emSign_ECC_Root_CA_-_G3.crt"
UPSTREAM_UNSTABLE = "github:NixOS/nixpkgs/nixos-unstable"
OSV_BATCH_URL = "https://api.osv.dev/v1/querybatch"
# OSV has no NixOS ecosystem. Query upstream tags, or the published registry name.
OSV_REGISTRIES = {
    "uv": ("PyPI", "uv"), "ruff": ("PyPI", "ruff"),
    "snowflake-cli": ("PyPI", "snowflake-cli"),
    "pnpm": ("npm", "pnpm"), "pyright": ("npm", "pyright"),
}
ROOT_UID = 0
ROOT_HELPER = "usr/local/sbin/dotfiles-system-update"
SUDOERS = "etc/sudoers.d/dotfiles-system-update"
SYSTEM_PROFILE = "/nix/var/nix/profiles/default"
ROOT_STATE = "var/lib/system-update"
ROOT_PATH = "/usr/sbin:/usr/bin:/sbin:/bin:/nix/var/nix/profiles/default/bin"
OSV_REPOSITORIES = {
    "nix": "https://github.com/NixOS/nix",
    "neovim": "https://github.com/neovim/neovim",
    "pixi": "https://github.com/prefix-dev/pixi",
    "bun": "https://github.com/oven-sh/bun",
    "nodejs_24": "https://github.com/nodejs/node",
    "python315": "https://github.com/python/cpython",
    "deno": "https://github.com/denoland/deno",
    "rclone": "https://github.com/rclone/rclone",
    "restic": "https://github.com/restic/restic",
    "glab": "https://gitlab.com/gitlab-org/cli",
    "brave": "https://github.com/brave/brave-browser",
    "google-chrome": "https://chromium.googlesource.com/chromium/src",
    "ghostty": "https://github.com/ghostty-org/ghostty",
    "zstd": "https://github.com/facebook/zstd",
    "mesa": "https://gitlab.freedesktop.org/mesa/mesa",
    "openssl": "https://github.com/openssl/openssl",
    "curl": "https://github.com/curl/curl",
    "libssh2": "https://github.com/libssh2/libssh2",
}


def osv_query(package):
    name = package.get("attribute", package["pname"])
    if name in OSV_REGISTRIES:
        ecosystem, upstream_name = OSV_REGISTRIES[name]
    else:
        ecosystem, upstream_name = "GIT", OSV_REPOSITORIES[name]
    return {"package": {"name": upstream_name, "ecosystem": ecosystem},
            "version": package["version"]}


def fetch_advisories(packages):
    """Return OSV batch results, resolving aliases within a shared 10s budget."""
    deadline = time.monotonic() + 10
    queries = [osv_query(package) for package in packages]
    request = urllib.request.Request(
        OSV_BATCH_URL, data=json.dumps({"queries": queries}).encode(),
        headers={"Content-Type": "application/json"}, method="POST",
    )
    with urllib.request.urlopen(request, timeout=10) as response:
        results = json.load(response)["results"]
    # querybatch returns IDs, not CVE aliases. Resolve them before patch filtering.
    details = {}
    for result in results:
        for vulnerability in result.get("vulns", []):
            identifier = vulnerability["id"]
            if identifier not in details:
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise TimeoutError("OSV query exceeded 10 seconds")
                url = "https://api.osv.dev/v1/vulns/" + urllib.parse.quote(identifier, safe="")
                with urllib.request.urlopen(url, timeout=remaining) as response:
                    details[identifier] = json.load(response)
            vulnerability.update(details[identifier])
    return results


def package_metadata(args, runner, names, upstream=False):
    """Inspect effective overlay metadata and comparable upstream derivations."""
    selected = json.dumps(names)
    if upstream:
        setup = f'''
          source = builtins.getFlake {json.dumps(UPSTREAM_UNSTABLE)};
          pkgs = import source {{
            system = "x86_64-linux";
            config = {{ allowUnfree = true; allowInsecurePredicate = _: true; }};
          }};
          raw = pkgs;
        '''
    else:
        setup = f'''
          flake = builtins.getFlake {json.dumps(flake_uri(args))};
          pkgs = flake.homeConfigurations."{HOME_CONFIGURATION}".pkgs;
          raw = import flake.inputs.nixpkgs-unstable {{
            system = "x86_64-linux";
            config = {{ allowUnfree = true; allowInsecurePredicate = _: true; }};
          }};
        '''
    expression = f'''
      let {setup}
      in builtins.listToAttrs (map (name:
        let upstreamPackage = builtins.getAttr name raw;
            p = if builtins.elem name [ "ghostty" "snowflake-cli" ]
                then builtins.getAttr name pkgs else upstreamPackage;
        in {{ inherit name; value = {{
          attribute = name;
          pname = p.pname or name;
          version = p.version or "";
          drvPath = upstreamPackage.drvPath;
          upstreamVersion = upstreamPackage.version or "";
          patches = map toString (p.patches or []);
          knownVulnerabilities = p.meta.knownVulnerabilities or [];
        }}; }}) (builtins.fromJSON {json.dumps(selected)}))
    '''
    return json.loads(runner.run([
        "nix", "eval", "--json", "--impure", "--expr", expression,
    ]).stdout)


def cves(text):
    return set(re.findall(r"CVE-\d{4}-\d{4,}", text, re.I))


def version_components(version):
    return tuple(int(part) for part in re.findall(r"\d+", version))


def closure_advisory_scan(args, runner, advisory_fetcher, warnings):
    locked = package_metadata(args, runner, list(UNSTABLE_PACKAGES + CRITICAL_LIBRARIES))
    names = list(locked)
    advisories = {name: [{"id": item} for item in package["knownVulnerabilities"]]
                  for name, package in locked.items()}
    try:
        results = advisory_fetcher([locked[name] for name in names])
        if len(results) != len(names):
            raise ValueError("OSV result count does not match queried packages")
        for name, result in zip(names, results):
            advisories[name].extend(result.get("vulns", []))
    except (OSError, ValueError, KeyError, TimeoutError) as error:
        warnings.append(f"OSV unavailable ({type(error).__name__}); using Nixpkgs metadata and Brave")

    for name, package in locked.items():
        patched = {identifier.upper() for identifier in cves(" ".join(package["patches"]))}
        remaining = []
        for advisory in advisories[name]:
            identifiers = {identifier.upper() for identifier in cves(json.dumps(advisory))}
            if not identifiers or not identifiers.issubset(patched):
                remaining.append(advisory)
        advisories[name] = remaining
    affected = [name for name in names if advisories[name] or name == "brave"]
    upstream = package_metadata(args, runner, affected, upstream=True)
    actionable, unpatched = [], []
    for name in affected:
        current, candidate = locked[name], upstream[name]
        # Compare raw upstream derivations on both sides: local patches and pins
        # must not themselves look like a new upstream security fix.
        changed = (current["drvPath"] != candidate["drvPath"]
                   or current.get("upstreamVersion", current["version"])
                   != candidate.get("upstreamVersion", candidate["version"]))
        items = advisories[name]
        if (name == "brave" and version_components(candidate["version"])
                > version_components(current["version"])):
            items = items + [{"id": "BRAVE_VERSION_BUMP"}]
        seen = set()
        for advisory in items:
            identifier = advisory["id"]
            if identifier in seen:
                continue
            seen.add(identifier)
            entry = {"package": name, "id": identifier, "version": current["version"],
                     "upstream_version": candidate["version"],
                     "status": "ACTIONABLE" if changed else "UNPATCHED_UPSTREAM"}
            (actionable if changed else unpatched).append(entry)
    return actionable, unpatched


def stable_branch_upgrade(args, runner, warnings):
    lock = json.loads((args.flake / "flake.lock").read_text())
    nodes = lock["nodes"]
    stable_node = nodes[nodes[lock.get("root", "root")]["inputs"]["nixpkgs"]]
    branch = stable_node["original"].get("ref", "")
    current = re.fullmatch(r"nixos-(\d{2})\.(\d{2})", branch)
    newer = []
    try:
        branches = runner.run([
            "git", "ls-remote", "--heads", "https://github.com/NixOS/nixpkgs.git", "nixos-*",
        ]).stdout
        for match in re.finditer(r"refs/heads/(nixos-(\d{2})\.(\d{2}))$", branches, re.M):
            if current and tuple(map(int, match.group(2, 3))) > tuple(map(int, current.groups())):
                newer.append(match.group(1))
    except (OSError, subprocess.CalledProcessError):
        warnings.append("Could not check upstream stable branches")
    last_modified = stable_node["locked"].get("lastModified")
    stale = (last_modified is not None and
             datetime.now(timezone.utc).timestamp() - last_modified > timedelta(days=30).total_seconds())
    if newer or stale:
        return {"current_branch": branch, "newer_branch": max(newer) if newer else None,
                "stale": stale}
    return None


def require_update_gates(args, runner, report):
    require_backup(args.backup_report, args.home)
    report["daily_backup"] = "VERIFIED"
    exceptions = require_corporate_stack(args, runner)
    distrust = args.system_root / "etc/ca-certificates.conf"
    if EMUDHRA_DISTRUST not in [line.strip() for line in distrust.read_text().splitlines()]:
        raise GateError("Corporate Endpoint Stack eMudhra root distrust is missing")
    report["corporate_endpoint_stack"] = "VERIFIED_WITH_EXCEPTIONS" if exceptions else "VERIFIED"
    report["corporate_service_exceptions"] = exceptions


def clean_worktree(args, runner):
    branch = runner.run(["git", "-C", str(args.flake), "rev-parse", "--abbrev-ref", "HEAD"]).stdout.strip()
    status = runner.run(["git", "-C", str(args.flake), "status", "--porcelain"]).stdout
    return branch == "master" and not status.strip()


def update_lock(args, runner, include_unstable):
    inputs = ["nixpkgs", "home-manager"]
    if include_unstable:
        inputs.append("nixpkgs-unstable")
    runner.run(["nix", "flake", "update", "--flake", str(args.flake), *inputs])


def require_candidate(args, runner):
    profile = Path(require_nix_profile(args, runner))
    candidate = (profile / "bin/ghostty").resolve()
    active = (args.home / ".nix-profile/bin/ghostty").resolve()
    if candidate != active:
        runner.run([
            "xvfb-run", "-d", "python3", str(args.flake / "tests/ghostty_child_probe.py"),
            str(candidate), "--report", str(args.state_dir / "ghostty-child.json"),
        ])


def changed_inputs(original, candidate):
    previous, updated = json.loads(original), json.loads(candidate)
    previous_nodes, updated_nodes = previous["nodes"], updated["nodes"]
    previous_inputs = previous_nodes[previous.get("root", "root")]["inputs"]
    updated_inputs = updated_nodes[updated.get("root", "root")]["inputs"]
    return {name for name in ("nixpkgs", "home-manager", "nixpkgs-unstable")
            if previous_nodes[previous_inputs[name]] != updated_nodes[updated_inputs[name]]}


def update_user_flake(args, runner, track):
    lock_path = args.flake / "flake.lock"
    original = lock_path.read_bytes()
    include_unstable = args.include_unstable or bool(track["actionable_advisories"])
    blocked_unstable = False
    try:
        try:
            update_lock(args, runner, include_unstable)
            require_candidate(args, runner)
        except (GateError, OSError, subprocess.CalledProcessError):
            lock_path.write_bytes(original)
            if not include_unstable:
                raise
            blocked_unstable = True
            update_lock(args, runner, False)
            require_candidate(args, runner)
    except (GateError, OSError, subprocess.CalledProcessError) as error:
        lock_path.write_bytes(original)
        track.update(status="BLOCKED_BY_PREFLIGHT", error=type(error).__name__)
        return
    if lock_path.read_bytes() == original:
        track["status"] = "UNSTABLE_BLOCKED_BY_PREFLIGHT" if blocked_unstable else "UP_TO_DATE"
        return
    changed = changed_inputs(original, lock_path.read_bytes())
    try:
        runner.run(["home-manager", "switch", "--flake", f"{flake_uri(args)}#{HOME_CONFIGURATION}"])
    except (OSError, subprocess.CalledProcessError):
        lock_path.write_bytes(original)
        track["status"] = "BLOCKED_BY_ACTIVATION"
        # Home Manager may already have executed activation actions; do not claim rollback.
        raise GateError("Home Manager activation failed; inspect the active generation")
    track.update(status="UNSTABLE_BLOCKED_BY_PREFLIGHT" if blocked_unstable else "UPDATED",
                 stable_updated=bool(changed.intersection(("nixpkgs", "home-manager"))),
                 unstable_updated="nixpkgs-unstable" in changed, commit_status="FAILED")
    # Restrict the additive local commit to the lockfile; never push.
    runner.run(["git", "-C", str(args.flake), "commit", "-m", "Update flake inputs", "flake.lock"])
    track["commit_status"] = "COMPLETED"


def retain_user_store(args, runner, retention):
    for profile in (args.home / ".nix-profile", args.home / ".local/state/nix/profiles/home-manager"):
        runner.run(["nix-env", "-p", str(profile), "--delete-generations", "14d"])
    runner.run(["nix-collect-garbage", "--delete-older-than", "14d"])
    retention["user_gc"] = "COMPLETED"


def require_root_owned(path, boundary=Path("/")):
    """Refuse symlinked or writable installation paths, including their parents."""
    for item in (path, *path.parents):
        info = item.lstat()
        if item.is_symlink() or info.st_uid != ROOT_UID or info.st_mode & 0o022:
            raise GateError(f"Root ownership invariant failed: {item}")
        if item == boundary:
            break


def helper_entrypoint():
    # -I excludes user site packages, PYTHONPATH and the current directory.
    return '''import os
import sys
if len(sys.argv) != 1:
    sys.exit("dotfiles-system-update accepts no arguments")
os.environ.clear()
os.environ.update(PATH="''' + ROOT_PATH + '''", HOME="/root",
                  NIX_CONFIG="experimental-features = nix-command flakes")
from dotfiles_migrate.system_update import main
sys.exit(main(["run-root", "--execute", "--confirm", "--allow-inactive-drivestrike",
               "--home", "/home/bchapman", "--state-dir", "/var/lib/system-update"]))
'''


def install_root_helper(args, runner, report, execute):
    helper, sudoers = args.system_root / ROOT_HELPER, args.system_root / SUDOERS
    report["installation"] = {"helper": str(helper), "helper_mode": "0755",
                              "sudoers": str(sudoers), "sudoers_mode": "0440",
                              "owner": "root:root", "status": "DRY_RUN",
                              "sudoers_rule": "bchapman ALL=(root) NOPASSWD: /usr/local/sbin/dotfiles-system-update",
                              "validation": "sudo visudo -cf <staged-sudoers> before placement"}
    if not execute:
        return
    # Explicitly privileged one-time installation; no passwordless rule references
    # the checkout. The archive contains a snapshot of the reviewed Python code.
    with tempfile.TemporaryDirectory(prefix="system-update-install-") as temporary:
        archive = Path(temporary) / "helper"
        archive.write_bytes(b"#!/usr/bin/python3 -I\n")
        with zipfile.ZipFile(archive, "a") as bundle:
            bundle.writestr("__main__.py", helper_entrypoint())
            for source in Path(__file__).parent.glob("*.py"):
                bundle.write(source, "dotfiles_migrate/" + source.name)
        rule = Path(temporary) / "sudoers"
        rule.write_text("bchapman ALL=(root) NOPASSWD: /usr/local/sbin/dotfiles-system-update\n")
        rule.chmod(0o600)
        runner.run(["sudo", "visudo", "-cf", str(rule)])
        for destination in (helper, sudoers, args.system_root / ROOT_STATE / "report.json"):
            ancestor = destination.parent
            while not ancestor.exists():
                ancestor = ancestor.parent
            require_root_owned(ancestor, args.system_root)
            if destination.exists() or destination.is_symlink():
                require_root_owned(destination, args.system_root)
            runner.run(["sudo", "install", "-d", "-o", "root", "-g", "root", "-m", "0755",
                        str(destination.parent)])
        for source, destination, mode in ((archive, helper, "0755"), (rule, sudoers, "0440")):
            runner.run(["sudo", "install", "-T", "-o", "root", "-g", "root", "-m", mode,
                        str(source), str(destination)])
    report["installation"]["status"] = "INSTALLED"
    report["status"] = "UPDATED"


def as_operator(args, command):
    # Never evaluate a user-controlled flake or execute its programs as root.
    return ["runuser", "-u", "bchapman", "--", "env", "-i", "HOME=" + str(args.home),
            "PATH=" + ROOT_PATH, "NIX_CONFIG=experimental-features = nix-command flakes", *command]


def daemon_version(output):
    match = re.search(r"\b(\d+\.\d+(?:\.\d+)?(?:[a-zA-Z0-9.-]*))", output)
    if not match:
        raise GateError("Could not determine Nix daemon version")
    return match.group(1)


def inspect_daemon(args, runner, advisory_fetcher, report):
    track = report["tracks"]["nix_daemon"]
    running = daemon_version(runner.run(["nix-daemon", "--version"]).stdout)
    name = runner.run(as_operator(args, [
        "nix", "eval", "--raw", f"{flake_uri(args)}#packages.x86_64-linux.daemonProfile.name",
    ])).stdout.strip()
    candidate = daemon_version(name)
    affected = False
    try:
        results = advisory_fetcher([{"attribute": "nix", "pname": "nix", "version": running}])
        affected = bool(results[0].get("vulns", []))
    except (OSError, ValueError, KeyError, IndexError, TimeoutError):
        report["warnings"].append("Nix daemon advisory feed unavailable; enforcing newer-only guard")
    eligible = (version_components(candidate) > version_components(running)
                or (affected and candidate != running))
    track.update(status="DRY_RUN" if eligible else "UP_TO_DATE", running_version=running,
                 candidate_version=candidate, running_advisory_affected=affected)
    return eligible


def trusted_daemon_expression(args):
    """Reconstruct daemonProfile using only official, revision-pinned Nixpkgs.

    Store ownership alone is not trust: an unprivileged Nix build can produce a
    root-owned executable. Never install arbitrary code from the user's flake.
    """
    lock = json.loads((args.flake / "flake.lock").read_text())
    nodes = lock["nodes"]
    inputs = nodes[lock.get("root", "root")]["inputs"]
    sources = {}
    for name in ("nixpkgs", "nixpkgs-unstable"):
        locked = nodes[inputs[name]]["locked"]
        if (locked.get("type") != "github" or locked.get("owner") != "NixOS"
                or locked.get("repo") != "nixpkgs"
                or not re.fullmatch(r"[0-9a-f]{40}", locked.get("rev", ""))):
            raise GateError("Daemon profile requires official revision-pinned NixOS/nixpkgs inputs")
        sources[name] = "github:NixOS/nixpkgs/" + locked["rev"]
    return f'''let
      stable = import (builtins.getFlake {json.dumps(sources["nixpkgs"])}) {{ system = "x86_64-linux"; }};
      unstable = import (builtins.getFlake {json.dumps(sources["nixpkgs-unstable"])}) {{ system = "x86_64-linux"; }};
      nix = unstable.nixVersions.latest;
    in stable.symlinkJoin {{
      name = "nix-daemon-profile-${{nix.version}}";
      paths = [ nix nix.man unstable.cacert ];
    }}'''


def update_daemon(args, runner, track):
    candidate = runner.run(as_operator(args, [
        "nix", "build", "--no-link", "--print-out-paths",
        f"{flake_uri(args)}#daemonProfile",
    ])).stdout.strip()
    trusted = runner.run([
        "nix", "build", "--no-link", "--print-out-paths", "--impure", "--expr",
        trusted_daemon_expression(args),
    ]).stdout.strip()
    if candidate != trusted:
        raise GateError("Flake daemonProfile differs from the trusted Nixpkgs daemon bundle")
    path = Path(trusted)
    if path.parent != args.system_root / "nix/store":
        raise GateError("Daemon candidate is not a Nix store path")
    info = path.stat()
    if info.st_uid != ROOT_UID or info.st_mode & 0o022:
        raise GateError("Daemon candidate must be root-owned and immutable")
    actual = daemon_version(runner.run([str(path / "bin/nix-daemon"), "--version"]).stdout)
    if actual != track["candidate_version"]:
        raise GateError("Built daemon version differs from inspected candidate")
    profile = str(args.system_root / SYSTEM_PROFILE.lstrip("/"))
    runner.run(["nix-env", "-p", profile, "--set", candidate])
    try:
        for suffix in ("service", "socket"):
            unit = args.system_root / f"etc/systemd/system/nix-daemon.{suffix}"
            if not unit.resolve().is_file():
                raise GateError(f"Nix daemon {suffix} unit does not resolve")
        runner.run(["systemctl", "daemon-reload"])
        runner.run(["systemctl", "restart", "nix-daemon.service"])
        if runner.run(["systemctl", "is-active", "nix-daemon"]).stdout.strip() != "active":
            raise GateError("Nix daemon did not become active")
    except (GateError, OSError, subprocess.CalledProcessError):
        track["status"] = "ROLLBACK_FAILED"
        runner.run(["nix-env", "--rollback", "-p", profile])
        runner.run(["systemctl", "daemon-reload"])
        runner.run(["systemctl", "restart", "nix-daemon.service"])
        track["status"] = "ROLLED_BACK"
        raise GateError("Nix daemon restart failed; previous profile restored")
    track["status"] = "UPDATED"


def simulate_host_upgrade(runner):
    output = runner.run(["apt-get", "-s", "upgrade", "--with-new-pkgs"]).stdout
    upgraded = []
    for line in output.splitlines():
        fields = line.split()
        if fields and fields[0] in ("Remv", "Purg"):
            name = fields[1] if len(fields) > 1 else "unknown"
            guard = "Protected Host Package Guard" if protected_package(name) else "Zero-Removal Upgrade Guard"
            raise GateError(f"{guard} refused removal of {name}")
        if len(fields) > 1 and fields[0] == "Inst":
            upgraded.append(fields[1])
    return upgraded


def run_root_tracks(args, runner, advisory_fetcher, report, execute):
    if execute:
        if os.geteuid() != 0:
            raise GateError("run-root requires root")
        require_root_owned(args.system_root / ROOT_HELPER, args.system_root)
        require_root_owned(args.system_root / SUDOERS, args.system_root)
        require_root_owned(args.state_dir, args.system_root)
    daemon, apt = report["tracks"]["nix_daemon"], report["tracks"]["ubuntu_apt"]
    failed = False
    try:
        eligible = inspect_daemon(args, runner, advisory_fetcher, report)
        if execute and eligible:
            update_daemon(args, runner, daemon)
    except (GateError, OSError, ValueError, subprocess.CalledProcessError) as error:
        if daemon["status"] not in ("ROLLED_BACK", "ROLLBACK_FAILED"):
            daemon["status"] = "BLOCKED_BY_GUARD"
        daemon["error"] = str(error)
        failed = True
    try:
        if execute:
            runner.run(["apt-get", "update"])
        apt["upgraded_packages"] = simulate_host_upgrade(runner)
        apt["status"] = "DRY_RUN"
        if execute:
            runner.run(["env", "DEBIAN_FRONTEND=noninteractive", "apt-get", "upgrade", "--with-new-pkgs",
                        "--yes", "-o", "Dpkg::Options::=--force-confdef", "-o",
                        "Dpkg::Options::=--force-confold", "-o", "DPkg::Lock::Timeout=120"])
            report["corporate_endpoint_stack"] = "UNVERIFIED"
            report["corporate_service_exceptions"] = {}
            require_update_gates(args, runner, report)
            apt["status"] = "UPDATED" if apt["upgraded_packages"] else "UP_TO_DATE"
    except (GateError, OSError, subprocess.CalledProcessError) as error:
        apt.update(status="BLOCKED_BY_GUARD", error=str(error))
        failed = True
    apt["reboot_required"] = (args.system_root / "var/run/reboot-required").exists()
    report["tracks"]["store_retention"]["system_gc"] = "DRY_RUN"
    if execute:
        # Preserve the rollback generations after an unsuccessful daemon update.
        if daemon["status"] in ("ROLLED_BACK", "ROLLBACK_FAILED", "BLOCKED_BY_GUARD"):
            report["tracks"]["store_retention"]["system_gc"] = "SKIPPED_DAEMON_FAILURE"
        else:
            profile = str(args.system_root / SYSTEM_PROFILE.lstrip("/"))
            runner.run(["nix-env", "-p", profile, "--delete-generations", "14d"])
            runner.run(["nix-collect-garbage", "--delete-older-than", "14d"])
            report["tracks"]["store_retention"]["system_gc"] = "COMPLETED"
        report["status"] = "PARTIAL" if failed else (
            "UPDATED" if "UPDATED" in (daemon["status"], apt["status"]) else "UP_TO_DATE")
    return 1 if failed else 0


def delegate_root(args, runner, report):
    if not (args.system_root / ROOT_HELPER).is_file():
        return 0
    command = ["sudo", *(["-n"] if args.nightly else []), "/" + ROOT_HELPER]
    try:
        output = runner.run(command).stdout
    except (OSError, subprocess.CalledProcessError) as error:
        output = getattr(error, "output", "")
        if not output:
            for name in ("nix_daemon", "ubuntu_apt"):
                report["tracks"][name]["status"] = "DEFERRED_SUDO_UNAVAILABLE"
            report["tracks"]["store_retention"]["system_gc"] = "DEFERRED_SUDO_UNAVAILABLE"
            return 0
    root_report = json.loads(output)
    for name in ("nix_daemon", "ubuntu_apt"):
        report["tracks"][name] = root_report["tracks"][name]
    report["tracks"]["store_retention"]["system_gc"] = root_report["tracks"]["store_retention"]["system_gc"]
    for key in ("corporate_endpoint_stack", "corporate_service_exceptions"):
        report[key] = root_report[key]
    report["warnings"].extend(root_report["warnings"])
    if root_report["status"] in ("FAILED", "PARTIAL"):
        report["status"] = "PARTIAL"
        return 1
    report["status"] = "UPDATED" if "UPDATED" in (
        report["tracks"]["nix_user"]["status"], root_report["status"]) else "UP_TO_DATE"
    return 0


def parser():
    common = argparse.ArgumentParser(add_help=False, argument_default=argparse.SUPPRESS)
    for flag in ("home", "flake", "state-dir", "backup-report", "system-root"):
        common.add_argument(f"--{flag}", type=Path)
    for flag in ("allow-inactive-drivestrike", "include-unstable", "nightly", "dry-run", "execute", "confirm"):
        common.add_argument(f"--{flag}", action="store_true")
    result = argparse.ArgumentParser(description=__doc__, parents=[common])
    commands = result.add_subparsers(dest="command", required=True)
    for command in ("check", "status", "run", "install-root-helper", "run-root"):
        commands.add_parser(command, parents=[common])
    return result


def main(argv=None, runner=None, advisory_fetcher=None):
    args = parser().parse_args(argv)
    args.home = getattr(args, "home", Path.home()).resolve()
    args.flake = getattr(args, "flake", args.home / ".dotfiles").resolve()
    default_state = (getattr(args, "system_root", Path("/")) / ROOT_STATE
                     if args.command == "run-root" else args.home / ".local/state/system-update")
    args.state_dir = getattr(args, "state_dir", default_state).absolute()
    args.backup_report = getattr(args, "backup_report", args.home / ".local/state/daily-backup/verification.json")
    args.system_root = getattr(args, "system_root", Path("/"))
    for flag in ("allow_inactive_drivestrike", "include_unstable", "nightly", "dry_run", "execute", "confirm"):
        setattr(args, flag, getattr(args, flag, False))
    execute = args.command in ("run", "run-root", "install-root-helper") and args.execute and args.confirm and not args.dry_run
    runner = runner if runner is not None else CommandRunner()
    advisory_fetcher = advisory_fetcher if advisory_fetcher is not None else fetch_advisories
    track = {"status": "DRY_RUN", "stable_updated": False, "unstable_updated": False,
             "actionable_advisories": [], "unpatched_advisories": [],
             "stable_branch_upgrade_available": None}
    retention = {"user_gc": "DRY_RUN", "system_gc": "DEFERRED_ROOT_HELPER_NOT_INSTALLED", "retention": "14d"}
    report = {"version": 1, "timestamp": datetime.now(timezone.utc).isoformat(), "status": "DRY_RUN",
              "daily_backup": "UNVERIFIED", "corporate_endpoint_stack": "UNVERIFIED",
              "corporate_service_exceptions": {}, "warnings": [], "tracks": {
                  "nix_user": track, "store_retention": retention,
                  "nix_daemon": {"status": "DEFERRED_ROOT_HELPER_NOT_INSTALLED"},
                  "ubuntu_apt": {"status": "DEFERRED_ROOT_HELPER_NOT_INSTALLED",
                                 "upgraded_packages": [], "reboot_required": False}}}
    result = 0
    try:
        if execute and args.command == "run-root":
            if os.geteuid() != 0:
                raise GateError("run-root requires root")
            require_root_owned(args.state_dir, args.system_root)
            report_path = args.state_dir / "report.json"
            if report_path.exists() or report_path.is_symlink():
                require_root_owned(report_path, args.system_root)
        if execute:
            # Refuse mutations if their audit report cannot be persisted.
            write_private(args.state_dir / "report.json", json.dumps(report, indent=2) + "\n")
        require_update_gates(args, runner, report)
        if args.command == "install-root-helper":
            install_root_helper(args, runner, report, execute)
        elif args.command == "run-root":
            result = run_root_tracks(args, runner, advisory_fetcher, report, execute)
        else:
            result = run_user_tracks(args, runner, advisory_fetcher, report, execute)
    except (GateError, OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        report["status"] = "FAILED"
        report["error"] = str(error) if isinstance(error, GateError) else type(error).__name__
        print(f"system-update: {report['error']}", file=sys.stderr)
        result = 1
    document = json.dumps(report, indent=2) + "\n"
    try:
        # Root reports must never follow a user-controlled destination, even on
        # the error path after ownership validation failed.
        if args.command == "run-root":
            require_root_owned(args.state_dir, args.system_root)
            destination = args.state_dir / "report.json"
            if destination.exists() or destination.is_symlink():
                require_root_owned(destination, args.system_root)
        write_private(args.state_dir / "report.json", document)
    except (OSError, GateError):
        if execute:
            print("system-update: could not write report", file=sys.stderr)
            result = 1
    print(document, end="")
    return result


def run_user_tracks(args, runner, advisory_fetcher, report, execute):
    track = report["tracks"]["nix_user"]
    retention = report["tracks"]["store_retention"]
    result = 0
    clean = clean_worktree(args, runner)
    if not clean:
        track["status"] = "SKIPPED_DIRTY_WORKTREE"
    if clean or not execute:
        actionable, unpatched = closure_advisory_scan(args, runner, advisory_fetcher, report["warnings"])
        track.update(actionable_advisories=actionable, unpatched_advisories=unpatched,
                     stable_branch_upgrade_available=stable_branch_upgrade(args, runner, report["warnings"]))
    else:
        report["warnings"].append("Advisory evaluation skipped for dirty or non-master flake")
    if execute:
        if clean:
            update_user_flake(args, runner, track)
        if track["status"] == "BLOCKED_BY_PREFLIGHT":
            report["status"] = "PARTIAL"
            result = 1
        else:
            retain_user_store(args, runner, retention)
            report["status"] = "PARTIAL"
            result = delegate_root(args, runner, report)
    return result
