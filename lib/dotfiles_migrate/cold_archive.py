"""Read-only archive uploads and receipt-gated host-side reclamation."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import stat
import subprocess
import sys
import tempfile

from .config import (
    ACTIVE_WORKSPACE, ARCHIVE_FILE_THRESHOLD, CUTOFF_TS, INACTIVE_PROJECT, ArchivePolicy, excluded,
    rclone_exclusions,
)
from .runner import CommandRunner, Rclone


class ArchiveError(Exception):
    pass


def tree_entries(source, filtered=True, on_permission_error=None):
    """Yield relative entries, warning and skipping unreadable directories."""
    def walk_error(error):
        if not isinstance(error, PermissionError):
            raise error
        print(f"cold-archive: warning: skipping unreadable directory: {error}", file=sys.stderr)
        if on_permission_error is not None:
            on_permission_error(error)

    for root, directories, files in os.walk(source, followlinks=False, onerror=walk_error):
        relative_root = Path(root).relative_to(source)
        directories.sort()
        files.sort()
        kept = []
        for name in directories:
            relative = relative_root / name
            if filtered and excluded(Path(source.name) / relative):
                continue
            yield relative
            if not (source / relative).is_symlink():
                kept.append(name)
        directories[:] = kept
        for name in files:
            relative = relative_root / name
            if not filtered or not excluded(Path(source.name) / relative):
                yield relative


def tree_stats(source):
    count = clean_bytes = latest_mtime = 0
    contains_git = False
    scan_errors = []

    def record_error(error):
        scan_errors.append(str(error))

    for relative in tree_entries(source, on_permission_error=record_error):
        path = source / relative
        info = path.lstat()
        if relative.name == ".git":
            contains_git = True
        if not stat.S_ISDIR(info.st_mode):
            if not (stat.S_ISREG(info.st_mode) or stat.S_ISLNK(info.st_mode)):
                raise ArchiveError(f"Unsupported special file: {path}")
            count += 1
            clean_bytes += info.st_size
            latest_mtime = max(latest_mtime, info.st_mtime_ns)
    raw_bytes = sum(
        (source / relative).lstat().st_size
        for relative in tree_entries(source, filtered=False, on_permission_error=record_error)
        if not (source / relative).is_dir() or (source / relative).is_symlink()
    )
    return {
        "file_count": count, "clean_bytes": clean_bytes, "total_bytes": raw_bytes,
        "latest_mtime": latest_mtime or source.stat().st_mtime_ns,
        "contains_git": contains_git, "scan_errors": sorted(set(scan_errors)),
    }


def git_metadata(source, runner):
    if not (source / ".git").exists():
        return {"git_head": None, "git_remote": None, "git_commit_ts": 0}

    def git(*args):
        return runner.run(["git", "-C", str(source), *args]).stdout.strip()

    try:
        head = git("rev-parse", "HEAD")
    except subprocess.CalledProcessError as error:
        if error.returncode != 128:
            raise
        # An unborn HEAD and a broken repository can both return 128. Keep
        # either active rather than trusting an incomplete activity estimate.
        print(f"cold-archive: warning: unresolved Git HEAD, keeping active: {source}", file=sys.stderr)
        return {
            "git_head": None, "git_remote": None, "git_commit_ts": CUTOFF_TS,
            "scan_errors": [f"Unresolved Git HEAD: {source}"],
        }
    timestamp = int(git("log", "-1", "--format=%ct", "--all"))
    try:
        remote = git("config", "--get", "remote.origin.url")
    except subprocess.CalledProcessError as error:
        if error.returncode != 1:
            raise
        remote = None
    return {"git_head": head, "git_remote": remote, "git_commit_ts": timestamp}


def sha256_file(path):
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def tree_digest(source):
    """Stable SHA-256 of names, types, link targets, and file content hashes."""
    records = []
    for relative in tree_entries(source):
        path = source / relative
        if path.is_symlink():
            records.append([relative.as_posix(), "link", os.readlink(path)])
        elif path.is_file():
            records.append([relative.as_posix(), "file", sha256_file(path)])
        else:
            records.append([relative.as_posix(), "directory"])
    return hashlib.sha256(json.dumps(sorted(records), ensure_ascii=True).encode()).hexdigest()


def read_receipts(path):
    if not path.exists():
        return []
    document = json.loads(path.read_text())
    if document.get("version") != 1 or not isinstance(document.get("items"), list):
        raise ArchiveError(f"Invalid receipt document: {path}")
    items = document["items"]
    if len({item["name"] for item in items}) != len(items):
        raise ArchiveError("Duplicate names in receipt document")
    return items


def write_private(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w") as handle:
            handle.write(text)
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def manifest_text(items):
    lines = ["# Cold Archive", "", "latest_mtime is the latest non-regenerable file mtime in Unix nanoseconds.", ""]
    for item in items:
        lines.extend([
            f"## {item['name']}", "",
            "```json", json.dumps(item, indent=2, sort_keys=True), "```", "",
        ])
    return "\n".join(lines)


class ColdArchive:
    def __init__(self, policy, runner, rclone, receipt):
        self.policy = policy
        self.runner = runner
        self.rclone = rclone
        self.receipt = Path(receipt)

    def scan(self):
        pinned = self.policy.pinned_names()
        sources = []
        if self.policy.code_root.exists():
            sources.extend(
                (path, False) for path in sorted(self.policy.code_root.iterdir())
                if path.is_dir() and not path.is_symlink()
            )
        sources.extend(
            (path, True) for path in self.policy.legacy_roots
            if path.is_dir() and not path.is_symlink()
        )
        items = []
        for source, legacy in sources:
            is_pinned = not legacy and source.name in pinned
            if is_pinned:
                # Null metadata distinguishes an exempt tree from an empty one.
                info = dict.fromkeys((
                    "file_count", "clean_bytes", "total_bytes", "latest_mtime",
                    "contains_git", "git_head", "git_remote", "git_commit_ts",
                ))
                info["scan_errors"] = []
                activity = None
            else:
                info = tree_stats(source)
                if info["scan_errors"]:
                    info.update(git_head=None, git_remote=None, git_commit_ts=0)
                else:
                    info.update(git_metadata(source, self.runner))
                activity = max(info["git_commit_ts"], info["latest_mtime"] / 1_000_000_000)
            inactive = not is_pinned and not info["scan_errors"] and (legacy or activity < CUTOFF_TS)
            # Prefix legacy names to avoid collisions with code repository names.
            name = source.name if not legacy else f"legacy-{source.name}"
            items.append({
                "name": name, "source": str(source.absolute()), **info,
                "activity_ts": activity, "pinned": is_pinned,
                "classification": INACTIVE_PROJECT if inactive else ACTIVE_WORKSPACE,
                "archive": inactive,
                "mode": None if is_pinned else "tar.zst" if (
                    info["contains_git"] or info["file_count"] > self.policy.file_threshold
                    or source == self.policy.home / "mail"
                ) else "rclone",
            })
        if len({item["name"] for item in items}) != len(items):
            raise ArchiveError("Local item name collision; rename the conflicting code directory")
        return items

    def _select(self, names, include_names=()):
        items = self.scan()
        if names:
            by_name = {item["name"]: item for item in items}
            missing = set(names) - by_name.keys()
            if missing:
                raise ArchiveError(f"Unknown local item(s): {', '.join(sorted(missing))}")
            items = [by_name[name] for name in dict.fromkeys(names)]
        else:
            items = [item for item in items if item["archive"] or item["name"] in include_names]
        if any(not item["archive"] for item in items):
            raise ArchiveError("Active or allowlisted items, or unreadable trees, cannot be archived or pruned")
        return items

    def _remote_size(self, item):
        if item["mode"] == "tar.zst":
            metadata = json.loads(self.rclone.run("lsjson", item["remote"], "--stat").stdout)
            if not isinstance(metadata, dict) or metadata.get("IsDir") is not False:
                raise ArchiveError(f"Remote archive is absent: {item['remote']}")
            return metadata["Size"]
        if item["mode"] != "rclone":
            raise ArchiveError(f"Unknown archive mode: {item['mode']}")
        files = json.loads(self.rclone.run(
            "lsjson", item["remote"], "--recursive", "--files-only",
        ).stdout)
        if not isinstance(files, list) or len(files) != item["file_count"]:
            raise ArchiveError(f"Remote file count mismatch: {item['name']}")
        # lsjson on a nonexistent path must fail, even for an empty directory.
        self.rclone.run("lsjson", item["remote"], "--stat")
        return sum(entry["Size"] for entry in files)

    def _require_remote_size(self, item):
        size = self._remote_size(item)
        if size != item["remote_size"]:
            raise ArchiveError(f"Remote size mismatch: {item['name']}")

    def _require_unchanged(self, item):
        source = Path(item["source"])
        current = tree_stats(source)
        if not current["scan_errors"]:
            current.update(git_metadata(source, self.runner))
        if current["scan_errors"]:
            raise ArchiveError(f"Source metadata is incomplete: {source}")
        for field in ("git_head", "latest_mtime", "file_count", "clean_bytes"):
            if current[field] != item[field]:
                raise ArchiveError(f"Source {field} changed since archiving: {source}")

    def _pack(self, source, directory):
        entries = [Path(".")] + list(tree_entries(source))
        paths_file = directory / "entries"
        paths_file.write_bytes(b"\0".join(os.fsencode(str(path)) for path in entries) + b"\0")
        archive = directory / "archive.tar.zst"
        # Explicit non-recursive entries enforce exactly the policy used to scan;
        # --null also protects newline names and names starting with a dash.
        self.runner.run([
            "tar", "-I", "zstd", "-cf", str(archive), "-C", str(source),
            "--no-recursion", "--null", "-T", str(paths_file),
        ])
        return archive, len(entries)

    def _upload_item(self, item, directory):
        source = Path(item["source"])
        result = dict(item)
        suffix = ".tar.zst" if item["mode"] == "tar.zst" else ""
        result["remote"] = f"{self.policy.remote.rstrip('/')}/{item['name']}{suffix}"
        if item["mode"] == "tar.zst":
            archive, entry_count = self._pack(source, directory)
            result.update(sha256=sha256_file(archive), archive_entry_count=entry_count,
                          remote_size=archive.stat().st_size)
            self.rclone.run("copyto", str(archive), result["remote"], "--immutable")
            self._require_remote_size(result)
            downloaded = directory / "verification.tar.zst"
            self.rclone.run("copyto", result["remote"], str(downloaded))
            if sha256_file(downloaded) != result["sha256"]:
                raise ArchiveError(f"Remote SHA-256 mismatch: {item['name']}")
        else:
            result.update(sha256=tree_digest(source), remote_size=item["clean_bytes"])
            args = ["--links", *rclone_exclusions()]
            self.rclone.run("copy", str(source), result["remote"], "--immutable",
                            "--create-empty-src-dirs", *args)
            self.rclone.run("check", str(source), result["remote"], "--one-way", *args)
            self._require_remote_size(result)
            if tree_digest(source) != result["sha256"]:
                raise ArchiveError(f"Source content changed during upload: {source}")
        self._require_unchanged(item)
        result["status"] = "VERIFIED"
        return result

    def _save_receipts(self, items):
        document = json.dumps({"version": 1, "items": items}, indent=2, sort_keys=True) + "\n"
        manifest = self.receipt.parent / "MANIFEST.md"
        write_private(self.receipt, document)
        write_private(manifest, manifest_text(items))
        for path, name in ((self.receipt, "verified-archives.json"), (manifest, "MANIFEST.md")):
            self.rclone.run("copyto", str(path), f"{self.policy.remote.rstrip('/')}/{name}")

    def upload_and_verify(self, names=(), dry_run=False):
        candidates = self._select(names)
        if dry_run:
            return candidates
        receipts = read_receipts(self.receipt)
        existing = {item["name"] for item in receipts}
        if any(item["name"] in existing for item in candidates):
            raise ArchiveError("Receipt already exists; archives are write-once")
        for item in candidates:
            with tempfile.TemporaryDirectory(prefix="cold-archive-", dir="/tmp") as temporary:
                verified = self._upload_item(item, Path(temporary))
            receipts.append(verified)
            self._save_receipts(receipts)
        return receipts

    def restore(self, name, destination):
        matches = [item for item in read_receipts(self.receipt) if item["name"] == name]
        if not matches or matches[0].get("status") != "VERIFIED":
            raise ArchiveError(f"No VERIFIED receipt for {name}")
        item = matches[0]
        destination = Path(destination)
        if destination.is_symlink() or (destination.exists() and any(destination.iterdir())):
            raise ArchiveError("Restore destination must be empty or absent")
        self._require_remote_size(item)
        destination.mkdir(parents=True, exist_ok=True)
        if item["mode"] == "tar.zst":
            with tempfile.TemporaryDirectory(prefix="cold-archive-restore-", dir="/tmp") as temporary:
                archive = Path(temporary) / "archive.tar.zst"
                self.rclone.run("copyto", item["remote"], str(archive))
                if sha256_file(archive) != item["sha256"]:
                    raise ArchiveError(f"Remote SHA-256 mismatch: {name}")
                self.runner.run(["tar", "-I", "zstd", "-xf", str(archive), "-C", str(destination)])
        elif item["mode"] == "rclone":
            self.rclone.run("copy", item["remote"], str(destination), "--links", "--create-empty-src-dirs")
        else:
            raise ArchiveError(f"Unknown archive mode: {item['mode']}")
        return {"name": name, "destination": str(destination)}

    def prune_local(self, names=(), execute=False, confirm=False):
        require_confirmation(execute, confirm)
        receipts = {item["name"]: item for item in read_receipts(self.receipt)}
        # An archived source that has become active must fail the batch,
        # not silently drop out of the set being checked for drift.
        candidates = self._select(names, include_names=receipts)
        if not execute:
            return {"dry_run": True, "would_delete": [item["source"] for item in candidates]}
        # Validate the whole batch before deleting anything, then recheck each
        # local source immediately before removal. Run on a quiescent host.
        verified = []
        for candidate in candidates:
            item = receipts.get(candidate["name"])
            if item is None or item.get("status") != "VERIFIED":
                raise ArchiveError(f"No VERIFIED receipt for {candidate['name']}")
            if item["source"] != candidate["source"]:
                raise ArchiveError(f"Receipt source mismatch: {candidate['name']}")
            require_real_directory(Path(item["source"]))
            self._require_unchanged(item)
            self._require_remote_size(item)
            verified.append(item)
        for item in verified:
            source = Path(item["source"])
            require_real_directory(source)
            self._require_unchanged(item)
            shutil.rmtree(source)
        return {"dry_run": False, "deleted": [item["source"] for item in verified]}

    def prune_caches(self, execute=False, confirm=False):
        require_confirmation(execute, confirm)
        targets = self.policy.cache_targets()
        for target in targets:
            require_real_directory(target)
        before = shutil.disk_usage(self.policy.home).free
        if execute:
            for target in targets:
                require_real_directory(target)
                shutil.rmtree(target)
        return {
            "dry_run": not execute, "targets": [str(path) for path in targets],
            "free_bytes_before": before,
            "free_bytes_after": shutil.disk_usage(self.policy.home).free,
        }


def require_confirmation(execute, confirm):
    if execute != confirm:
        raise ArchiveError("Deletion requires both --execute and --confirm")


def require_real_directory(path):
    if not path.is_dir() or path.is_symlink() or path.absolute() != path.resolve():
        raise ArchiveError(f"Refusing non-directory or symlinked deletion target: {path}")


def build_parser():
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--home", type=Path, default=argparse.SUPPRESS)
    common.add_argument("--work-root", type=Path, default=argparse.SUPPRESS)
    common.add_argument("--allowlist", type=Path, default=argparse.SUPPRESS,
                        help="File of pinned code repository names, one per line")
    common.add_argument("--receipt", type=Path, default=argparse.SUPPRESS,
                        help="Writable receipt path (default: ./verified-archives.json)")
    common.add_argument("--rclone-config", type=Path, default=argparse.SUPPRESS)
    common.add_argument("--file-threshold", type=int, default=argparse.SUPPRESS)
    parser = argparse.ArgumentParser(description=__doc__, parents=[common])
    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("scan", parents=[common])
    upload = subparsers.add_parser("upload-and-verify", parents=[common])
    upload.add_argument("names", nargs="*")
    upload.add_argument("--dry-run", action="store_true")
    restore = subparsers.add_parser("restore", parents=[common])
    restore.add_argument("name")
    restore.add_argument("destination", type=Path)
    for command in ("prune-local", "prune-caches"):
        prune = subparsers.add_parser(command, parents=[common])
        if command == "prune-local":
            prune.add_argument("names", nargs="*")
        mode = prune.add_mutually_exclusive_group()
        mode.add_argument("--dry-run", action="store_true")
        mode.add_argument("--execute", action="store_true")
        prune.add_argument("--confirm", action="store_true")
    return parser


def main(argv=None, runner=None):
    args = build_parser().parse_args(argv)
    runner = runner or CommandRunner()
    policy = ArchivePolicy(
        home=getattr(args, "home", Path.home()).absolute(),
        work_root=getattr(args, "work_root", Path("/mnt/work")).absolute(),
        allowlist=getattr(args, "allowlist", None),
        file_threshold=getattr(args, "file_threshold", ARCHIVE_FILE_THRESHOLD),
    )
    try:
        with Rclone(runner, getattr(args, "rclone_config", None)) as rclone:
            archive = ColdArchive(policy, runner, rclone, getattr(args, "receipt", Path("verified-archives.json")))
            if args.command == "scan":
                result = archive.scan()
            elif args.command == "upload-and-verify":
                result = archive.upload_and_verify(args.names, args.dry_run)
            elif args.command == "restore":
                result = archive.restore(args.name, args.destination)
            elif args.command == "prune-local":
                result = archive.prune_local(args.names, args.execute, args.confirm)
            else:
                result = archive.prune_caches(args.execute, args.confirm)
        print(json.dumps(result, indent=2, sort_keys=True))
        return 0
    except (ArchiveError, OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        print(f"cold-archive: {error}", file=sys.stderr)
        return 1
