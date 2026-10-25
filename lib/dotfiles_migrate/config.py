"""Canonical archive policy. Paths are parameterized for offline tests and previews."""

from dataclasses import dataclass, field
from pathlib import Path


CUTOFF_TS = 1767225600  # 2026-01-01T00:00:00Z
ARCHIVE_FILE_THRESHOLD = 500
BACKUP_REMOTE = "gdrive:bchapman-backup"
COLD_ARCHIVE_REMOTE = f"{BACKUP_REMOTE}/cold-archive"
LEGACY_GINKGO_DIRS = (
    "projects", "ferment", "ghcode", "hts", "joyn", "omics", "results",
    "reviews", "servers",
)
REGENERABLE_NAMES = frozenset({
    ".venv", "venv", ".pixi", "node_modules", "target", "__pycache__",
    ".mypy_cache", ".pytest_cache", ".ruff_cache", ".next", ".tox", ".nox",
    "data-frames",
})
INACTIVE_PROJECT = "Inactive Project"
ACTIVE_WORKSPACE = "Active Workspace"


@dataclass(frozen=True)
class ArchivePolicy:
    home: Path = field(default_factory=Path.home)
    work_root: Path = Path("/mnt/work")
    allowlist: Path = None
    file_threshold: int = ARCHIVE_FILE_THRESHOLD
    remote: str = COLD_ARCHIVE_REMOTE

    @property
    def code_root(self):
        return self.home / "ginkgo/code"

    @property
    def legacy_roots(self):
        return (self.home / "mail", self.work_root) + tuple(
            self.home / "ginkgo" / name for name in LEGACY_GINKGO_DIRS
        )

    def pinned_names(self):
        if self.allowlist is None:
            return set()
        return {
            line.split("#", 1)[0].strip()
            for line in self.allowlist.read_text().splitlines()
            if line.split("#", 1)[0].strip()
        }

    def cache_targets(self):
        plans = self.code_root / "agent-plans"
        targets = [self.home / ".dotfiles/tmp"]
        for name in ("scratch", "repos"):
            targets.extend(sorted(plans.glob(f"*/*/{name}")))
        return [path for path in targets if path.exists() or path.is_symlink()]


def excluded(relative_path):
    """Keep the same filtering policy for counting, packing, and copying."""
    parts = relative_path.parts
    # Git refs can themselves be named "target" or "venv". Once inside
    # repository metadata, preserve everything rather than applying cache rules.
    if ".git" in parts:
        parts = parts[:parts.index(".git")]
    if any(part in REGENERABLE_NAMES for part in parts):
        return True
    if "agent-plans" in parts:
        plan_parts = parts[parts.index("agent-plans") + 1:]
        if "scratch" in plan_parts or "repos" in plan_parts:
            return True
    return ".dotfiles" in parts and "tmp" in parts[parts.index(".dotfiles") + 1:]


def rclone_exclusions():
    args = []
    for name in sorted(REGENERABLE_NAMES):
        args.extend(["--exclude", f"{name}/**"])
    for pattern in ("agent-plans/**/scratch/**", "agent-plans/**/repos/**", ".dotfiles/tmp/**"):
        args.extend(["--exclude", pattern])
    return args
