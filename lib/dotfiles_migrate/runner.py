"""Injectable subprocess boundary and sandbox-safe rclone credentials."""

import errno
import os
from pathlib import Path
import shutil
import subprocess
import tempfile


class CommandRunner:
    def run(self, args):
        env = os.environ.copy()
        path_parts = [part for part in env.get("PATH", "").split(os.pathsep) if part]
        for nix_bin in ("/nix/var/nix/profiles/default/bin", str(Path.home() / ".nix-profile/bin")):
            if nix_bin not in path_parts:
                path_parts.append(nix_bin)
        env["PATH"] = os.pathsep.join(path_parts)
        env.setdefault("NIX_CONFIG", "experimental-features = nix-command flakes")
        return subprocess.run(
            [str(arg) for arg in args], check=True, capture_output=True, text=True, env=env,
        )


class Rclone:
    """Keep refreshed tokens in a private writable directory on read-only mounts.

    The temporary credentials live only as long as this context. Refreshed tokens
    cannot be written back to a read-only source and are deliberately not logged.
    """

    def __init__(self, runner, config_path=None):
        self.runner = runner
        self.source_config = Path(config_path) if config_path else Path(
            os.environ.get("RCLONE_CONFIG", Path.home() / ".config/rclone/rclone.conf")
        )
        self.config_path = self.source_config
        self._temporary = None
        self._prepared = False

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        if self._temporary is not None:
            self._temporary.cleanup()

    def _prepare_config(self):
        if self._prepared:
            return
        try:
            # rclone atomically replaces its config; a writable file alone is
            # insufficient when its containing directory is a read-only mount.
            with tempfile.TemporaryFile(dir=self.source_config.parent):
                pass
        except OSError as error:
            if error.errno not in (errno.EROFS, errno.EACCES):
                raise
            self._temporary = tempfile.TemporaryDirectory(prefix="cold-archive-rclone-", dir="/tmp")
            self.config_path = Path(self._temporary.name) / "rclone.conf"
            shutil.copyfile(self.source_config, self.config_path)
            self.config_path.chmod(0o600)
        self._prepared = True

    def run(self, *args):
        self._prepare_config()
        return self.runner.run(["rclone", "--config", str(self.config_path), *args])
