"""Offline structural checks; Nix builds remain a separate host preflight gate."""

import hashlib
import os
from pathlib import Path
import re
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
UNSTABLE_PACKAGES = {
    "neovim", "wezterm", "uv", "pixi", "bun", "nodejs_24", "python315", "deno", "pnpm",
    "ruff", "pyright", "rclone", "restic", "glab", "brave", "google-chrome",
}
HOME_PACKAGES = {
    "ripgrep", "fd", "bat", "fzf", "jq", "rclone", "restic", "pkgs.unstable.zstd", "git", "glab",
    "neovim", "wezterm", "pkgs.unstable.ghostty", "tmux", "awscli2", "snowflake-cli", "babashka", "clojure", "leiningen",
    "ruff", "pyright", "tree-sitter", "xclip", "wl-clipboard", "pass", "gnupg",
    "nodejs_24", "python315", "uv", "pixi", "bun", "deno", "pnpm",
}
# SHA-256 of the active user configs at import time, independent of production Nix.
IMPORTED_CONFIGS = {
    "config/nvim/init.lua": "11445b38ebc3983654b1b40aa5541fb1bef92114fbdc5750b247ab17e552c15c",
    "config/nvim/nvim-pack-lock.json": "4de08dbba16e09a37c0e1b2503d33718422cfc0c3938efaad0a68c1ae1e38b70",
    "config/wezterm/wezterm.lua": "1c41636673e813e427a627fe446c13b01fab8a0d24cfe2786bcfac5416937594",
    "config/herdr/config.toml": "8633509a5812dc6330a2ea2ca21064e78a98f9d26437bfd0c768cfa0bb351bdc",
}
RETIRED_FILES = (
    "install", "install.conf.yaml", "Xresources", "alot/config", "alot/hooks.py",
    "bootstrap/backup.sh", "bootstrap/packages.sh", "bootstrap/sync_from_drive.sh",
    "bootstrap/sync_to_drive.sh", "bootstrap/transfer.sh", "bootstrap/vim.clj",
    "emacs_d_brad/extensions.el", "emacs_d_brad/packages.el", "emacs_d_brad/secrets.el.gpg",
    "emacs_d_org-roam/packages.el", "flake8", "notmuch-config", "notmuch/notmuch-clean.sh",
    "notmuch/notmuch-poll.py", "offlineimap.py", "offlineimaprc", "pep8", "pylintrc",
    "spacemacs", "xmonad.hs", "yapf.cfg",
)


class TestFlakeAndHome(unittest.TestCase):
    def test_flake_inputs_overlay_and_outputs(self):
        flake = (ROOT / "flake.nix").read_text()
        for declaration in (
            'nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";',
            'nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";',
            'url = "github:nix-community/home-manager/release-24.11";',
            'inputs.nixpkgs.follows = "nixpkgs";',
            'system = "x86_64-linux";',
            'unstable = import nixpkgs-unstable',
            'inherit unstable;',
            'pkgs = import nixpkgs',
            'overlays = [ unstableOverlay ];',
            'overlays.default = unstableOverlay;',
        ):
            with self.subTest(declaration=declaration):
                self.assertIn(declaration, flake)
        overlay = re.search(r"inherit \(unstable\)([^;]+);", flake)
        self.assertIsNotNone(overlay)
        self.assertEqual(set(overlay.group(1).split()), UNSTABLE_PACKAGES)
        self.assertRegex(
            flake,
            r'homeConfigurations\."bchapman@bchapman-ubnt" = home-manager\.lib\.homeManagerConfiguration\s*\{\s*inherit pkgs;\s*modules = \[\s*./home\s*\{\s*targets.genericLinux.enable = true;[^}]*\}\s*\];',
        )
        self.assertRegex(
            flake,
            r'nixosConfigurations\.bchapman-ubnt = nixpkgs\.lib\.nixosSystem\s*\{\s*inherit system;\s*modules = \[\s*./hosts/bchapman-ubnt',
        )
        self.assertIn("home-manager.nixosModules.home-manager", flake)
        self.assertIn("nixpkgs.overlays = [ unstableOverlay ];", flake)
        self.assertIn("home-manager.useGlobalPkgs = true;", flake)
        self.assertIn("home-manager.useUserPackages = true;", flake)
        self.assertIn("home-manager.users.bchapman.imports = [ ./home ./home/ghostty.nix ];", flake)
        self.assertEqual(flake.count("config.allowUnfree = true;"), 3)

    def test_ghostty_nixos_desktop_integration(self):
        home = (ROOT / "home/default.nix").read_text()
        flake = (ROOT / "flake.nix").read_text()
        desktop = (ROOT / "home/ghostty.nix").read_text()
        standalone, nixos = flake.split("nixosConfigurations.bchapman-ubnt =", 1)
        self.assertIn("pkgs.unstable.ghostty", home)
        self.assertNotIn("./home/ghostty.nix", standalone)
        self.assertIn("home-manager.users.bchapman.imports = [ ./home ./home/ghostty.nix ];", nixos)
        self.assertIn("home.packages = [ pkgs.gnome-terminal pkgs.xdg-terminal-exec ];", desktop)
        self.assertNotIn("excludePackages", (ROOT / "hosts/bchapman-ubnt/default.nix").read_text())
        self.assertIn('ghostty = "${pkgs.unstable.ghostty}/bin/ghostty";', desktop)
        self.assertIn("home.sessionVariables.TERMINAL = ghostty;", desktop)
        self.assertIn('"org/gnome/desktop/default-applications/terminal"', desktop)
        self.assertIn("exec = ghostty;", desktop)
        self.assertIn('exec-arg = "-e";', desktop)
        self.assertIn('"xdg-terminals.list".text = "com.mitchellh.ghostty.desktop\\n";', desktop)
        self.assertIn("custom-keybindings = lib.mkAfter [", desktop)
        self.assertIn('"/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/ghostty/"', desktop)
        self.assertIn('"org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/ghostty"', desktop)
        self.assertIn('binding = "<Primary><Alt>t";', desktop)
        self.assertIn("command = ghostty;", desktop)
        legacy, login = desktop.split('"autostart/com.mitchellh.ghostty.desktop".text =', 1)
        legacy = legacy.split('"autostart/gnome-terminal.desktop" =', 1)[1]
        self.assertIn("force = true;", legacy)
        self.assertIn("Hidden=true", legacy)
        for line in ("Type=Application", "Exec=${ghostty}", "TryExec=${ghostty}",
                     "Terminal=false", "OnlyShowIn=GNOME;", "X-GNOME-Autostart-enabled=true"):
            self.assertIn(line, login)
        self.assertNotIn("Hidden=true", login)
        self.assertNotIn("/usr/bin/gnome-terminal", desktop)

    def test_ubuntu_uses_native_mime_database(self):
        flake = (ROOT / "flake.nix").read_text()
        standalone = flake.split('homeConfigurations."bchapman@bchapman-ubnt" =', 1)[1]
        standalone, nixos = standalone.split("nixosConfigurations.bchapman-ubnt =", 1)
        self.assertIn("xdg.mime.enable = false;", standalone)
        self.assertNotIn("xdg.mime.enable", nixos)
        self.assertNotIn("xdg.mime.enable", (ROOT / "home/default.nix").read_text())

    def test_home_manager_symlinks_packages_ca_vars_and_backup_timer(self):
        home = (ROOT / "home/default.nix").read_text()
        self.assertIn('dotfiles = "${config.home.homeDirectory}/.dotfiles";', home)
        links = {
            ".bashrc": "bashrc",
            ".gitconfig": "gitconfig",
            ".inputrc": "inputrc",
            ".tmux.conf": "tmux.conf",
            ".vimrc": "vimrc",
            ".config/pip/pip.conf": "pip.conf",
            ".config/msmtp/config": "msmtprc",
            ".config/nvim/init.lua": "config/nvim/init.lua",
            ".config/nvim/nvim-pack-lock.json": "config/nvim/nvim-pack-lock.json",
            ".wezterm.lua": "config/wezterm/wezterm.lua",
            ".config/herdr/config.toml": "config/herdr/config.toml",
        }
        for destination, source in links.items():
            with self.subTest(destination=destination):
                self.assertTrue((ROOT / source).is_file())
                self.assertIn(
                    f'"{destination}".source = config.lib.file.mkOutOfStoreSymlink "${{dotfiles}}/{source}";',
                    home,
                )
        for source, digest in IMPORTED_CONFIGS.items():
            with self.subTest(imported=source):
                self.assertEqual(hashlib.sha256((ROOT / source).read_bytes()).hexdigest(), digest)
        packages = re.search(r"home\.packages = with pkgs;\s*\[([^]]+)\];", home)
        self.assertIsNotNone(packages)
        self.assertEqual(set(packages.group(1).split()), HOME_PACKAGES)
        variables = re.search(r"home\.sessionVariables = \{([^}]+)\};", home)
        self.assertIsNotNone(variables)
        for name in ("SSL_CERT_FILE", "REQUESTS_CA_BUNDLE", "CURL_CA_BUNDLE",
                     "NODE_EXTRA_CA_CERTS", "GIT_SSL_CAINFO"):
            self.assertIn(f'{name} = "/etc/ssl/certs/ca-certificates.crt";', variables.group(1))
        self.assertIn('home.username = "bchapman";', home)
        self.assertIn('home.homeDirectory = "/home/bchapman";', home)
        self.assertIn('home.stateVersion = "24.11";', home)
        self.assertIn('programs.home-manager.enable = true;', home)
        service, timer = home.split("systemd.user.timers.dotfiles-daily-backup =", 1)
        self.assertIn("systemd.user.services.dotfiles-daily-backup =", service)
        self.assertIn('Type = "oneshot";', service)
        self.assertIn('ExecStart = "/home/bchapman/.dotfiles/bin/daily-backup run";', service)
        self.assertIn('ExecStartPost = "/home/bchapman/.dotfiles/bin/nightly-maintenance";', service)
        self.assertIn('Environment = lib.mapAttrsToList (name: value: "${name}=${value}")', service)
        self.assertIn("config.home.sessionVariables //", service)
        self.assertIn('PATH = "${lib.makeBinPath config.home.packages}:/usr/bin:/bin";', service)
        self.assertIn('UMask = "0077";', service)
        self.assertIn('OnCalendar = "*-*-* 22:00:00 America/New_York";', timer)
        self.assertIn('Persistent = true;', timer)
        self.assertIn('Unit = "dotfiles-daily-backup.service";', timer)
        self.assertIn('Install.WantedBy = [ "timers.target" ];', timer)

    def test_overlay_omits_zstd_and_bashrc_fallback(self):
        flake = (ROOT / "flake.nix").read_text()
        overlay = re.search(r"inherit \(unstable\)([^;]+);", flake)
        self.assertIsNotNone(overlay)
        self.assertNotIn("zstd", overlay.group(1).split())
        home_module = (ROOT / "home/default.nix").read_text()
        packages = re.search(r"home\.packages = with pkgs;\s*\[([^]]+)\];", home_module)
        self.assertIsNotNone(packages)
        self.assertIn("pkgs.unstable.zstd", packages.group(1).split())
        self.assertNotIn("zstd", packages.group(1).split())

        with tempfile.TemporaryDirectory(prefix="test-shell-transition-") as temporary:
            home = Path(temporary)
            nvm = home / ".nvm/nvm.sh"
            nvm.parent.mkdir()
            nvm.write_text('export PATH="$NVM_DIR/versions/node/test/bin:$PATH"\n')
            session = home / ".nix-profile/etc/profile.d/hm-session-vars.sh"
            environment = {
                "HOME": str(home), "USER": "dotfiles-transition-test", "PATH": "/usr/bin:/bin",
            }
            legacy = {
                "/home/linuxbrew/.linuxbrew/bin", "/home/linuxbrew/.linuxbrew/sbin",
                str(home / ".local/share/pnpm"), str(home / ".deno/bin"),
                str(home / ".pixi/bin"), str(home / ".nvm/versions/node/test/bin"),
            }
            agents = {str(home / ".local/bin"), str(home / ".bun/bin")}
            for activated in (False, True):
                with self.subTest(home_manager=activated):
                    if activated:
                        session.parent.mkdir(parents=True)
                        session.write_text('export PATH="$HOME/.nix-profile/bin:$PATH"\n')
                    result = subprocess.run(
                        ["bash", "--noprofile", "--norc", "-c",
                         '. "$1"; printf "%s\\n" "$PATH"', "bash", str(ROOT / "bashrc")],
                        env=environment, check=True, text=True, capture_output=True,
                    )
                    paths = result.stdout.strip().split(os.pathsep)
                    self.assertTrue(agents.issubset(paths), paths)
                    self.assertIn("/usr/bin", paths)
                    if activated:
                        self.assertIn(str(home / ".nix-profile/bin"), paths)
                        self.assertTrue(legacy.isdisjoint(paths), paths)
                    else:
                        self.assertTrue(legacy.issubset(paths), paths)
                        self.assertLess(paths.index("/home/linuxbrew/.linuxbrew/bin"),
                                        paths.index("/usr/bin"))

    def test_shell_git_updates_and_legacy_file_retirement(self):
        shell = (ROOT / "bashrc").read_text()
        subprocess.run(["bash", "-n", str(ROOT / "bashrc")], check=True)
        self.assertIn('. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"', shell)
        self.assertIn('. "/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh"', shell)
        self.assertIn('export PATH="$PATH:$HOME/.local/bin:$HOME/.bun/bin"', shell)
        # Non-interactive shells also need the runtime and trust environment.
        self.assertLess(shell.index("hm-session-vars.sh"), shell.index("[[ $- == *i* ]] || return"))
        self.assertLess(shell.index("$HOME/.local/bin"), shell.index("[[ $- == *i* ]] || return"))
        self.assertNotIn("GIT_SSL_NO_VERIFY=true", shell)
        git = (ROOT / "gitconfig").read_text()
        self.assertIn("helper = !glab auth git-credential", git)
        self.assertNotIn("!/usr/bin/glab auth git-credential", git)
        for name in RETIRED_FILES:
            with self.subTest(retired=name):
                path = ROOT / name
                self.assertFalse(path.exists() or path.is_symlink(), name)
        # The checkout metadata/submodule is intentionally left for a later cleanup.
        self.assertTrue((ROOT / ".gitmodules").is_file())
        self.assertTrue((ROOT / "dotbot").is_dir())

    def test_nightly_maintenance_continues_to_shutdown_when_repo_sync_fails(self):
        script = ROOT / "bin/nightly-maintenance"
        subprocess.run(["bash", "-n", str(script)], check=True)
        with tempfile.TemporaryDirectory(prefix="test-nightly-maintenance-") as temporary:
            root = Path(temporary)
            home = root / "home"
            (home / "ginkgo/code/acs-mirror").mkdir(parents=True)
            (home / "ginkgo/code/acs-scheduler").mkdir(parents=True)
            bindir = root / "bin"
            bindir.mkdir()
            log = root / "calls.log"

            for name, body in (
                ("uv", 'echo "uv $*" >> "$CALL_LOG"\n'
                       'if [ "${FAIL_UV:-0}" = "1" ]; then exit 1; fi\n'),
                ("git", 'echo "git $*" >> "$CALL_LOG"\n'
                        'if [ "${FAIL_GIT:-0}" = "1" ]; then exit 1; fi\n'),
                ("date", 'echo "${FAKE_HOUR:-22}"\n'),
                ("systemctl", 'echo "systemctl $*" >> "$CALL_LOG"\n'),
                ("dbus-send", 'echo "dbus-send $*" >> "$CALL_LOG"\n'),
            ):
                stub = bindir / name
                stub.write_text("#!/usr/bin/env bash\n" + body)
                stub.chmod(0o755)

            base_env = {
                "HOME": str(home),
                "PATH": f"{bindir}:/usr/bin:/bin",
                "CALL_LOG": str(log),
            }

            # Off-VPN failure in acs-mirror check skips update/sync, still attempts git pull and powers off at 22:00.
            result = subprocess.run(
                [str(script)],
                env={**base_env, "FAIL_UV": "1", "FAIL_GIT": "1", "FAKE_HOUR": "22"},
                check=True, text=True, capture_output=True,
            )
            self.assertIn("acs-mirror sync failed", result.stderr)
            self.assertIn("acs-scheduler pull failed", result.stderr)
            self.assertEqual(
                log.read_text().splitlines(),
                [
                    "uv run acs-mirror check",
                    "git pull origin master",
                    "systemctl poweroff -i",
                ],
            )

            # Daytime catch-up run (10:00) performs all sync steps and skips poweroff.
            log.unlink()
            subprocess.run(
                [str(script)],
                env={**base_env, "FAKE_HOUR": "10"},
                check=True, text=True, capture_output=True,
            )
            self.assertEqual(
                log.read_text().splitlines(),
                [
                    "uv run acs-mirror check",
                    "uv run acs-mirror update",
                    "uv run acs-mirror sync",
                    "git pull origin master",
                ],
            )


if __name__ == "__main__":
    unittest.main()
