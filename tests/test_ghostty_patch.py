"""Offline patch/wiring checks; use ghostty_child_probe.py for the real runtime."""

from pathlib import Path
import re
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
PATCH = ROOT / "patches/ghostty-child-environment.patch"
SELECTORS = {
    "GI_TYPELIB_PATH", "GIO_EXTRA_MODULES", "GDK_PIXBUF_MODULE_FILE",
    "GST_PLUGIN_SYSTEM_PATH_1_0",
}
# The upstream child-environment seam, not the parent process environment.
UPSTREAM_BOUNDARY = '''    pub fn defaultTermioEnv(self: *Self) !std.process.EnvMap {
        const app = Application.default();
        const alloc = app.allocator();
        var env = try internal_os.getEnvMap(alloc);
        errdefer env.deinit();

        if (app.savedLanguage()) |language| {
            try env.put("LANG", language);
        } else {
            env.remove("LANG");
        }

        // Don't leak these GTK environment variables to child processes.
        env.remove("GDK_DEBUG");
        env.remove("GDK_DISABLE");
        env.remove("GSK_RENDERER");

        // Remove some environment variables that are set when Ghostty is launched
        // from a `.desktop` file, by D-Bus activation, or systemd.
        env.remove("GIO_LAUNCHED_DESKTOP_FILE");
        env.remove("GIO_LAUNCHED_DESKTOP_FILE_PID");
        env.remove("DBUS_STARTER_ADDRESS");
        return env;
    }
'''


class TestGhosttyPatch(unittest.TestCase):
    def test_patch_removes_only_private_selectors_from_child_map(self):
        with tempfile.TemporaryDirectory(prefix="test-ghostty-patch-") as temporary:
            root = Path(temporary)
            source = root / "src/apprt/gtk/class/surface.zig"
            source.parent.mkdir(parents=True)
            source.write_text(UPSTREAM_BOUNDARY)
            subprocess.run(
                ["patch", "--batch", "--fuzz=0", "-p1", "-i", str(PATCH)],
                cwd=root, check=True, text=True, capture_output=True,
            )
            patched = source.read_text()
        original_removals = re.findall(r'env.remove\("([^"]+)"\);', UPSTREAM_BOUNDARY)
        removals = re.findall(r'env.remove\("([^"]+)"\);', patched)
        self.assertCountEqual(removals, [*original_removals, *SELECTORS])
        self.assertIn('var env = try internal_os.getEnvMap(alloc);', patched)
        self.assertIn('try env.put("LANG", language);', patched)
        self.assertIn('return env;', patched)
        self.assertNotIn("unsetenv", patched)
        self.assertNotIn("setenv", patched)

    def test_overlay_and_desktop_share_patched_pinned_ghostty(self):
        flake = (ROOT / "flake.nix").read_text()
        self.assertIn('nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";', flake)
        self.assertIn("ghostty = unstable.ghostty.overrideAttrs (old: {", flake)
        self.assertIn("patches = (old.patches or []) ++ [ ./patches/ghostty-child-environment.patch ];", flake)
        home = (ROOT / "home/default.nix").read_text()
        packages = re.search(r"home\.packages = with pkgs;\s*\[([^]]+)\];", home)
        self.assertIn("ghostty", packages.group(1).split())
        self.assertNotIn("pkgs.unstable.ghostty", home)
        desktop = (ROOT / "home/ghostty.nix").read_text()
        self.assertIn('ghostty = "${pkgs.ghostty}/bin/ghostty";', desktop)
        self.assertNotIn("pkgs.unstable.ghostty", desktop)
        self.assertNotIn("doCheck = false", flake.split("ghostty = unstable.ghostty.overrideAttrs", 1)[1].split("});", 1)[0])
        for source in ("bashrc", "home/default.nix", "home/ghostty.nix"):
            for name in SELECTORS:
                with self.subTest(source=source, variable=name):
                    self.assertNotIn(name, (ROOT / source).read_text())


if __name__ == "__main__":
    unittest.main()
