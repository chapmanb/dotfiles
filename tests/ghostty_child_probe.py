"""Opt-in Ubuntu/Nix smoke test, run under Xvfb with a built Ghostty binary.

Example: xvfb-run -d python3 tests/ghostty_child_probe.py /nix/store/.../bin/ghostty \
    --report tmp/ghostty-child.json
No desktop configuration is loaded or activated.
"""

import argparse
import json
import os
from pathlib import Path
import subprocess
import sys


SELECTORS = (
    "GI_TYPELIB_PATH", "GIO_EXTRA_MODULES", "GDK_PIXBUF_MODULE_FILE",
    "GST_PLUGIN_SYSTEM_PATH_1_0",
)
PRESERVED = (
    "PATH", "SSL_CERT_FILE", "REQUESTS_CA_BUNDLE", "CURL_CA_BUNDLE",
    "NODE_EXTRA_CA_CERTS", "GIT_SSL_CAINFO", "DISPLAY", "DBUS_SESSION_BUS_ADDRESS",
)
LOAD_ERRORS = ("undefined symbol", "Failed to load module:", "GLIBC_")


def probe_child(report_path):
    expected = json.loads(report_path.with_suffix(".expected.json").read_text())
    report = {
        "selectors_absent": {name: name not in os.environ for name in SELECTORS},
        "preserved": {name: os.environ.get(name) == value for name, value in expected.items()},
        "path_added": [entry for entry in os.environ.get("PATH", "").split(os.pathsep)
                       if entry not in (expected.get("PATH") or "").split(os.pathsep)],
        "path_removed": [entry for entry in (expected.get("PATH") or "").split(os.pathsep)
                         if entry not in os.environ.get("PATH", "").split(os.pathsep)],
        "term": os.environ.get("TERM"),
        "ghostty_resources_present": bool(os.environ.get("GHOSTTY_RESOURCES_DIR")),
        "commands": [],
    }
    commands = (
        ["/usr/bin/gnome-terminal", "--version"],
        ["/usr/bin/gsettings", "get", "org.gnome.desktop.interface", "clock-format"],
        [str(Path.home() / ".nix-profile/bin/libreoffice"), "--version"],
        [str(Path.home() / ".nix-profile/bin/inkscape"), "--version"],
    )
    for command in commands:
        result = subprocess.run(command, capture_output=True, text=True, timeout=20)
        report["commands"].append({
            "command": command,
            "exit_code": result.returncode,
            "stdout": result.stdout,
            "stderr": result.stderr,
        })
    report["passed"] = (
        all(report["selectors_absent"].values())
        and all(report["preserved"].values())
        and report["term"] == "xterm-ghostty"
        and report["ghostty_resources_present"]
        and all(item["exit_code"] == 0 for item in report["commands"])
        and all(not any(error in item["stderr"] for error in LOAD_ERRORS)
                for item in report["commands"])
    )
    report_path.write_text(json.dumps(report, indent=2) + "\n")
    return 0 if report["passed"] else 1


def probe_terminal(binary, report_path):
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.unlink(missing_ok=True)
    expected = {name: os.environ.get(name) for name in PRESERVED}
    # Ghostty normally appends its own binary directory, retaining PATH priority.
    path = expected["PATH"] or ""
    if str(binary.parent) not in path.split(os.pathsep):
        expected["PATH"] = path + os.pathsep + str(binary.parent) if path else str(binary.parent)
    report_path.with_suffix(".expected.json").write_text(json.dumps(expected))
    result = subprocess.run([
        str(binary), "--config-default-files=false",
        "--gtk-single-instance=false", "--linux-cgroup=never",
        "--abnormal-command-exit-runtime=0", "--quit-after-last-window-closed=true",
        "-e", "/usr/bin/python3", str(Path(__file__).resolve()), "--child", str(report_path),
    ], capture_output=True, text=True, timeout=90)
    report_path.with_suffix(".ghostty.log").write_text(result.stdout + result.stderr)
    if not report_path.exists():
        raise RuntimeError("No child report; inspect " + str(report_path.with_suffix(".ghostty.log")))
    report = json.loads(report_path.read_text())
    print(json.dumps(report, indent=2))
    return 0 if report["passed"] and result.returncode == 0 else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", nargs="?", type=Path)
    parser.add_argument("--report", type=Path)
    parser.add_argument("--child", type=Path, help=argparse.SUPPRESS)
    args = parser.parse_args()
    if args.child:
        return probe_child(args.child)
    if not args.binary or not args.report:
        parser.error("binary and --report are required")
    return probe_terminal(args.binary.resolve(), args.report.resolve())


if __name__ == "__main__":
    sys.exit(main())
