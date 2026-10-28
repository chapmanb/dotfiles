"""Native prompt behavior in private interactive Bash sessions, without user config."""

import configparser
import errno
import fcntl
import os
from pathlib import Path
import pty
import re
import select
import signal
import struct
import subprocess
import tempfile
import termios
import time
import unittest


ROOT = Path(__file__).resolve().parents[1]
BASH = "/bin/bash"
READY = b"\x1ePROMPT_READY\x1f"
ANSI = re.compile(r"\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*\x07|[()][A-Z])")
MARK = "#" if os.geteuid() == 0 else "$"


def plain(text):
    return ANSI.sub("", text).replace(READY.decode(), "")


def shell_environment(home, extra=None):
    home = Path(home)
    return {
        "HOME": str(home), "PATH": "/usr/bin:/bin", "TERM": "xterm-256color",
        "USER": "prompt-test", "HISTFILE": str(home / "history"),
        **(extra or {}),
    }


def interactive_shell(commands, *, before="", after="", environment=None, directory=None):
    with tempfile.TemporaryDirectory(prefix="test-bash-prompt-") as temporary:
        home = Path(temporary)
        (home / ".bashrc").symlink_to(ROOT / "bashrc")
        # Reproduce the previous framework load on the unfixed tree, without user config.
        shell = home / ".dotfiles/shell"
        shell.mkdir(parents=True)
        if (ROOT / "shell/liquidprompt").exists():
            (shell / "liquidprompt").symlink_to(ROOT / "shell/liquidprompt")
        (home / ".fzf.bash").touch()
        (home / ".liquidpromptrc").write_text(
            "LP_ENABLE_LOAD=0\nLP_ENABLE_BATT=0\nLP_ENABLE_TEMP=0\nLP_ENABLE_RUNTIME=0\n"
            "LP_ENABLE_GIT=0\nLP_ENABLE_SVN=0\nLP_ENABLE_HG=0\nLP_ENABLE_BZR=0\n"
        )
        rc = home / "rc"
        rc.write_text(
            "PS1='START> '\n" + before + f'\nsource "{ROOT / "bashrc"}"\n' + after + "\n"
            "__test_ready() { local ret=$?; printf '\\036PROMPT_READY\\037'; return \"$ret\"; }\n"
            "if [[ $(declare -p PROMPT_COMMAND) == 'declare -a '* ]]; then\n"
            "  PROMPT_COMMAND+=(__test_ready)\n"
            "else\n"
            "  PROMPT_COMMAND+='; __test_ready'\n"
            "fi\n"
        )
        pid, terminal = pty.fork()
        if pid == 0:
            fcntl.ioctl(1, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 240, 0, 0))
            os.chdir(directory or home)
            os.execve(BASH, [BASH, "--noprofile", "--rcfile", str(rc), "-i"],
                      shell_environment(home, environment))

        def read_prompt():
            output = bytearray()
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline:
                readable, _, _ = select.select([terminal], [], [], 0.1)
                if not readable:
                    continue
                try:
                    chunk = os.read(terminal, 65536)
                except OSError as error:
                    if error.errno == errno.EIO:
                        break
                    raise
                if not chunk:
                    break
                output.extend(chunk)
                if READY in output and plain(output.decode(errors="replace")).endswith(f"{MARK} "):
                    return output.decode(errors="replace")
            raise AssertionError(f"No completed prompt: {output!r}")

        try:
            exchanges = [read_prompt()]
            for command in commands:
                os.write(terminal, command.encode() + b"\n")
                exchanges.append(read_prompt())
            history = (home / "history").read_text() if (home / "history").exists() else ""
            return exchanges, history
        finally:
            # This is exclusively the private test shell, never an operator session.
            os.kill(pid, signal.SIGKILL)
            os.waitpid(pid, 0)
            os.close(terminal)


GHOSTTY_STYLE_HOOK = r'''
__ghostty_preexec_hook() { printf '\e]133;C;\a'; }
__test_ghostty_prompt() {
    local ret=$?
    printf '\e]133;D;%s\a' "$ret"
    if [[ "$PS0" != *'$(__ghostty_preexec_hook >/dev/tty)'* ]]; then
        PS0+='$(__ghostty_preexec_hook >/dev/tty)'
    fi
    return "$ret"
}
PROMPT_COMMAND+='; __test_ghostty_prompt'
'''


class TestBashPrompt(unittest.TestCase):
    def test_local_prompt_colors_status_and_history(self):
        output, history = interactive_shell(["(exit 7)", "true"])
        expected_identity = "root" if os.geteuid() == 0 else ""
        self.assertRegex(plain(output[0]), rf"\[{expected_identity}:~\] {re.escape(MARK)} $")
        self.assertIn("\x1b[32m", output[0])
        self.assertIn("\x1b[1m", output[0])
        self.assertTrue(plain(output[1]).endswith(f"] 7 {MARK} "), output[1])
        self.assertIn("\x1b[35m", output[1])
        self.assertTrue(plain(output[2]).endswith(f"] {MARK} "), output[2])
        self.assertIn("(exit 7)", history)
        self.assertIn("true", history)

    def test_ssh_hostname_cue(self):
        hostname = os.uname().nodename.split(".")[0]
        identity = f"root@{hostname}" if os.geteuid() == 0 else hostname
        for remote in ({"SSH_CONNECTION": "test"}, {"SSH_TTY": "/dev/test"}):
            with self.subTest(remote=remote):
                output, _ = interactive_shell([], environment=remote)
                self.assertIn(f"[{identity}:~] {MARK} ", plain(output[0]))

    def test_directory_content_is_displayed_not_executed(self):
        with tempfile.TemporaryDirectory(prefix="test-prompt-path-") as temporary:
            root = Path(temporary)
            hostile = root / "$(touch PROMPT_INJECTION)`touch PROMPT_BACKTICK`"
            hostile.mkdir()
            output, _ = interactive_shell(["true"], directory=hostile)
            self.assertIn(hostile.name, plain(output[-1]))
            self.assertFalse((hostile / "PROMPT_INJECTION").exists())
            self.assertFalse((hostile / "PROMPT_BACKTICK").exists())

    def test_ghostty_preexec_expands_and_keeps_command_status(self):
        output, _ = interactive_shell(
            ["shopt -q promptvars && printf '<EXPANSION_ON>\\n'", "(exit 7)", "true"],
            after=GHOSTTY_STYLE_HOOK,
        )
        transcript = "".join(output)
        self.assertNotIn("$(__ghostty_preexec_hook >/dev/tty)", transcript)
        self.assertIn("\r\n<EXPANSION_ON>\r\n", plain(output[1]))
        self.assertGreaterEqual(transcript.count("\x1b]133;C;\x07"), 3)
        self.assertIn("\x1b]133;D;7\x07", output[2])
        self.assertTrue(plain(output[2]).endswith(f"] 7 {MARK} "))
        self.assertTrue(plain(output[3]).endswith(f"] {MARK} "))

    def test_existing_hook_status_and_resourcing_are_preserved(self):
        output, _ = interactive_shell(
            ["(exit 7)", "source ~/.bashrc; declare -p PROMPT_COMMAND", "(exit 9)"],
            before=r'''PROMPT_COMMAND='printf "<PRIOR:%s>\n" "$?"' ''',
        )
        self.assertIn("\r\n<PRIOR:7>\r\n", plain(output[1]))
        self.assertEqual(output[2].count("__dotfiles_prompt"), 1)
        self.assertEqual(plain(output[3]).count("<PRIOR:9>"), 1)
        self.assertTrue(plain(output[3]).endswith(f"] 9 {MARK} "))

    def test_array_hook_type_and_entries_are_preserved(self):
        with tempfile.TemporaryDirectory(prefix="test-prompt-array-") as home:
            result = subprocess.run(
                [BASH, "--noprofile", "--norc", "-ic",
                 'PROMPT_COMMAND=(first second); source "$1"; source "$1"; '
                 'declare -p PROMPT_COMMAND; printf "<PRIVILEGE:%s>\\n" "$PS1"',
                 "bash", str(ROOT / "bashrc")],
                env=shell_environment(home), capture_output=True, text=True, check=True,
            )
            self.assertIn('declare -a PROMPT_COMMAND=([0]="__dotfiles_prompt" [1]="first" [2]="second")',
                          result.stdout)
        output, _ = interactive_shell(["printf '<PS1:%s>\\n' \"$PS1\""])
        self.assertIn(r"\$", output[-1])

    def test_noninteractive_shell_does_not_install_prompt(self):
        with tempfile.TemporaryDirectory(prefix="test-prompt-noninteractive-") as home:
            result = subprocess.run(
                [BASH, "--noprofile", "--norc", "-c",
                 'source "$1"; if declare -F __dotfiles_prompt; then exit 1; fi; '
                 'test -z "${PROMPT_COMMAND-}"', "bash", str(ROOT / "bashrc")],
                env=shell_environment(home), capture_output=True, text=True,
            )
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_liquidprompt_submodule_config_and_link_are_retired(self):
        modules = configparser.ConfigParser()
        modules.read(ROOT / ".gitmodules")
        self.assertEqual(set(modules.sections()), {
            'submodule "dotbot"', 'submodule "shell/bashmarks"',
            'submodule "shell/base16-shell"', 'submodule "tmux/plugins/tpm"',
            'submodule "shell/nottoomuch"',
        })
        self.assertFalse((ROOT / "shell/liquidprompt").exists())
        retired_config = subprocess.run(
            ["git", "ls-files", "--error-unmatch", "--", "liquidpromptrc"],
            cwd=ROOT, capture_output=True, text=True,
        )
        self.assertEqual(retired_config.returncode, 1, retired_config.stdout)
        for source in ("bashrc", "home/default.nix"):
            self.assertNotIn("liquidprompt", (ROOT / source).read_text().lower())


if __name__ == "__main__":
    unittest.main()
