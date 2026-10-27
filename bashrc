# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi
export NIX_CONFIG="${NIX_CONFIG:-experimental-features = nix-command flakes}"

# Load declarative tools and unified CA trust before local overrides.
if [ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
elif [ -f "/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh" ]; then
    . "/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh"
fi

# Keep agent launchers available without shadowing Nix-provided runtimes.
export PATH="$PATH:$HOME/.local/bin:$HOME/.bun/bin"

# Load settings from .bashrc.local if it exists
# This contains passwords and local settings
if [[ -f $HOME/.bashrc.local ]]; then
    source $HOME/.bashrc.local
fi
if [[ -f $HOME/.bashrc.ginkgo ]]; then
    source $HOME/.bashrc.ginkgo
fi

# -- Standard PATHs
export PATH=$PATH:/sbin/:/usr/sbin:/bin:/usr/bin:/usr/local/bin:/usr/X11R6/bin

# -- Standard configuration

umask 002
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'
export EDITOR=nvim
export PAGER=less

export DEBIAN_FRONTEND=noninteractive

export HISTCONTROL=ignoreboth
export HISTCONTROL=erasedups
shopt -s histappend
export HISTSIZE=10000000
export HISTFILESIZE=10000000
export PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
__reload_history ()
[[ -f ~/.fzf.bash ]] && source ~/.fzf.bash

# Disable Ctrl-Z job-control suspend (accidental presses were stopping TUI sessions)
[[ $- == *i* ]] && stty susp undef

# Reload git completion after fzf, which otherwise clobbers `git` with fuzzy path completion
[ -f /usr/share/bash-completion/completions/git ] && . /usr/share/bash-completion/completions/git

shopt -s checkwinsize

# bcbio
unset JAVA_HOME
if [ -d "$BCBIO" ]; then
    if [ "$BCBIO" != "/usr/local" ]; then
        export PATH=$BCBIO/bin:$PATH
    fi
    #export LD_LIBRARY_PATH=$BCBIO/lib:$LD_LIBRARY_PATH
    #export PERL5LIB=$BCBIO/lib/perl5:${PERL5LIB}
    alias bcbio_anaconda="$BCBIO/share/bcbio_nextgen/anaconda/bin/anaconda"
fi

if [ ! -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ] &&
   [ -f "/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh" ]; then
    export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
else
    export PATH="$HOME/.nix-profile/bin:$PATH"
fi

# Custom aliases
alias skype='LD_PRELOAD=/usr/lib/i386-linux-gnu/mesa/libGL.so.1 skype'
alias ctrlcaps='setxkbmap -option ctrl:nocaps'
alias oauth="~/.oauth/bchapman-openauth.sh"
alias vpnrc="sudo openconnect vpn.rc.fas.harvard.edu --script=~/.vpn/vpnc-script --user=bchapman@odyssey"
alias vpnwwcrc='sudo vpnc glasgow'
alias pyi='python setup.py build && sudo python setup.py install'
finder () {
	find . -type f -print0 | xargs -0 grep $1
}
killer () {
	ps ax | grep "$1" | awk '{print $1}' | xargs kill
}
alias sacct_std='sacct -s r -s pd -X --format JobID,JobName,Partition,NNodes,AllocCPUS,State,NodeList'

# --- Interactive only commands
# If not running interactively, stop here
[[ $- == *i* ]] || return


if [ -f /usr/local/bin/aws_completer ]; then
	complete -C '/usr/local/bin/aws_completer' aws
fi

# Disable ctrl-s ctrl-q (suspend, resume. frequently really really annoying!)
# http://geekanova.blogspot.co.uk/2012/11/ctrl-s-freezes-terminal.html
[[ $- == *i* ]] && stty -ixon -ixoff

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ -f ~/.dotfiles/shell/liquidprompt/liquidprompt ]; then
    source ~/.dotfiles/shell/liquidprompt/liquidprompt
fi

#if [ -f ~/.dotfiles/shell/bashmarks/bashmarks.sh ]; then
#    source ~/.dotfiles/shell/bashmarks/bashmarks.sh
#fi

if [ -f ~/.dotfiles/mintty-colors-solarized/mintty-solarized-dark.sh ]; then
	source ~/.dotfiles/mintty-colors-solarized/mintty-solarized-dark.sh
fi

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

## highlighting
BASE16_SHELL="$HOME/.dotfiles/shell/base16-shell/base16-ocean.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Last pass
export LPASS_CLIPBOARD_COMMAND="xclip -selection clipboard -in -l 1"
export LPASS_AGENT_TIMEOUT=0

# auto activate virtualenv
# Modified solution based on https://stackoverflow.com/questions/45216663/how-to-automatically-activate-virtualenvs-when-cding-into-a-directory/56309561#56309561
function cd() {
  builtin cd "$@"

  ## Default path to virtualenv in your projects
  DEFAULT_ENV_PATH="./.venv"

  ## If env folder is found then activate the vitualenv
  function activate_venv() {
    if [[ -f "${DEFAULT_ENV_PATH}/bin/activate" ]] ; then
      source "${DEFAULT_ENV_PATH}/bin/activate"
      echo "Activating ${VIRTUAL_ENV}"
    fi
  }

  if [[ -z "$VIRTUAL_ENV" ]] ; then
    activate_venv
  else
    ## check the current folder belong to earlier VIRTUAL_ENV folder
    # if yes then do nothing
    # else deactivate then run a new env folder check
      parentdir="$(dirname ${VIRTUAL_ENV})"
      if [[ "$PWD"/ != "$parentdir"/* ]] ; then
        echo "Deactivating ${VIRTUAL_ENV}"
        deactivate
        activate_venv
      fi
  fi
}

# WSL (Windows Subsystem for Linux) specific settings.
if grep -qE "(Microsoft|WSL)" /proc/version &>/dev/null; then
    # Adjustments for WSL's file / folder permission metadata.
    if [ "$(umask)" = "0000" ]; then
      umask 0022
    fi

    # Access local X-server with VcXsrv.
    #   Requires: https://sourceforge.net/projects/vcxsrv/ (or alternative)
    export DISPLAY=:0

    # Windows Docker
    # https://nickjanetakis.com/blog/setting-up-docker-for-windows-and-wsl-to-work-flawlessly
    export DOCKER_HOST=tcp://localhost:2375

    # ssh-agent on windows
    eval $(~/windows_home/local/share/ssh-agent-wsl/ssh-agent-wsl -r)
fi
