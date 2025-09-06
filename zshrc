# ;;-*- mode: sh; -*-

export PS1="%1~ %# "

export LANG="en_US.UTF-8"
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

alias tchk="terraform fmt -check -list -recursive"
alias tfmt="terraform fmt -recursive"
alias cc="cc -Wall -Werror -Wextra -pedantic -std=c99"
alias c++="c++ -Wall -Werror -Wextra -pedantic -std=c++20"
alias ytmp3="youtube-dl -x --audio-format mp3 --no-check-certificate"

if [ "$(uname)" = "Darwin" ]; then
  alias nproc="sysctl -n hw.logicalcpu"
fi

alias m="make -j$(nproc)"
alias n="ninja"
alias k="kubectl"
alias tf="terraform"
alias mg="mg -n"
alias sbcl="rlwrap sbcl"

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
  colorflag="--color"
else # OS X `ls`
  colorflag="-G"
fi

# export LSCOLORS=ExFxCxDxBxegedabagacad # light colours
export LSCOLORS=gxfxcxdxbxegedabagacad # dark colours

alias l="ls -lhF ${colorflag}"
alias la="ls -lahF ${colorflag}"
alias lsd="ls -lhF ${colorflag} | grep --color=never '^d'"
alias ls="command ls ${colorflag}"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

alias untar='tar xvf'

# Homebrew installs packages under /opt/homebrew/ when running in Apple
# Silicon.
if [[ $(uname -m) == 'arm64' ]]; then
  export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
fi

export PATH="/usr/local/opt/binutils/bin:/usr/local/lib/python3.9/site-packages:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$PATH:$HOME/.docker/bin"

export HOMEBREW_NO_AUTO_UPDATE=1

export CMAKE_BUILD_PARALLEL_LEVEL=$(nproc)
export CMAKE_GENERATOR=Ninja
export CMAKE_EXPORT_COMPILE_COMMANDS=1

# Enable the shell to send information to vterm via properly escaped
# sequences.
vterm_printf() {
  printf "\e]%s\e\\" "$1"
}

vterm_prompt_end() {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

# Let vterm know what dir I'm in
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

vterm_cmd() {
  local vterm_elisp
  vterm_elisp=""
  while [ $# -gt 0 ]; do
    vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
    shift
  done
  vterm_printf "51;E$vterm_elisp"
}

find_file() {
  vterm_cmd find-file "${PWD}/${@:-.}"
}

alias e="find_file"

[ -f "$HOME/.fzf.zsh" ] && source "$HOME/.fzf.zsh"
[ -f "$HOME/.work_aliases" ] && source "$HOME/.work_aliases"
