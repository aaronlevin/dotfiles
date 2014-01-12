#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# PATH
GEMPATH="$(ruby -rubygems -e "puts Gem.user_dir")/bin"
CABALPATH="/home/aaron/.cabal/bin"
HOMEPATH="/home/aaron/bin"
export PATH=$CABALPATH:$PATH:$GEMPATH:$HOMEPATH

alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
# PS1='[\u@\h \W]\$ '

# Add current git branch to PS1
function __git_inside_repo {
  git rev-parse &>/dev/null
}

PS1_TOGGLE="CONCISE"

function __git_ps1 {
  if __git_inside_repo; then
    local branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [ "$branch" == "HEAD" ]; then
      branch=$(git rev-parse --short HEAD 2>/dev/null)
    fi
    echo -n "($branch) "
  fi
}

II="concise"
alias ii='. $HOME/bin/ii-toggle'
ii

