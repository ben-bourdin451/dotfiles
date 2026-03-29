#!/bin/zsh

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load: ~/.oh-my-zsh/themes/
ZSH_THEME="awesomepanda"

HYPHEN_INSENSITIVE="true"
# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# HIST_STAMPS="mm/dd/yyyy"
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(git aws npm nvm docker docker-compose)

source $ZSH/oh-my-zsh.sh
alias szsh='source ~/.zshrc'
export LANG=en_GB.UTF-8
[ -f "$HOME/env.sh" ] && source "$HOME/env.sh"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# PATH
export PATH=$PATH:/usr/local/bin:$HOME/.local/bin
[[ "$OSTYPE" == "darwin"* ]] && export PATH=$PATH:/usr/local/opt/coreutils/libexec/gnubin #core utils

[[ "$OSTYPE" == "darwin"* ]] && export CMAKE_OSX_ARCHITECTURES=arm64

###############
# Emacs
###############
export EDITOR="emacsclient -t"

# OS specific
if [[ "$OSTYPE" == "darwin"* ]]; then
		alias emacs="emacsappclient -c -nw"
		alias em="emacsapp"
		alias ec="emacsappclient"

		alias vpnreset='sudo ifconfig en0 down && sudo route -n flush && sudo ifconfig en0 up'

elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
		alias emacs="emacsclient"

		# Package manager aliases
		if command -v dnf &>/dev/null; then
			alias get='sudo dnf install -y'
			alias purge='sudo dnf remove -y'
		elif command -v apt-get &>/dev/null; then
			alias get='sudo apt-get install -y'
			alias purge='sudo apt-get purge -y'
		fi
		# fd is fdfind on Debian/Ubuntu only
		command -v fdfind &>/dev/null && ! command -v fd &>/dev/null && alias fd=fdfind
		# Clipboard aliases (only when tools are available)
		command -v copyq &>/dev/null && alias pbcopy='copyq add -' && alias pbpaste='copyq read 0'
fi
alias eprofile='emacsclient -nw $HOME/.zshrc'
alias etmuxconf='emacsclient -nw $HOME/.tmux.conf'
alias esshconf='emacsclient -nw $HOME/.ssh/config'

###############
# General
###############
alias ll='ls -la'
command -v bat &>/dev/null && alias cat='bat'

# find
command -v fd &>/dev/null && export FZF_DEFAULT_COMMAND='fd --type f -E .git -E .node_modules'
alias findf="fzf --preview 'bat --style=numbers --color=always {} | head -500'"

# man
if [[ "$OSTYPE" == "darwin"* ]]; then
		export MANPATH=$MANPATH:/opt/homebrew/opt/coreutils/libexec/gnuman
		alias man="batman" # batman is the man
fi

# openssl
if [[ "$OSTYPE" == "darwin"* ]]; then
		export LDFLAGS="-L/opt/homebrew/opt/openssl/lib"
		export CPPFLAGS="-I/opt/homebrew/opt/openssl/include"
		export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl/lib/pkgconfig"
fi

#########
# Python - sucks
#
# used by:
# - powerline
#########
# pyenv
export PYENV_ROOT=/usr/local/var/pyenv
if command -v pyenv &>/dev/null; then eval "$(pyenv init -)"; fi
[[ "$OSTYPE" == "darwin"* ]] && export PATH="/opt/homebrew/opt/python/libexec/bin:$PATH"

#########
# JS
#########
# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

export JSII_SILENCE_WARNING_UNTESTED_NODE_VERSION=1

#########
# Go
#########
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN

#########
# Ruby
#########

# RVM
# export PATH="$PATH:$HOME/.rvm/bin"
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

#########
# Flutter & Dart
#########
export PATH=$PATH:/usr/local/flutter/bin
export PATH=$PATH:/usr/local/android_sdk/cmdline-tools/bin
export ANDROID_HOME=/usr/local/android_sdk
export FLUTTER_GIT_URL=git@github.com:/flutter/flutter.git

#########
# AWS
#########
export AWS_DATA_PATH="$HOME/tools/aws-cli"
export AWS_PAGER=""
alias awsmfa="$HOME/aws_mfa.sh"
unalias awssso 2>/dev/null
awssso() { aws sso login --sso-session "$1"; }
alias cdk="npx aws-cdk --no-change-set"

#########
# Terraform
#########
# export TF_LOG="TRACE"
export TF_LOG_PATH="$HOME/.terraform.d/tf.log"
# export TF_VAR_gcp_creds="$HOME/.config/gcloud/application_default_credentials.json"

alias tf="terraform"
alias tflog='tail -f $TF_LOG_PATH'

tf-set-creds() {
    local creds
    creds=$(aws sts assume-role --role-arn "$TF_ROLE" --role-session-name 'ben-tf' --duration-seconds 3600)
    export AWS_ACCESS_KEY_ID=$(echo "$creds" | jq -r '.Credentials.AccessKeyId')
    export AWS_SECRET_ACCESS_KEY=$(echo "$creds" | jq -r '.Credentials.SecretAccessKey')
    export AWS_SESSION_TOKEN=$(echo "$creds" | jq -r '.Credentials.SessionToken')

    mv ~/.aws/credentials ~/.aws/credentials.prev
}
tf-unset-creds() {
    unset AWS_ACCESS_KEY_ID
    unset AWS_SECRET_ACCESS_KEY
    unset AWS_SESSION_TOKEN

    mv ~/.aws/credentials.prev ~/.aws/credentials
}

# cdktf completion
#compdef cdktf
###-begin-cdktf-completions-###
#
# yargs command completion script
#
# Installation: cdktf completion >> ~/.zshrc
#    or cdktf completion >> ~/.zsh_profile on OSX.
#
_cdktf_yargs_completions()
{
  local reply
  local si=$IFS
  IFS=$'
' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" cdktf --get-yargs-completions "${words[@]}"))
  IFS=$si
  _describe 'values' reply
}
compdef _cdktf_yargs_completions cdktf
###-end-cdktf-completions-###


#########
# GitHub App auth for gh CLI
#########
[[ -f "$HOME/.config/gh/env" ]] && source "$HOME/.config/gh/env"
gh() {
    local token=$("$HOME/.local/bin/gh-app-token" 2>/dev/null)
    GH_TOKEN="${token:-}" command gh "$@"
}
alias ghme='command gh'

#########
# Git
#########
alias gg='git grep -ri'
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias rmorig='find . -type f -name "*.orig" -exec rm {} \;'

gitgone() {
    git fetch -p && for branch in $(git branch -v | fgrep 'gone]' | awk '{print $1}'); do git branch -D $branch; done
}

wtgone() {
    git fetch -p
    for branch in $(git branch -v | fgrep 'gone]' | awk '{print $1}'); do
        wt=$(git worktree list | fgrep "[$branch]" | awk '{print $1}')
        if [ -n "$wt" ]; then
            git worktree remove --force "$wt"
            git worktree prune
        fi
        git branch -D "$branch"
    done
}

gitpurge() {
    git fetch -p && for branch in $(git branch -a --merged | fgrep 'remotes' | egrep -v 'remotes/origin/(release-candidate|master)$' | awk '{gsub(/remotes\/origin\//, ""); print}'); do git push origin :$branch --no-verify; done
}

#########
# Passwords
#########
pw() {
    LC_ALL=C tr -dc 'A-Za-z0-9$@&+-#_?!' </dev/random | head -c ${1:-20} | pbcopy
    # LC_ALL=C tr -dc 'A-Za-z0-9!#$%&()*+-:;<=>?@[\]^_{|}~' </dev/random | head -c ${1:-20} | pbcopy
}
pwtrunc() {
    local pw=$(pbpaste)
    if [[ -z "$pw" ]]; then
        echo "Nothing in clipboard"
        return
    fi

    if [[ -z "$1" ]]; then
        read "1?1st: "
    fi

    if [[ -z "$2" ]]; then
        read "2?2nd: "
    fi

    if [[ -z "$3" ]]; then
        read "3?3rd: "
    fi

    echo ${pw:$(($1-1)):1}${pw:$(($2-1)):1}${pw:$(($3-1)):1}
}
command -v op &>/dev/null && eval "$(op completion zsh)" && compdef _op op

#########
# Docker
#########

# Toolbox - /usr/local/bin/docker
alias dockerstart='docker-machine start && eval $(docker-machine env default)'
alias dockereval='eval $(docker-machine env default)'
alias dockerunset='unset DOCKER_TLS_VERIFY;unset DOCKER_CERT_PATH;unset DOCKER_MACHINE_NAME;unset DOCKER_HOST'

# Generic
alias dockerexec='docker exec -it $(docker ps -q)' # runs the command on first running container
alias dockerimgclean='docker rmi $(docker images -f "dangling=true" -q)'
alias dockerpsclean='docker rm $(docker ps -a -f status=exited -q)'
alias dockerclean='docker system prune --volumes'


#########
# Misc
#########

ulimit -n 10240 2>/dev/null

# Colors
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

# Custom functions
now() { date +%s }
tping() { ping "$@" | perl -nle "print scalar(localtime), \" \", \$_"; } # ping with timestamp
unescape() { pbpaste | sed 's/\\"/"/g' | sed 's/\\\\"/"/g' | sed 's/"{/{/g' | sed 's/}"/}/g' | pbcopy }
jqformat() { pbpaste | jq | pbcopy }

loadtest() {
		local DURATION=60 # seconds
		local TPS=20 # number of requests per second
		local end=$((SECONDS+$DURATION))
		#start load
		while [ $SECONDS -lt $end ];
		do
				for ((i=1;i<=$TPS;i++)); do
						curl -X POST <url> -H 'Accept: application/json' -H 'Authorization: Bearer xxxxxxxxxxxxx' -H 'Content-Type: application/json' -d '{}' --cacert /path/to/cert/cert.crt -o /dev/null -s -w '%{time_starttransfer}\n' >> response-times.log &
				done
				sleep 1
		done
		wait
		#end load
		echo "Load test has been completed"
}

healthcheck() {
		local DURATION=300 # seconds
		local TPS=${2:-1} # requests per second, default 1
		local end=$((SECONDS+$DURATION))

		while [ $SECONDS -lt $end ];
		do
				for ((i=1;i<=$TPS;i++)); do
						$1 &
				done
				sleep 1
		done
		wait

		echo "finished"
}

# Remove a stale entry from known_hosts by line number
known-hosts-rm() {
  local host
  host=$(sed -n "${1}p" ~/.ssh/known_hosts | cut -d' ' -f1)
  ssh-keygen -R "$host" && rm -f ~/.ssh/known_hosts.old && echo "Removed host key for $host"
}

alias rmlogs='find logs -type f -mtime +1 -exec rm {} \;'

# pnpm
if [[ "$OSTYPE" == "darwin"* ]]; then
		export PNPM_HOME="/Users/ben/Library/pnpm"
		case ":$PATH:" in
			*":$PNPM_HOME:"*) ;;
			*) export PATH="$PNPM_HOME:$PATH" ;;
		esac
fi
# pnpm end

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

fpath=(/Users/ben/.zsh/completions $fpath)
