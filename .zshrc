#!/bin/bash

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
plugins=(git aws npm nvm pip docker docker-compose kubectl rvm gem)

source $ZSH/oh-my-zsh.sh
alias szsh='source ~/.zshrc'
export LANG=en_GB.UTF-8
source $HOME/env.sh

# PATH
export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin
export PATH=$PATH:/usr/local/opt/coreutils/libexec/gnubin #core utils

# Emacs
if [[ -n $SSH_CONNECTION ]]; then
		export EDITOR="emacs"
else
		export EDITOR="emacs"
fi

# OS specific
if [[ "$OSTYPE" == "darwin"* ]]; then
		alias emacs="emacsappclient -c -nw"
		alias em="emacsapp"
		alias ec="emacsappclient"

		alias vpnreset='sudo ifconfig en0 down && sudo route -n flush && sudo ifconfig en0 up'

elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
		alias emacs="emacsclient"

		alias get='sudo apt-get install -y'
    alias purge='sudo apt-get purge -y'
    alias fd=fdfind
    alias pbcopy='copyq add -'
    alias pbpaste='copyq read 0'
fi
alias eprofile='emacs -nw $HOME/.zshrc'
alias etmuxconf='emacs -nw $HOME/.tmux.conf'
alias esshconf='emacs -nw $HOME/.ssh/config'

###############
# General
###############
alias ll='ls -la'
alias cat='bat'

# find
export FZF_DEFAULT_COMMAND='fd --type f -E .git -E .node_modules'
alias findf="fzf --preview 'bat --style=numbers --color=always {} | head -500'"

# man
export MANPATH=$MANPATH:/usr/local/opt/coreutils/libexec/gnuman
if [[ "$OSTYPE" == "darwin"* ]]; then
		alias man="batman" # batman is the man
fi

# openssl
export PATH=$PATH:/usr/local/opt/openssl@1.1/bin
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include"
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"

#########
# Python - sucks
#########
export PATH=$PATH:$HOME/.local/bin

# pyenv
export PYENV_ROOT=/usr/local/var/pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

#########
# JS
#########
export PATH=$PATH:$HOME/.nvm/versions/node/v13.13.0/bin

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # load nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # load nvm bash_completion


#########
# Java
#########
# jenv
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"


#########
# Go
#########
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN
alias go="$GOPATH/bin/go"

#########
# Ruby
#########

# RVM
# export PATH="$PATH:$HOME/.rvm/bin"
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*


#########
# AWS
#########
export AWS_DATA_PATH="$HOME/tools/aws-cli"
alias awsmfa="$HOME/aws_mfa.sh"


#########
# Terraform
#########
# export TF_LOG="TRACE"
export TF_LOG_PATH="$HOME/terraform/tf.log"
# export TF_VAR_gcp_creds="$HOME/.config/gcloud/application_default_credentials.json"

alias tf="terraform"
alias tflog='tail -f $HOME/terraform/tf.log'

tf-set-creds() {
    creds=$(aws sts assume-role --role-arn $TF_ROLE --role-session-name 'ben-tf' --duration-seconds 3600)
    export AWS_ACCESS_KEY_ID=$(echo $creds | jq '.Credentials.AccessKeyId' | sed 's/"//g')
    export AWS_SECRET_ACCESS_KEY=$(echo $creds | jq '.Credentials.SecretAccessKey' | sed 's/"//g')
    export AWS_SESSION_TOKEN=$(echo $creds | jq '.Credentials.SessionToken' | sed 's/"//g')

    mv ~/.aws/credentials ~/.aws/credentials.prev
}
tf-unset-creds() {
    unset AWS_ACCESS_KEY_ID
    unset AWS_SECRET_ACCESS_KEY
    unset AWS_SESSION_TOKEN

    mv ~/.aws/credentials.prev ~/.aws/credentials
}


#########
# Git
#########
alias gg='git grep -ri'
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias rmorig='find . -type f -name "*.orig" -exec rm {} \;'

gitgone() {
    git fetch -p && for branch in $(git branch -v | fgrep 'gone]'| awk '{$1=$1;print}' | perl -n -l -e '/^([a-zA-Z\/0-9-_]+)/ && print $1'); do git branch -D $branch; done
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
    pw=$(pbpaste)
    if [ -z $pw ]; then
        echo "Nothing in clipboard"
        return
    fi

    if [ -z $1 ]; then
        read "1?1st: "
    fi

    if [ -z $2 ]; then
        read "2?2nd: "
    fi

    if [ -z $3 ]; then
        read "3?3rd: "
    fi

    echo ${pw:$(($1-1)):1}${pw:$(($2-1)):1}${pw:$(($3-1)):1}
}


#########
# Docker
#########

# Toolbox - /usr/local/bin/docker
alias dockerstart='docker-machine start && eval $(docker-machine env default)'
alias dockereval='eval $(docker-machine env default)'
alias dockerunset='unset DOCKER_TLS_VERIFY;unset DOCKER_CERT_PATH;unset DOCKER_MACHINE_NAME;unset DOCKER_HOST'

# Generic
alias dockerexec='docker exec -it $(docker ps -q)' # runs the command on first running container
alias dockerimageclean='docker rmi $(docker images -f "dangling=true" -q)'
alias dockerpsclean='docker rm $(docker ps -a -f status=exited -q)'
alias dockerclean='dockerpsclean && dockerimageclean'


#########
# Misc
#########

ulimit -n 10240

# Colors
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

# Custom functions
now() { date +%s }
tping() { ping "$@" | perl -nle "print scalar(localtime), " ", $_"; } # ping with timestamp
unescape() { pbpaste | sed 's/\\"/"/g' | sed 's/\\\\"/"/g' | sed 's/"{/{/g' | sed 's/}"/}/g' | pbcopy }

loadtest() {
		DURATION=60 # seconds
		TPS=20 # number of requests per second
		end=$((SECONDS+$DURATION))
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
		DURATION=300 # seconds
		end=$((SECONDS+$DURATION))

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

alias rmlogs='find logs -type f -mtime +1 -exec rm {} \;'
alias rmsublworkspaces='find $HOME/workspace -type f -name "*.sublime-workspace" -exec rm {} \;'
