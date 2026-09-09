#!/bin/zsh

# Dedupe PATH entries — .zshenv runs for every shell (including nested ones),
# so without this, PATH grows on each invocation.
typeset -U path PATH fpath manpath MANPATH

# Homebrew + Python — STATIC prepends only, no subprocess. This file runs on
# every zsh invocation, so nothing here may fork. These exist purely so that
# non-login shells and scripts can still find brew-installed tools; the
# authoritative ordering is re-asserted in ~/.zprofile, which runs *after*
# /etc/zprofile's path_helper (which would otherwise demote them under /usr/bin).
[[ -x /opt/homebrew/bin/brew ]] && export PATH="/opt/homebrew/opt/python/libexec/bin:/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"

# Rust / cargo
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# Locale / editor
export LANG=en_GB.UTF-8
export EDITOR="emacsclient -t"

# ls colors
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

# PATH basics
export PATH=$PATH:/usr/local/bin:$HOME/.local/bin
if [[ "$OSTYPE" == "darwin"* ]]; then
		export PATH=$PATH:/usr/local/opt/coreutils/libexec/gnubin
		export CMAKE_OSX_ARCHITECTURES=arm64
fi

# Python / pyenv
export PYENV_ROOT=/usr/local/var/pyenv

# Node / nvm
export NVM_DIR="$HOME/.nvm"
export JSII_SILENCE_WARNING_UNTESTED_NODE_VERSION=1

# Go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN

# Flutter / Dart / Android
export PATH=$PATH:/usr/local/flutter/bin
export PATH=$PATH:/usr/local/android_sdk/cmdline-tools/bin
export ANDROID_HOME=/usr/local/android_sdk
export FLUTTER_GIT_URL=git@github.com:/flutter/flutter.git

# AWS
export AWS_DATA_PATH="$HOME/tools/aws-cli"
export AWS_PAGER=""

# Terraform
export TF_LOG_PATH="$HOME/.terraform.d/tf.log"

# fzf
command -v fd &>/dev/null && export FZF_DEFAULT_COMMAND='fd --type f -E .git -E .node_modules'

# pnpm
if [[ "$OSTYPE" == "darwin"* ]]; then
    export PNPM_HOME="/Users/ben/Library/pnpm"
    case ":$PATH:" in
        *":$PNPM_HOME:"*) ;;
        *) export PATH="$PNPM_HOME:$PATH" ;;
    esac
fi

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# env.sh — secrets / tokens (GH_TOKEN, GITHUB_TOKEN, etc.)
[ -f "$HOME/env.sh" ] && source "$HOME/env.sh"
