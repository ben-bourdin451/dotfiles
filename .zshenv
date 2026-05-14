#!/bin/zsh

# Dedupe PATH entries — .zshenv runs for every shell (including nested ones),
# so without this, PATH grows on each invocation.
typeset -U path PATH fpath manpath MANPATH

# Homebrew
[[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"

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

# macOS + homebrew build flags
if [[ "$OSTYPE" == "darwin"* ]] && command -v brew &>/dev/null; then
    export MANPATH=$MANPATH:/opt/homebrew/opt/coreutils/libexec/gnuman

		OPENSSL_PREFIX=$(brew --prefix openssl 2>/dev/null)
    if [[ -n "$OPENSSL_PREFIX" ]]; then
        export PATH="$OPENSSL_PREFIX/bin:$PATH"
        export LDFLAGS="-L$OPENSSL_PREFIX/lib"
        export CPPFLAGS="-I$OPENSSL_PREFIX/include"
        export PKG_CONFIG_PATH="$OPENSSL_PREFIX/lib/pkgconfig"
    fi
fi

# Python / pyenv
export PYENV_ROOT=/usr/local/var/pyenv
[[ "$OSTYPE" == "darwin"* ]] && export PATH="/opt/homebrew/opt/python/libexec/bin:$PATH"

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
