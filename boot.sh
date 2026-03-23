#!/bin/sh
set -e

REPO_DOTFILES=${REPO_DOTFILES:-git@github.com:ben-bourdin451/dotfiles.git}

command_exists() {
	command -v "$@" >/dev/null 2>&1
}

error() {
	echo ${RED}"Error: $@"${RESET} >&2
}

setup_color() {
	# Only use colors if connected to a terminal
	if [ -t 1 ]; then
		RED=$(printf '\033[31m')
		GREEN=$(printf '\033[32m')
		YELLOW=$(printf '\033[33m')
		BLUE=$(printf '\033[34m')
		BOLD=$(printf '\033[1m')
		RESET=$(printf '\033[m')
	else
		RED=""
		GREEN=""
		YELLOW=""
		BLUE=""
		BOLD=""
		RESET=""
	fi
}

config() {
	git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
}


setup_cfg() {
	echo "${BLUE}Cloning dotfiles...${RESET}"

	command_exists git || {
		error "git is not installed"
		exit 1
	}

	if [ "$OSTYPE" = cygwin ] && git --version | grep -q msysgit; then
		error "Windows/MSYS Git is not supported on Cygwin"
		error "Make sure the Cygwin git package is installed and is first on the \$PATH"
		exit 1
	fi

	git clone --bare "$REPO_DOTFILES" "$HOME/.dotfiles" || {
		error "git clone of dotfiles repo failed"
		exit 1
	}

	config checkout || {
		error "dotfiles checkout failed"
		exit 1
	}
	config config --local status.showUntrackedFiles no

	echo
}

common_installs() {
	command_exists node || {
		echo "Installing nvm & node..."
		sh -c "$(curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh)"
	}

	# diff-so-fancy
	sudo git clone https://github.com/so-fancy/diff-so-fancy.git /usr/local/src
	sudo ln -s /usr/local/src/diff-so-fancy/diff-so-fancy /usr/local/bin/diff-so-fancy

	# echo "TODO: install golang"
	# GO111MODULE=on go get golang.org/x/tools/gopls@latest
	# https://golang.org/doc/install

	aws_cli
}

darwin_installs() {
	if ! [ -x "$(command -v brew)" ]; then
		echo "Installing homebrew..."
		/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	fi

	brew install git \
		fzf \
		ripgrep \
		fd \
		bat eth-p/software/bat-extras \
		diff-so-fancy \
		jq \
		yarn
}

apt_installs() {
	echo "Apt installs"
	sudo apt-get update && apt-get install -y \
		emacs \
		fzf \
		bat \
		fd-find \
		ripgrep

	sudo ln -s /usr/bin/batcat /usr/local/bin/bat
}

aws_cli() {
	command_exists aws || {
		if [[ "$OSTYPE" == "darwin"* ]]; then
			echo "Installing AWS CLI..."
			curl "https://awscli.amazonaws.com/AWSCLIV2.pkg" -o "AWSCLIV2.pkg"
			sudo installer -pkg AWSCLIV2.pkg -target /
		fi
	}
}

main() {
	setup_color

	if ! [ -d "$HOME/.oh-my-zsh" ]; then
		echo "Installing zsh..."
		sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
	fi

	if [ -d "$HOME/.dotfiles" ]; then
		echo "${YELLOW}Config is already setup${RESET}"
	else
		setup_cfg
	fi

	common_installs

	if [[ "$OSTYPE" == "darwin"* ]]; then
		darwin_installs
	elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
		apt_installs
	fi
}

main "$@"
