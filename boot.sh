#!/bin/bash
set -e

REPO_DOTFILES=${REPO_DOTFILES:-git@github.com:ben-bourdin451/dotfiles.git}

command_exists() {
	command -v "$@" >/dev/null 2>&1
}

ok() {
	echo "✅ $1"
}

update_available() {
	echo "⬆️  $1 — ${YELLOW}update available: $2${RESET}"
}

error() {
	echo ${RED}"Error: $@"${RESET} >&2
}

# Fetch latest release tag from a GitHub repo (e.g. "BurntSushi/ripgrep")
github_latest_tag() {
	local tag
	tag=$(curl -fsSL "https://api.github.com/repos/$1/releases/latest" 2>/dev/null | grep -o '"tag_name": *"[^"]*"' | cut -d'"' -f4)
	if [[ -z "$tag" ]]; then
		error "Failed to fetch latest release for $1 (GitHub API rate limit?)"
		return 1
	fi
	echo "$tag"
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

OS=""
detect_os() {
	if [[ "$OSTYPE" == "darwin"* ]]; then
		OS="macos"
	elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
		. /etc/os-release
		case "$ID" in
			amzn) OS="al2023" ;;
			ubuntu|debian) OS="debian" ;;
			*) error "Unsupported distro: $ID"; exit 1 ;;
		esac
	fi
}

config() {
	git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $@
}

setup_cfg() {
	command_exists git || {
		error "git is not installed"
		exit 1
	}

	if [ -d "$HOME/.dotfiles" ]; then
		config fetch -q 2>/dev/null || true
		local local_head remote_head
		local_head=$(config rev-parse HEAD 2>/dev/null)
		remote_head=$(config rev-parse @{u} 2>/dev/null || echo "$local_head")
		if [[ "$local_head" != "$remote_head" ]]; then
			update_available "dotfiles" "$(config log --oneline HEAD..@{u} 2>/dev/null | wc -l | tr -d ' ') commit(s) behind"
		else
			ok "dotfiles"
		fi
		return
	fi

	echo "Cloning dotfiles..."
	git clone --bare "$REPO_DOTFILES" "$HOME/.dotfiles" || {
		error "git clone of dotfiles repo failed"
		exit 1
	}

	config checkout -f || {
		error "dotfiles checkout failed"
		exit 1
	}
	config config --local status.showUntrackedFiles no
}

common_installs() {
	local pkgs=(zsh htop jq tar gzip tmux)
	case "$OS" in
		macos)  ;; # handled by darwin_installs (brew)
		al2023) sudo dnf install -y "${pkgs[@]}" ;;
		debian) sudo apt-get update && sudo apt-get install -y "${pkgs[@]}" ;;
	esac
}

darwin_installs() {
	if command_exists brew; then
		ok "homebrew"
	else
		echo "Installing homebrew..."
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
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

al2023_installs() {
	# emacs (headless)
	if command_exists emacs; then
		ok "emacs"
	else
		sudo dnf install -y emacs-nox
	fi

	# Go — latest stable ARM64
	if command_exists go; then
		local go_installed go_latest
		go_installed=$(go version | awk '{print $3}')
		go_latest=$(curl -fsSL 'https://go.dev/dl/?mode=json' 2>/dev/null | grep -o '"go[0-9.]*"' | head -1 | tr -d '"')
		if [[ -n "$go_latest" && "$go_installed" != "$go_latest" ]]; then
			update_available "go ($go_installed)" "$go_latest"
		else
			ok "go ($go_installed)"
		fi
	else
		echo "Installing Go..."
		GO_VERSION=$(curl -fsSL 'https://go.dev/dl/?mode=json' | grep -o '"go[0-9.]*"' | head -1 | tr -d '"')
		curl -fsSL "https://go.dev/dl/$GO_VERSION.linux-arm64.tar.gz" | sudo tar -C /usr/local -xzf -
	fi

	# GitHub CLI
	if command_exists gh; then
		ok "gh ($(gh --version | head -1 | awk '{print $3}'))"
	else
		echo "Installing GitHub CLI..."
		sudo dnf install -y 'dnf-command(config-manager)'
		sudo dnf config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo
		sudo dnf install -y gh
	fi

	# ripgrep
	if command_exists rg; then
		local rg_installed rg_latest
		rg_installed=$(rg --version | head -1 | awk '{print $2}')
		rg_latest=$(github_latest_tag "BurntSushi/ripgrep")
		if [[ -n "$rg_latest" && "$rg_installed" != "$rg_latest" ]]; then
			update_available "ripgrep ($rg_installed)" "$rg_latest"
		else
			ok "ripgrep ($rg_installed)"
		fi
	else
		echo "Installing ripgrep..."
		RG_VERSION=$(github_latest_tag "BurntSushi/ripgrep")
		curl -fsSL "https://github.com/BurntSushi/ripgrep/releases/download/$RG_VERSION/ripgrep-$RG_VERSION-aarch64-unknown-linux-gnu.tar.gz" | sudo tar -xzf - --strip-components=1 -C /usr/local/bin "ripgrep-$RG_VERSION-aarch64-unknown-linux-gnu/rg"
	fi

	# fd
	if command_exists fd; then
		local fd_installed fd_latest
		fd_installed=$(fd --version | awk '{print $2}')
		fd_latest=$(github_latest_tag "sharkdp/fd")
		fd_latest=${fd_latest#v}
		if [[ -n "$fd_latest" && "$fd_installed" != "$fd_latest" ]]; then
			update_available "fd ($fd_installed)" "$fd_latest"
		else
			ok "fd ($fd_installed)"
		fi
	else
		echo "Installing fd..."
		FD_VERSION=$(github_latest_tag "sharkdp/fd")
		curl -fsSL "https://github.com/sharkdp/fd/releases/download/$FD_VERSION/fd-$FD_VERSION-aarch64-unknown-linux-gnu.tar.gz" | sudo tar -xzf - --strip-components=1 -C /usr/local/bin "fd-$FD_VERSION-aarch64-unknown-linux-gnu/fd"
	fi

	# bat
	if command_exists bat; then
		local bat_installed bat_latest
		bat_installed=$(bat --version | awk '{print $2}')
		bat_latest=$(github_latest_tag "sharkdp/bat")
		bat_latest=${bat_latest#v}
		if [[ -n "$bat_latest" && "$bat_installed" != "$bat_latest" ]]; then
			update_available "bat ($bat_installed)" "$bat_latest"
		else
			ok "bat ($bat_installed)"
		fi
	else
		echo "Installing bat..."
		BAT_VERSION=$(github_latest_tag "sharkdp/bat")
		curl -fsSL "https://github.com/sharkdp/bat/releases/download/$BAT_VERSION/bat-$BAT_VERSION-aarch64-unknown-linux-gnu.tar.gz" | sudo tar -xzf - --strip-components=1 -C /usr/local/bin "bat-$BAT_VERSION-aarch64-unknown-linux-gnu/bat"
	fi

	# fzf (git-based — pull to update)
	if command_exists fzf; then
		if [ -d "$HOME/.fzf" ]; then
			local fzf_before fzf_after
			fzf_before=$(git -C "$HOME/.fzf" rev-parse HEAD 2>/dev/null)
			git -C "$HOME/.fzf" pull -q 2>/dev/null || true
			fzf_after=$(git -C "$HOME/.fzf" rev-parse HEAD 2>/dev/null)
			if [[ "$fzf_before" != "$fzf_after" ]]; then
				"$HOME/.fzf/install" --all --no-update-rc > /dev/null 2>&1
				ok "fzf (updated to $(fzf --version | awk '{print $1}'))"
			else
				ok "fzf ($(fzf --version | awk '{print $1}'))"
			fi
		else
			ok "fzf ($(fzf --version | awk '{print $1}'))"
		fi
	else
		echo "Installing fzf..."
		if [ -d "$HOME/.fzf" ]; then
			git -C "$HOME/.fzf" pull -q 2>/dev/null || true
		else
			git clone --depth 1 https://github.com/junegunn/fzf.git "$HOME/.fzf"
		fi
		"$HOME/.fzf/install" --all --no-update-rc
	fi

	# diff-so-fancy (git-based — pull to update)
	if [ -d /usr/local/src/diff-so-fancy ]; then
		sudo git -C /usr/local/src/diff-so-fancy pull -q 2>/dev/null || true
		ok "diff-so-fancy"
	else
		echo "Installing diff-so-fancy..."
		sudo git clone https://github.com/so-fancy/diff-so-fancy.git /usr/local/src/diff-so-fancy
		sudo ln -sf /usr/local/src/diff-so-fancy/diff-so-fancy /usr/local/bin/diff-so-fancy
	fi
}

apt_installs() {
	echo "Apt installs"
	sudo apt-get update && sudo apt-get install -y \
		emacs \
		fzf \
		bat \
		fd-find \
		ripgrep

	# bat is installed as batcat on Debian/Ubuntu
	if command_exists batcat && ! command_exists bat; then
		sudo ln -sf /usr/bin/batcat /usr/local/bin/bat
	fi

	# diff-so-fancy (git-based — pull to update)
	if [ -d /usr/local/src/diff-so-fancy ]; then
		sudo git -C /usr/local/src/diff-so-fancy pull -q 2>/dev/null || true
		ok "diff-so-fancy"
	else
		sudo git clone https://github.com/so-fancy/diff-so-fancy.git /usr/local/src/diff-so-fancy
		sudo ln -sf /usr/local/src/diff-so-fancy/diff-so-fancy /usr/local/bin/diff-so-fancy
	fi
}

aws_cli() {
	if command_exists aws; then
		ok "aws-cli ($(aws --version | awk '{print $1}' | cut -d/ -f2))"
		return
	fi
	if [[ "$OS" == "macos" ]]; then
		echo "Installing AWS CLI..."
		curl "https://awscli.amazonaws.com/AWSCLIV2.pkg" -o "AWSCLIV2.pkg"
		sudo installer -pkg AWSCLIV2.pkg -target /
	fi
}

user_tools() {
	# oh-my-zsh (git-based — pull to update)
	if [ -d "$HOME/.oh-my-zsh" ]; then
		git -C "$HOME/.oh-my-zsh" pull -q 2>/dev/null || true
		ok "oh-my-zsh"
	else
		echo "Installing oh-my-zsh..."
		KEEP_ZSHRC=yes sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
	fi

	# nvm + node
	if [ -d "$HOME/.nvm" ]; then
		ok "nvm"
	else
		echo "Installing nvm..."
		NVM_VERSION=$(github_latest_tag "nvm-sh/nvm")
		curl -fsSL "https://raw.githubusercontent.com/nvm-sh/nvm/$NVM_VERSION/install.sh" | bash
	fi
	export NVM_DIR="$HOME/.nvm"
	[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
	if ! command_exists node; then
		nvm install --lts
	fi

	# pnpm
	if command_exists pnpm; then
		npm install -g pnpm@latest > /dev/null 2>&1 || true
		ok "pnpm ($(pnpm --version))"
	else
		echo "Installing pnpm..."
		npm install -g pnpm
	fi

	# Claude Code
	if command_exists claude; then
		ok "claude"
	else
		echo "Installing Claude Code..."
		curl -fsSL https://claude.ai/install.sh | bash
	fi

	# Dotfiles
	setup_cfg
}

main() {
	setup_color
	detect_os

	echo "${GREEN}Detected OS: $OS${RESET}"
	echo ""

	common_installs

	case "$OS" in
		macos)  darwin_installs ;;
		al2023) al2023_installs ;;
		debian) apt_installs ;;
	esac

	aws_cli
	user_tools

	echo ""
	echo "${GREEN}All done!${RESET}"
}

main "$@"
