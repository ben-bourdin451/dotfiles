#!/usr/bin/env bash
# SessionStart hook: inject dotfiles guidance ONLY when Claude Code is launched
# directly in the home directory (where dotfiles work happens). Stays out of the
# global context so it never loads for unrelated project sessions.
#
# OS-agnostic: keys off $PWD/$HOME and lives at a fixed ~/.claude path, so it
# behaves identically on macOS and every Linux distro the dotfiles sync to.

# Only act when the session's working directory is exactly $HOME.
[ "$PWD" = "$HOME" ] || exit 0

# Anything written to stdout from a SessionStart hook is added to Claude's context.
cat <<'EOF'
# Dotfiles (home directory)

My home directory (~) config files are tracked in a **bare git repo** at
`$HOME/.dotfiles/` (`core.bare=true`) with work-tree = `$HOME`, synced to
`git@github.com:ben-bourdin451/dotfiles.git` (branch `master`).

- Use the `config` alias for dotfiles git ops:
  `config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'`. In non-interactive
  shells the alias isn't loaded — use the explicit
  `git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME" …` form.
- Do NOT place files *inside* `$HOME/.dotfiles/` — that's the git metadata dir.
- **Untracked files are hidden** (`status.showUntrackedFiles=no`), so `config status`
  shows only tracked changes. Add new files explicitly by name — never
  `config add .` or `config add -A`.
- After editing a tracked file: `config add <file> && config commit -m "message"`
  (and `config push` when asked).
- Global gitignore is `$HOME/.gitignore` (via `core.excludesfile`); applies
  everywhere since work-tree is `$HOME`. Already ignores `.env`, `.DS_Store`, IDE
  files, Terraform state, `.claude/worktrees/`, etc.
- Personal scripts/tools live in `$HOME/scripts/<name>/` (e.g. the Wi-Fi reset tool
  at `~/scripts/hyperoptic-zyxel-wifi/`, secrets in a gitignored `.env`).
EOF
