# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code).

Check what agents & skills are available for use before starting a task.

## Generic rules

### git

- Use -C to run git commands in different directories
- **branch naming**: never use `/` in the branch names, use `-` or `_` instead. max 70 characters.

### github

- **PRs**: add my user (ben-bourdin451) as a reviewer to all PRs you create. enable auto-merge.

## Dotfiles

This is the home directory (`~`). Configuration files here are tracked in a bare git repo at `$HOME/.dotfiles/` with the work tree set to `$HOME`, synced to `git@github.com:ben-bourdin451/dotfiles.git`.

### Usage

Use the `config` alias instead of `git` for all dotfiles operations:

```
config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
```

Examples:
- `config status` — show changed tracked files
- `config add <file>` — stage a file
- `config commit -m "message"` — commit changes
- `config push` — push to remote

### Key details

- **Untracked files are hidden by default.** The repo sets `status.showUntrackedFiles=no` so `config status` only shows changes to already-tracked files. New files must be explicitly added with `config add`.
- **Branch:** `master`
- **`.gitignore`** at `$HOME/.gitignore` excludes common noise (`.DS_Store`, `.env`, IDE files, Terraform state, etc.). This gitignore applies globally since the work tree is `$HOME`.

### When editing dotfiles

- Use `config` (not `git`) for any add/commit/push/diff/log operations on tracked dotfiles.
- Only tracked files are part of the repo. Do not `config add .` or `config add -A` — always add specific files by name.
- After editing a tracked file, stage and commit with `config add <file> && config commit -m "message"`.
