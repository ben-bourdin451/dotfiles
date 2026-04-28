# CLAUDE.md

Check what agents & skills are available for use before starting a task.

## Generic rules

- Use `jq` instead of python when piping and filtering JSON in bash tool calls.
- Use -C to run git commands in different directories

## git & github

- **branch naming**: never use `/` in the branch names, use `-` or `_` instead. max 70 characters.
- **Never discard uncommitted work**: before running `git checkout -- .`, `git restore .`, `git clean -fd`, or `git stash` in any repo, first check `git status` and `git diff --stat`. If there are uncommitted changes, ask the user before discarding them. Use targeted `git checkout -- <file>` for specific files instead of blanket restores.
- when instructed to create PRs, always check to see if CI passes. if not keep iterating until it does.
- **Never rebase branches that exist on the remote**: do not rebase or otherwise rewrite history of any branch that has been pushed to a remote (this includes `git pull --rebase` on feature branches, `git rebase <base>`, `git rebase -i`, `git commit --amend` on pushed commits, etc.). Rewriting shared history breaks in-flight PR reviews. Note: my `~/.gitconfig` has `pull.rebase=true` — that is intentional for direct commits on `main`/`master` only; for feature branches that exist on the remote, use `git merge` (e.g. `git merge origin/main`) or `git pull --no-rebase --ff-only`/`--no-ff` instead. If a rebase truly seems necessary on a pushed branch, ask first.
- **worktree location**: for any git-tracked repo, place new worktrees at `<repo-root>/.claude/worktrees/<branch-name>` (e.g. `git -C <repo> worktree add .claude/worktrees/<branch> -b <branch> origin/<base>`). The path `.claude/worktrees/` is in the global gitignore (`~/.gitignore`, wired via `core.excludesfile`) so nested worktrees won't show up as untracked in the parent repo. Prefer this over sibling-directory conventions like `<repo>-worktrees/`.

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
