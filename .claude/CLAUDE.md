# CLAUDE.md

Before starting implementation tasks, check available agents & skills.

## Generic rules

- Use `jq` instead of python when piping and filtering JSON in bash tool calls.
- Use -C to run git commands in different directories

## Testing

- **Colocate unit tests with source, 1:1 by name.** Place unit-test files next to the file they cover — never in a shared `tests/`, `__tests__/`, or `spec/` directory. Each test file maps to exactly one source file: `foo.ts` → `foo.test.ts`, `Hello.vue` → `Hello.test.ts`, `service.go` → `service_test.go`. When adding the first test in a project that currently uses a shared `tests/` dir, also migrate existing tests to the colocated layout and update any test-runner `include` globs and `tsconfig` paths. Integration and end-to-end tests are exempt — they may live in dedicated directories (e.g. `*_int_test.go`, `e2e/`, `integration/`).

## git & github

- **branch naming**: never use `/` in the branch names, use `-` or `_` instead. max 70 characters.
- **Never discard uncommitted work**: before running `git checkout -- .`, `git restore .`, `git clean -fd`, or `git stash` in any repo, first check `git status` and `git diff --stat`. If there are uncommitted changes, ask the user before discarding them. Use targeted `git checkout -- <file>` for specific files instead of blanket restores.
- when instructed to create PRs, always check to see if CI passes. if not keep iterating until it does.
- **Never rebase branches that exist on the remote**: do not rebase or otherwise rewrite history of any branch that has been pushed to a remote (this includes `git pull --rebase` on feature branches, `git rebase <base>`, `git rebase -i`, `git commit --amend` on pushed commits, etc.). Rewriting shared history breaks in-flight PR reviews. Note: my `~/.gitconfig` has `pull.rebase=true` — that is intentional for direct commits on `main`/`master` only; for feature branches that exist on the remote, use `git merge` (e.g. `git merge origin/main`) or `git pull --no-rebase --ff-only`/`--no-ff` instead. If a rebase truly seems necessary on a pushed branch, ask first.
- **worktree location**: for any git-tracked repo, place new worktrees at `<repo-root>/.claude/worktrees/<branch-name>` (e.g. `git -C <repo> worktree add .claude/worktrees/<branch> -b <branch> origin/<base>`). The path `.claude/worktrees/` is in the global gitignore (`~/.gitignore`, wired via `core.excludesfile`) so nested worktrees won't show up as untracked in the parent repo. Prefer this over sibling-directory conventions like `<repo>-worktrees/`.
