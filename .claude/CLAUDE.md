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

## Scope

Deliver what was asked, at the scope intended. Make routine judgment calls yourself, and check in only when different readings of the request would lead to materially different work. If the request seems mistaken or a better approach exists, say so in a sentence and continue with the task as asked rather than quietly narrowing, widening, or transforming it. Finish the whole task, and stop short of actions that are clearly beyond what was asked.

If, while working or testing, you find a pre-existing bug, a performance concern, or behavior the task doesn't mention, don't fix, optimize or extend it in this change unless the requested behavior cannot work without it; report it as a follow-up in your summary.

Where the task is ambiguous, implement the reading its wording and the surrounding code most directly support, state that assumption in your summary, and don't build for the other readings as well. Verify your work however you like; scratch scripts and quick checks need not be kept. Commit tests only where the task asks for them or this repository already keeps tests for this kind of change, sized like the neighboring test files — roughly one focused test per stated behavior — and don't turn scratch checks into additional permanent test files. This is about extras only: implement every behavior the task asks for, completely.

## Long execution

When you have enough information to act, act. Do not re-derive facts already established in the conversation, re-litigate a decision the user has already made, or narrate options you will not pursue in user-facing messages. If you are weighing a choice, give a recommendation, not an exhaustive survey. This does not apply to thinking blocks.
