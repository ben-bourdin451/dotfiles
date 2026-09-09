---
name: handoff
description: >
  Generate a self-contained handoff prompt from the current conversation that
  can be pasted into a fresh agent context. Use when the user wants to spin off
  follow-up work, a side quest, or any task that surfaced during the current
  session but is better executed in a clean context. Writes the prompt to
  /tmp/claude-handoff-<timestamp>.md and copies it to the macOS clipboard via
  pbcopy. Triggered by "/handoff", "handoff this", "spin this off", "write a
  handoff", or similar.
---

# Handoff Skill

You are producing a **handoff prompt** — a self-contained brief that a fresh agent can act on with no prior context. The output goes to a file in `/tmp` and to the clipboard so the user can paste it into a new agent session.

## Inputs

The skill may be invoked with or without an argument:

- **With focus** (`/handoff fix the auth middleware drift`): the argument scopes the handoff. Build the brief around that focus, pulling supporting context from the conversation.
- **Without focus** (`/handoff`): scan the recent conversation for candidate tasks with **recency bias** (later turns matter more than earlier ones). Candidates are things like:
  - Work the current agent identified but didn't do ("outstanding now: X, Y, Z")
  - Side quests the agent surfaced as independent (unrelated bugs, refactors, drift)
  - Items the user said "later" or "separately" to
  - Follow-ups implied by a partial fix ("only the tooling — none of the underlying drift fixed")

## Selecting the candidate

1. **One obvious candidate** → write it directly, no prompt.
2. **Multiple candidates** → use `AskUserQuestion` with each candidate as an option (max 4, most recent first). Allow `multiSelect: true` so the user can bundle several into one handoff if they want. Each selected candidate becomes one section in the output.
3. **No candidates found** → ask the user what to hand off. Do not fabricate work.

## What the handoff must contain

A fresh agent has **zero context**. Write as if briefing a smart colleague who just walked in. Each handoff section should have:

```
# <Task title — imperative, one line>

## Context
- Repo / working dir: <absolute path>
- Branch: <branch name, if in a git repo>
- Relevant commit / PR: <if any>

## Background
<2–5 sentences: what led to this task. What was tried, what worked, what didn't,
and why this piece was deferred. Include only what the new agent needs to make
judgment calls — don't dump the full conversation.>

## What needs to happen
<Concrete scope. Bullet points if there are multiple steps. Be specific about
files, functions, or behaviors. Avoid hand-waving like "clean up the module".>

## Relevant files
- `path/to/file.ext:LINE` — why it matters
- `path/to/other.ext` — why it matters

## Out of scope
<Things the new agent should NOT touch. Especially important when the parent
conversation surfaced multiple issues and only one is being handed off.>

## Verification
<How the new agent will know it's done — tests to run, behavior to observe,
checks to pass.>
```

Skip any section that genuinely has nothing to say — don't pad with "N/A". But `Context`, `What needs to happen`, and `Verification` are required.

If multiple candidates were selected, separate each with a `---` divider and repeat the structure per task. Add a short top-level note: `> Handoff bundle: N independent tasks. Each section is self-contained — execute in separate agent sessions unless dependencies are noted.`

## Tone and length

- Write the brief in second person ("You are picking up...") or imperative — the reader is the next agent, not the user.
- Aim for ~150–400 words per task. Long enough to be self-contained, short enough that nothing is fluff.
- Don't quote the prior conversation verbatim. Summarize. The handoff is a brief, not a transcript.
- Include file paths with line numbers where they matter. Resolve relative dates to absolute (e.g. "earlier today" → the actual date).
- Don't reference the previous agent or session ("the previous agent found..."). State facts about the code/state directly.

## Writing the file

1. Compute timestamp: `date +%Y%m%d-%H%M%S` via Bash.
2. Path: `/tmp/claude-handoff-<timestamp>.md`.
3. Write the handoff content with the `Write` tool.
4. Copy to clipboard: `pbcopy < /tmp/claude-handoff-<timestamp>.md` via Bash.
5. Report back to the user with: the path, a one-line summary of what was handed off, and a hint that it's on the clipboard ready to paste.

Example report:

```
Handoff written → /tmp/claude-handoff-20260522-143812.md (copied to clipboard).
Task: Fix unbounded query drift in `internal/users/handler.go` flagged during tooling pass.
```

## Don't

- Don't launch a new agent yourself — the user wants to paste this into a fresh session of their choosing.
- Don't include secrets, tokens, or full file contents. Reference paths instead.
- Don't include conversation pleasantries, agent reasoning, or meta-commentary.
- Don't write the handoff into the project directory — it's a throwaway artifact, `/tmp` is correct.
- Don't write multiple files for a multi-task bundle — one file, multiple sections.
