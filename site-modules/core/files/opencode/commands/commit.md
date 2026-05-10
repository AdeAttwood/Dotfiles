---
description: Create a focused commit using the repository's recent commit style
---

Repository-Aware Commit Workflow

Goal

Create a focused git commit whose title, body, and test plan match the style
used by the current repository.

Requested commit guidance: `$ARGUMENTS`

Rules

1. Inspect the worktree before staging anything.
2. Inspect the last 10 commits before drafting the message:
   `git log -10 --format='%h%n%s%n%b%n---'`.
3. Use the recent commits to detect the dominant title style,
   capitalization, ticket format, tone, and wording.
4. Prefer the detected repository style unless the user explicitly requests a
   different style in `$ARGUMENTS`.
5. Stage only files relevant to this commit.
6. Review the staged diff before committing.
7. Keep the commit focused; split unrelated work instead of combining it.
8. Do not commit files that likely contain secrets.
9. Do not amend, force push, or skip hooks unless the user explicitly asks.

Workflow

1. Run `git status --short` to identify tracked and untracked changes.
2. Run `git diff --stat` and `git diff` to inspect unstaged changes.
3. Run `git log -10 --format='%h%n%s%n%b%n---'` to learn the local commit
   style.
4. Stage the relevant files with `git add <paths>`.
5. Run `git diff --cached --stat` and `git diff --cached` to verify the exact
   staged changes.
6. Draft a commit message from the staged diff and detected style.
7. Commit with `git commit -m "<title>" -m "<body>"`.
8. Run `git status --short` after the commit to verify the result.

Style Detection

- If recent titles use Conventional Commits, use
  `<type>(<scope>): <summary>` or `<type>: <summary>`.
- If recent titles use JIRA keys, use `[KEY-123] <Summary>` when a ticket is
  available.
- If recent titles are plain imperative summaries, use that style.
- Always use `Summary:` and `Test Plan:` body sections.
- Match recent body tone and wording when it is consistent.
- Keep title summaries imperative and no longer than 72 characters.
- Wrap body text at 72 characters per line.

Body Template

Summary:

Explain what changed and why. Start with user or system impact. Describe root
cause if this is a fix. Call out behavior changes explicitly if applicable.

Test Plan:

1. Run `<exact command>`.
2. Run `<another exact command>`.
3. Verify any required manual behavior.

Test Plan Rules

- Build the Test Plan from validation commands that were actually run or from
  reproducible manual steps.
- Record the command line before running validation so the Test Plan can use
  the command, not its output.
- Include exact commands, for example `dotnet test` or
  `npm test -- --runInBand`.
- Never paste command output into the Test Plan.
- Do not include restore banners, compiler output, generated artifact paths,
  test counts, durations, or `Passed!` lines.
- If validation ran in CI, include the CI job name or CI command.
- Use `Not tested` only when no validation was performed, and include why.

Good Test Plan Example

Test Plan:

1. Run `dotnet test test/TcpService.Core.Tests/TcpService.Core.Tests.csproj`.
2. Run `npm test`.
3. Confirm the updated behavior manually in the browser.
