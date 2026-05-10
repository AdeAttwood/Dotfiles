---
description: Close the current tk ticket after implementation without creating a commit
---

TK Ticket Close Workflow

Goal

Close the current `tk` ticket for the current repository after implementation,
without committing or changing anything else.

Rules

1. This command only closes a ticket. Do not stage, commit, or push.
2. Detect `project:owner/repo` from `git remote get-url origin`.
3. Prefer the ticket already active in the current OpenCode session when it is
   clear which ticket was started or worked on.
4. If the session does not make the ticket obvious, inspect `tk` for
   `in_progress` tickets in the current project.
5. If exactly one candidate is clear after that lookup, close it.
6. Otherwise, require an explicit ticket ID from the user.
7. Warn if the git worktree is dirty, but do not block closing unless the user
   changes course.
8. Only close the chosen ticket and report the closed ticket. Do not suggest
   the next ticket unless the user asks.

Resolution Procedure

1. Derive `owner/repo` from `origin`.
2. Check the current session for the most recently started or worked ticket in
   this project.
3. If no session ticket is clear, query `tk` for `in_progress` tickets and
   filter to `project:owner/repo`.
4. If exactly one ticket remains, use it.
5. If more than one remains, ask the user for the ID.

Close Procedure

1. Check git status and warn if the worktree is dirty.
2. Close the ticket with `tk close <id>`.
3. Confirm the ticket ID and title that were closed.

Output

Return only the closed ticket confirmation unless the user asks for more.
