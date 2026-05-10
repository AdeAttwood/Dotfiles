---
description: Start and implement the highest-priority ready AFK tk ticket for the current project
---

TK Ticket Work Workflow

Goal

Pick a ready AFK ticket for the current repository, start it in `tk`, implement
it in the current workspace, validate the change, and leave the ticket in
`in_progress`.

Rules

1. Detect `project:owner/repo` from `git remote get-url origin`.
2. If the user explicitly names a ticket ID, prefer that ticket after verifying
   it belongs to the current project and is workable.
3. Otherwise, find ready tickets for the current project with `tk ready -T`.
4. Do not rely on repeated `-T` flags for AND filtering. Filter AFK tickets by
   inspecting ticket tags after collecting project-scoped ready tickets.
5. Auto-pick only `mode:afk` tickets.
6. Select the ticket by highest priority first, then oldest creation time.
7. Start the chosen ticket with `tk start <id>` before editing code.
8. Read the chosen ticket and its parent context when relevant before making
   changes.
9. Implement the ticket in the current workspace.
10. Run relevant validation.
11. Do not commit.
12. Leave the ticket `in_progress` and report the ticket ID, title, and
    validation result.

Selection Procedure

1. Derive `owner/repo` from `origin`.
2. Run `tk ready -T "project:owner/repo"` to identify candidate ticket IDs.
3. Inspect each candidate with `tk show --json` or `tk query`.
4. Keep only tickets tagged `mode:afk`.
5. Sort candidates by `priority` ascending, then `created` ascending.
6. If no AFK ticket is ready, stop and say so.

Implementation Procedure

1. Start the chosen ticket.
2. Read the ticket body and parent ticket if present.
3. Make the smallest correct change.
4. Run tests, builds, or other relevant validation.
5. Stop before commit and leave the ticket `in_progress`.

Output

Report:

- The ticket ID and title that were started.
- The key code changes.
- What validation ran and whether it passed.
- That the ticket remains `in_progress`.
