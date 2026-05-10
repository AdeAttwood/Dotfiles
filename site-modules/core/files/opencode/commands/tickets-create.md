---
description: Break work into tk tickets with project/workspace tags, templates, dependencies, and scheduling
---

TK Ticket Creation Workflow

Goal

Turn a feature, bug, or PRD-style request into a consistent set of `tk`
tickets for the current workspace, including scheduling executable work onto
the workspace queue.

Rules

1. Detect the project from `origin` and tag every created ticket with
   `project:owner/repo`.
2. Detect the workspace ID from the current repository directory basename and
   schedule executable child tickets with `workspace:<workspace-id>`.
3. In this workflow, scheduling means assigning a ticket to a workspace queue
   with `workspace:<workspace-id>`, exactly one execution mode tag, priority,
   and any real dependency blockers. There is no date-based scheduling unless
   the user explicitly asks for a tag such as `schedule:YYYY-MM-DD`.
4. Break work into thin vertical slices. Prefer small executable tickets over
   broad horizontal tickets.
5. Show the proposed breakdown and wait for approval before creating any
   tickets.
6. If the work naturally splits into multiple tickets, create one detailed
   parent `epic`. If the work is a small direct chain, skip the parent.
7. Child tickets must use `--parent <parent-id>` when a parent exists.
8. Use dependencies only for real blockers. Do not force a linear chain when
   tickets can proceed in parallel.
9. When a parent exists, the parent must depend on every child with
   `tk dep <parent-id> <child-id> --check-cycle`.
10. Tag every executable child ticket with `workspace:<workspace-id>` and
    exactly one execution mode: `mode:afk` or `mode:hitl`.
11. Tag the parent `epic` with `workspace:<workspace-id>` only when all child
    tickets are scheduled for the same workspace. For cross-workspace plans,
    leave the parent project-scoped and document the workspace split in Design.
12. AFK tickets are runnable by the workspace AFK runner once their dependencies
    are resolved. HITL tickets are scheduled for human work in the same
    workspace queue.
13. Prefer `tk create --description --design --acceptance` over raw
    `--template`. The templates below define the content to put into those
    fields.
14. If a ticket must reference a git-ignored file, use its full absolute path,
     not a repo-relative path, because worker worktrees may not contain ignored
     files.
15. Parent tickets default to `mode:hitl` unless the user explicitly says the
     final wrap-up can be done AFK.
16. After creation, summarize ticket IDs, titles, tags, parent links,
    dependencies, and which tickets are scheduled for AFK.

Workflow

1. Detect `owner/repo` from `git remote get-url origin`.
2. Detect `<workspace-id>` from the current repository directory basename.
3. Break the request into slices and present an approval list with:
   Title, `AFK` or `HITL`, workspace, priority, blockers, and a short rationale.
4. After approval, create tickets in dependency order.
5. Add child-to-child dependencies for real blockers only.
6. If a parent exists, add the parent link on every child and add parent
   dependencies on every child.
7. Return a concise ticket map after creation, including the scheduled AFK queue.

Parent Template

Use this template for the parent `epic` when the work spans multiple tickets.

Title

Use a short umbrella outcome, not an implementation phase name.

Type

Use `epic`.

Tags

Use `project:owner/repo` and `mode:hitl` by default. Add
`workspace:<workspace-id>` when all child tickets are scheduled for the same
workspace.

Description

State the overall problem, the desired outcome, and why this group of tickets
exists.

If you mention a git-ignored file here, use its full absolute path.

Design

Capture shared constraints, the planned slice breakdown, workspace scheduling,
and any dependency or ordering rationale that applies across child tickets.

If you mention a git-ignored file here, use its full absolute path.

Acceptance

Use externally verifiable completion criteria such as:

- All required child tickets exist and link back to this parent.
- The dependency graph matches the intended execution order.
- All child tickets are closed.
- Final integration or review for the parent scope is complete.

Child Template

Use this template for executable tickets, with or without a parent.

Title

Use one narrow, executable vertical slice.

Type

Use `task` by default. Use `feature`, `bug`, or `chore` only when clearly
better.

Tags

Use `project:owner/repo`, `workspace:<workspace-id>`, plus exactly one of
`mode:afk` or `mode:hitl`.

Description

State the slice outcome, why it matters, the scheduled workspace, and the parent
ticket ID when relevant.

If you mention a git-ignored file here, use its full absolute path.

Design

Capture important constraints, boundaries, non-goals, and whether the ticket is
safe for AFK execution. Keep this focused.

If you mention a git-ignored file here, use its full absolute path.

Acceptance

Write 2-5 concrete, externally verifiable outcomes. Describe behavior, not
implementation chores. For `mode:afk`, include enough validation guidance for an
unattended worker to know when the ticket is complete.

Creation Pattern

Create a same-workspace parent:

`tk create -t epic --description "..." --design "..." --acceptance "..." --tags "project:owner/repo,workspace:<workspace-id>,mode:hitl" "Parent title"`

Create a cross-workspace parent:

`tk create -t epic --description "..." --design "..." --acceptance "..." --tags "project:owner/repo,mode:hitl" "Parent title"`

Create a child with a parent:

`tk create --parent <parent-id> --description "..." --design "..." --acceptance "..." --tags "project:owner/repo,workspace:<workspace-id>,mode:afk" "Child title"`

Create a HITL child scheduled for the same workspace:

`tk create --parent <parent-id> --description "..." --design "..." --acceptance "..." --tags "project:owner/repo,workspace:<workspace-id>,mode:hitl" "Child title"`

Set priority when scheduling order matters:

`tk create --priority 1 --parent <parent-id> --description "..." --design "..." --acceptance "..." --tags "project:owner/repo,workspace:<workspace-id>,mode:afk" "High-priority child title"`

Add a dependency:

`tk dep <ticket-id> <dependency-id> --check-cycle`

Make the parent wait on a child:

`tk dep <parent-id> <child-id> --check-cycle`

Output Format

When presenting the proposed breakdown before creation, use this shape:

1. `<title>`
   Type: `AFK` or `HITL`
   Workspace: `<workspace-id>`
   Priority: `0`-`4`
   Blocked by: `None` or `<ticket titles>`
   Scheduled: `AFK queue` or `HITL queue`
   Why: `<one short sentence>`

When summarizing created tickets, include the actual ticket IDs and an AFK queue
summary ordered by priority, then dependency readiness.
