---
name: nelson-core-help-authoring
description: Write and review Nelson XML help, examples, syntax sections, See Also entries, bilingual help updates, MCP usage docs, and user-facing documentation for changed behavior.
---

# Nelson Core Help Authoring

Keep help aligned with the actual public behavior and examples that users can run.

## When To Use

- Public function syntax, defaults, options, or errors change.
- A new function, builtin, MCP tool, resource, or prompt becomes user-facing.
- Existing help is stale, incomplete, or misleading.
- Examples need to reflect stable CLI or MCP startup.
- EN and FR help files need synchronized concepts.

## When Not To Use

- The task is only internal implementation without user-facing behavior.
- The user wants a code review: use `nelson-core-code-review`.
- The user wants packaging coverage only: use `nelson-core-packaging`.

## Workflow

1. Read the implementation and tests before editing help.
2. Identify user-facing syntax, defaults, outputs, side effects, and failure modes.
3. Update the relevant help files in both language trees.
4. Keep examples short and runnable.
5. Update See Also entries only when referenced symbols exist.
6. Run module tests and format checks.

## Help Checklist

| Section | Review Rule |
| --- | --- |
| Short description | One concise user-facing sentence |
| Syntax | Include common calls and option forms |
| Description | Explain behavior, defaults, and side effects |
| Examples | Prefer minimal runnable examples |
| Notes | Document policy flags, modes, or platform details |
| See Also | Link only existing functions or pages |
| Translation | Keep concepts aligned across language trees |

## Example Quality

- Use stable startup flags for command-line examples.
- Quote paths that may contain spaces.
- Avoid machine-specific absolute paths unless clearly illustrative.
- Avoid examples that require network access.
- Keep graphical examples explicit about display mode.
- Prefer small inputs and visible expected behavior.

## MCP Help Rules

For MCP server documentation:

- Distinguish the agent client from the local model provider.
- Show stdio startup as one command.
- Include policy options when they affect tool availability.
- Document workspace root and output limit defaults.
- Mention resources and prompts when they are part of discovery.

## Done When

- Help matches implementation and tests.
- EN and FR files describe the same behavior.
- Examples use stable commands.
- New public behavior is discoverable from help.
- No forbidden project terminology is introduced.
