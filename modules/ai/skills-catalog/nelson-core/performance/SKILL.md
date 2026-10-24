---
name: nelson-core-performance
description: Analyze and improve Nelson runtime performance, memory use, array growth, repeated filesystem scans, process startup cost, tool output size, native loops, and MCP response payloads.
---

# Nelson Core Performance

Improve performance by measuring the expensive path and reducing unnecessary work without changing semantics.

## When To Use

- User reports slow code, large memory use, or sluggish tests.
- A tool returns huge output or repeatedly scans broad folders.
- Native code performs expensive loops or serialization.
- A test suite is slow because of process startup or repeated setup.
- An MCP response needs output limits or structured summaries.

## When Not To Use

- The task is only correctness debugging: use `nelson-core-debugging`.
- The task is only review: use `nelson-core-code-review`.
- Performance is speculative and there is no slow path to inspect.

## Workflow

1. Identify the user-visible slow operation.
2. Measure or bound the cost before editing when practical.
3. Classify the cost: interpreter execution, native loop, filesystem traversal, process startup, graphics, or serialization.
4. Reduce repeated work before adding caching.
5. Preserve output shape and error behavior.
6. Add tests for the behavior most likely to regress.

## Common Issues

| Issue | Review Direction |
| --- | --- |
| Growing arrays in loops | Preallocate or collect pieces and concatenate once |
| Repeated recursive scans | Narrow the search root or reuse discovered module data |
| Large protocol text | Return structured summaries and truncation flags |
| Slow subprocess tests | Use stable startup flags and avoid duplicate process launches |
| Expensive path canonicalization | Canonicalize once per request boundary |
| Duplicate serialization | Build JSON or structs through one helper path |
| Unbounded output capture | Apply output limits before response construction |

## MCP Performance

For MCP tools:

- Keep `structuredContent` concise.
- Include file paths or summaries instead of full large data when possible.
- Respect `maxOutputCharacters`.
- Mark truncation explicitly.
- Avoid scanning every module when a symbol or module argument narrows the task.

## Native Performance

- Prefer simple standard library algorithms.
- Avoid repeated string conversion inside tight loops.
- Reserve containers when approximate size is known.
- Keep filesystem traversal filters early.
- Do not add global caches without invalidation.

## Done When

- The slow path is identified.
- The improvement is measurable or logically bounded.
- Tests preserve behavior.
- Output remains stable for callers.
- No broad cache or global state is added without need.
