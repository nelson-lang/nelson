---
name: nelson-core-debugging
description: Debug Nelson failures, crashes, hangs, incorrect results, test regressions, CLI startup issues, and MCP stdio problems. Use when behavior is broken and the root cause is not yet known.
---

# Nelson Core Debugging

Find the smallest reproducible failure, separate runtime startup effects from the target behavior, and leave a regression test behind.

## When To Use

- A test fails, aborts, hangs, or returns no output.
- A function returns incorrect values or changes state unexpectedly.
- A CLI command behaves differently with user modules loaded.
- An MCP request does not return valid JSON-RPC.
- A native builtin crashes or leaves the evaluator in the wrong state.

## When Not To Use

- User only wants a review: use `nelson-core-code-review`.
- Root cause is known and only tests are needed: use `nelson-core-testing`.
- Task is only about installed files or installer scripts: use `nelson-core-packaging`.

## Workflow

1. Reproduce the failure with the exact command or test.
2. Reduce to the smallest command that still fails.
3. Add stable startup flags to rule out startup scripts and user modules.
4. Compare CLI modes only when display or terminal mode matters.
5. Inspect the owning module and nearest tests before editing.
6. Add logging or assertions locally, then remove diagnostic noise.
7. Fix the root cause, not only the symptom.
8. Add or update a focused regression test.

## Reproduction Commands

For module tests:

```sh
bin/macOS/nelson-adv-cli --quiet --no-ipc --no-user-modules --timeout 120 --execute="test_run('ai'); quit"
```

For one test file:

```sh
bin/macOS/nelson-adv-cli --quiet --no-ipc --no-user-modules --timeout 60 --execute="run('modules/ai/tests/test_mcpinfo.m'); quit"
```

For stdio:

```sh
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"ping","params":{}}' \
  | bin/macOS/nelson-adv-cli --quiet --no-ipc --no-user-startup --no-user-modules --execute="mcpserver"
```

## Failure Patterns

| Symptom | Likely Area | First Checks |
| --- | --- | --- |
| No CLI output | Startup scripts or user modules | Add `--no-user-startup --no-user-modules` |
| Stdio hangs | Nested process, missing EOF, no timeout | Use one request per line and outer timeout |
| Abort trap | Native code or process timeout | Isolate one test, check C++ recent edits |
| Empty MCP output | Command quoting or startup interference | Print command and test direct shell form |
| Path rejected | Workspace policy or path parsing | Test absolute path and path with spaces |
| Resource missing | Metadata drift or install data missing | Check list/read/info and packaging |

## Native Debugging Checklist

- Validate builtin argument counts and types before conversion.
- Check `Evaluator` state after failures and after server loops.
- Confirm exceptions return JSON-RPC errors where protocol requires them.
- Keep side effects after policy checks.
- Rebuild the target after C++ edits before retesting.

## Script Debugging Checklist

- Check `nargin` defaults and shadowed input variables.
- Verify cleanup handles early failure.
- Avoid broad `try/catch` blocks that hide diagnostics.
- Confirm strings passed to `system` quote spaces and special characters.
- Avoid running sequential tests in parallel.

## Done When

- The root cause is named in plain language.
- The fix is scoped to the failing behavior.
- A regression test covers the failure mode.
- The final verification uses the mode that originally failed.
- No diagnostic-only code remains.
