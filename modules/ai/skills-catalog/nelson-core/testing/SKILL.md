---
name: nelson-core-testing
description: Design, write, stabilize, and run Nelson tests for modules, builtins, CLI behavior, MCP tools, packaging, and regression fixes. Use when adding coverage, reproducing a bug, preventing hangs, or validating changed Nelson behavior.
---

# Nelson Core Testing

Create focused Nelson tests that prove behavior without depending on user machine state or unrelated modules.

## When To Use

- User asks to add tests, reproduce a bug, or validate a fix.
- A change touches public functions, C++ builtins, CLI startup, MCP tools, help metadata, or packaging.
- A previous test hangs, leaks temp files, or behaves differently across CLI modes.
- A new edge case needs regression coverage.

## When Not To Use

- User asks for a read-only code review: use `nelson-core-code-review`.
- User asks for root-cause debugging before writing tests: use `nelson-core-debugging`.
- User asks only for installer rules: use `nelson-core-packaging`.

## Workflow

1. Locate nearby tests in `modules/<module>/tests/` and match their style.
2. Write the smallest test that proves one behavior.
3. Add early `onCleanup` for temp files, environment variables, figures, and path changes.
4. Use deterministic command-line flags for subprocess tests.
5. Mark tests with the local sequential marker when they share process state or spawn nested Nelson processes.
6. Run the specific test first, then `test_run('<module>')`.
7. Keep test data under `temp/` or the module test fixture tree.

## Test Structure

| Test Type | Location | Pattern |
| --- | --- | --- |
| Script function behavior | `modules/<module>/tests/test_<name>.m` | Direct assertions with small inputs |
| Builtin behavior | Owning module tests | Check `nargin`, `nargout`, success, and error paths |
| MCP stdio | `modules/ai/tests` | Use `ai_mcp_request` or one JSON-RPC line per request |
| CLI behavior | Module or engine tests | Use outer timeout and stable startup flags |
| Packaging | Existing packaging integration test | Assert CMake or installer text includes new files |
| Graphics | Graphics module tests | Use advanced or graphical mode and explicit cleanup |

## Assertions

Prefer direct assertions:

```matlab
asserts.isequal(actual, expected);
asserts.istrue(contains(output, 'expected text'));
asserts.isfalse(isempty(result));
```

For structs or cells, assert both shape and key fields. Avoid brittle full-output comparisons when order or timing is not part of the behavior.

## Subprocess Tests

Use stable startup flags unless the test is explicitly about startup scripts:

```matlab
cmd = [nelson_exe, ' --quiet --no-ipc --no-user-startup --no-user-modules --timeout 30 --execute="quit"'];
status = system(cmd, 40);
asserts.isequal(status, 0);
```

For MCP requests, use `ai_mcp_request` so request and output files are cleaned consistently.

## Sequential Tests

Add the local marker when tests:

- Spawn nested Nelson processes.
- Mutate environment variables.
- Depend on a shared port, display mode, path cache, or current folder.
- Exercise process timeout or stdio behavior.

Do not run sequential MCP tests in parallel manually.

## Cleanup Checklist

| Resource | Cleanup |
| --- | --- |
| Temp file | `onCleanup(@() rmfile(path))` |
| Temp folder | `onCleanup(@() rmdir(path, 's'))` after folder creation |
| Environment variable | Save old value and restore with `onCleanup` |
| Current folder | Save `pwd()` and restore with `onCleanup` |
| Figure | Close or use existing graphics cleanup pattern |
| Path change | Restore path or remove added folder |

## Edge Cases To Cover

- Empty input and missing required input.
- Relative and absolute paths.
- Paths containing spaces.
- Disabled policy flags.
- Missing files or directories.
- Non-default display mode when relevant.
- Output truncation when limits are part of behavior.

## Done When

- The targeted test passes.
- `test_run('<module>')` passes for the owning module.
- The test fails for the original bug when practical.
- Cleanup still runs on assertion failure.
- The test does not require user-specific folders, installed credentials, or network access.
