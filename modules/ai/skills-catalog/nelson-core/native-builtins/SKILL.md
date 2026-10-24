---
name: nelson-core-native-builtins
description: Implement, review, and test Nelson C++ builtins, gateway entries, argument validation, evaluator interactions, native helper code, JSON protocol responses, and structured native outputs.
---

# Nelson Core Native Builtins

Keep native builtins small, explicit, testable, and aligned with module gateway registration.

## When To Use

- Adding or modifying a C++ builtin.
- Changing `Gateway.cpp` registrations or builtin headers.
- Returning structs, cells, strings, JSON protocol data, or evaluator output from native code.
- Debugging native argument conversion or evaluator side effects.
- Enforcing policy before native side effects.

## When Not To Use

- The implementation is pure `.m` code: use `nelson-core-module-work`.
- The task is only review: use `nelson-core-code-review`.
- The task is only MCP connection behavior: use `nelson-companion-mcp-workflows`.

## Workflow

1. Read the module's existing builtin wrappers and gateway style.
2. Validate `nLhs`, input count, and input types at the builtin boundary.
3. Convert inputs once and keep converted values local.
4. Run policy checks before evaluation, file access, or formatting.
5. Keep protocol serialization separate from computation.
6. Return typed Nelson values when possible; use JSON text only for protocol payloads.
7. Add tests for success, invalid input, and one edge case.
8. Rebuild native targets and run module tests.

## Gateway Checklist

| Check | Standard |
| --- | --- |
| Name | Gateway name matches public function name |
| Header | Builtin declaration is present in `builtin/include` |
| Source | Builtin implementation is present in `builtin/cpp` |
| Arity | Registered `nargout` and `nargin` match behavior |
| Library link | Builtin target links the module library when needed |
| Tests | `nargin`, `nargout`, and behavior are tested |

## Argument Validation

- Reject unsupported input shapes early.
- Distinguish character arrays and string scalars deliberately.
- Use defaults only when they are documented.
- Return clear errors for missing required inputs.
- Avoid partial side effects before validation completes.

## Evaluator State

When using `Evaluator*`:

- Reset or preserve state intentionally.
- Avoid leaving quit, abort, or error state after a recoverable protocol error.
- Capture output explicitly when the caller expects text.
- Do not call evaluation helpers after policy denies execution.
- Keep current folder changes protected by cleanup logic.

## C++ Quality

- Keep functions below repository limits.
- Prefer local helpers over broad classes.
- Avoid duplicate path, quoting, and truncation logic.
- Format with the repository C++ style.
- Use standard library parsers instead of ad hoc parsing when practical.
- If ad hoc parsing is needed, test quotes, escapes, and spaces.

## Done When

- `make <module-target> <module-builtin-target>` passes.
- `clang-format --dry-run -Werror` passes for touched C++.
- Module tests cover the public behavior.
- Invalid input and policy-denied behavior are covered.
