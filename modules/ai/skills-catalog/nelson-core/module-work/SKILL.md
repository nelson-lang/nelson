---
name: nelson-core-module-work
description: Change Nelson modules safely, including functions, native code, loaders, help, tests, installed data, and module boundaries. Use when adding features, fixing module behavior, moving files, or updating module packaging.
---

# Nelson Core Module Work

Work inside the owning module and preserve Nelson's module structure, install rules, tests, and help.

## When To Use

- Adding or modifying a public Nelson function.
- Adding a C++ builtin or helper inside a module.
- Changing `loader.m`, `etc/startup.m`, `etc/finish.m`, or module startup behavior.
- Adding module-owned runtime data.
- Updating module tests, help, or packaging for changed behavior.

## When Not To Use

- Native builtin internals are the main concern: use `nelson-core-native-builtins`.
- The task is only review: use `nelson-core-code-review`.
- The task is only test design: use `nelson-core-testing`.
- The task only concerns installed files: use `nelson-core-packaging`.

## Module Layout

| Path | Purpose | Review Point |
| --- | --- | --- |
| `src/` | Native module implementation | Keep C++ helpers scoped and formatted |
| `builtin/` | Gateway registration and builtin wrappers | Match names, argument counts, and exports |
| `functions/` | Public or private Nelson `.m` functions | Match naming and help expectations |
| `tests/` | Unit and integration tests | Add focused coverage for behavior changes |
| `help/` | User-facing help | Update both language trees when behavior changes |
| `etc/` | Startup and finish hooks | Keep side effects explicit and reversible |
| `module.iss` | Windows installer entries | Include new shipped data |
| `CMakeLists.txt` | Build and install rules | Avoid root build edits when module rules suffice |

## Workflow

1. Identify the owning module and read nearby implementations.
2. Decide whether the change belongs in native code, `.m` code, help, tests, or data.
3. Keep the public surface minimal and predictable.
4. Add tests before or with the behavior change.
5. Update help and packaging when user-visible behavior or shipped files change.
6. Run the smallest useful test, then the module test suite.
7. Run format checks for touched code.

## Design Checklist

- Reuse existing module helpers before creating new abstractions.
- Avoid expanding the core for feature-specific behavior that belongs in a module.
- Keep parsing, computation, and presentation separate.
- Prefer explicit errors over silent fallback.
- Keep file paths and workspace policies in one place.
- Avoid changing public behavior without tests and help.
- Avoid refactoring neighboring modules unless required by the task.

## Packaging Checklist

For every new file or directory:

```text
Does runtime code need it after installation?
Is it covered by CMake module data installation?
Is it covered by module.iss for Windows packaging?
Does a packaging integration test assert the rule?
```

## Done When

- The owning module remains self-contained.
- Tests cover the changed behavior.
- Help and packaging are updated when needed.
- Format and build checks pass.
- No unrelated module behavior changed.
