---
name: nelson-core-packaging
description: Add and review Nelson installed files, module data directories, help archives, tests, Windows installer entries, CMake install rules, and packaging integration tests. Use when files must ship with Nelson.
---

# Nelson Core Packaging

Make sure files added to a module are available from source builds, installed builds, and platform installers.

## When To Use

- A change adds a directory under `modules/<module>/`.
- New runtime data, catalog files, examples, fixtures, or resources must ship.
- Help, tests, or functions need installer coverage.
- A module packaging test exists and should be updated.
- User asks whether install or `.iss` files need changes.

## When Not To Use

- User is only changing executable logic: use module, native builtin, or testing skills.
- User asks for quality review across many dimensions: use `nelson-core-code-review`.

## Workflow

1. Identify whether the file is runtime data, test data, help, function code, or source-only material.
2. Check module-level `CMakeLists.txt`.
3. Check `module.iss` for Windows installer inclusion.
4. Check existing packaging integration tests.
5. Run the targeted packaging test.
6. Run the owning module tests if packaging affects runtime behavior.

## File Type Matrix

| File Type | CMake Install Rule | Installer Rule | Test Needed |
| --- | --- | --- | --- |
| Public `.m` function | Usually automatic from `functions/` | `functions\*.m` | Function behavior test |
| Help archive | Usually `help/*.nhz` | `help\*.nhz` | Help build or packaging check |
| Unit test | Usually `tests/*.m` | `tests\*.m` | Existing module test |
| Runtime data directory | Add `EXTRA_DIRS` or pattern | Add recursive `Source` line | Packaging integration assertion |
| Fixture file | Add explicit pattern if needed | Add explicit pattern if shipped | Test that consumes fixture |

## CMake Pattern

For a whole data directory owned by a module:

```cmake
nelson_install_module_data(${module_name} EXTRA_DIRS skills-catalog)
```

Use pattern-based installs when only selected extensions should ship.

## Installer Pattern

For a recursive module data directory:

```text
Source: {#RootPath}modules\{#MODULE_NAME}\skills-catalog\*; DestDir: {app}\modules\{#MODULE_NAME}\skills-catalog\; Flags: recursesubdirs;Components: {#COMPONENT_AI};
```

Keep component ownership consistent with the module component.

## Review Checklist

- New data exists under the module, not a global folder, unless it is shared infrastructure.
- CMake and installer destination paths match runtime lookup paths.
- Recursive install does not accidentally ship build artifacts.
- Tests assert packaging text when the module already has packaging integration coverage.
- Help and tests remain controlled by their existing optional components.
- Root build files are unchanged unless module-level rules cannot express the change.

## Done When

- Source tree lookup works.
- Installed data rule includes the file or directory.
- Windows installer rule includes the file or directory when applicable.
- Packaging integration test passes.
- `test_run('<module>')` passes if runtime behavior depends on the packaged data.
