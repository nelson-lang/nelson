---
name: nelson-core-code-review
description: Review Nelson code for correctness, quality, performance, maintainability, packaging, tests, help, and repository conventions. Use when reviewing .m files, C++ builtins, module changes, MCP tools, installer changes, or preparing Nelson code for handoff.
---

# Nelson Core Code Review

Systematically review Nelson changes for correctness, regressions, maintainability, test coverage, packaging, and consistency with the local module architecture.

## When To Use

- User asks for review, audit, quality assessment, or cleanup suggestions.
- A change touches Nelson `.m` code, C++ builtins, module packaging, help, tests, or MCP tools.
- Code is ready for handoff and needs a defect-focused pass.
- A generated change needs validation before a maintainer inspects it.
- User reports "code smell", fragile behavior, missing tests, or inconsistent module structure.

## When Not To Use

- User wants a runtime failure debugged first: use `nelson-core-debugging`.
- User asks to write tests rather than assess coverage: use `nelson-core-testing`.
- User asks for packaging installation rules only: use `nelson-core-packaging`.
- User asks for performance measurement and optimization: use `nelson-core-performance`.

## Workflow

1. Identify the changed files and owning module.
2. Run static checks when possible: `lint_nelson_code`, `check_nelson_code`, `check_nelson_format`, or local CLI equivalents.
3. Load local resources when available: `guidelines://nelson-coding`, `guidelines://nelson-testing`, and `guidelines://nelson-modules`.
4. Review behavior first: argument validation, error handling, evaluator state, file paths, cleanup, and policy flags.
5. Review module integration: gateway registration, CMake install data, `module.iss`, tests, help, and metadata APIs.
6. Review maintainability: complexity, duplication, function length, hidden side effects, and naming clarity.
7. Report findings by severity with file and line references. Put defects before style.

## Static Analysis

Use the MCP tools when reviewing through Nelson MCP:

```json
{"name":"lint_nelson_code","arguments":{"path":"modules/ai","format":"text","include_subfolders":true}}
```

```json
{"name":"check_nelson_format","arguments":{"path":"modules/ai"}}
```

For local shell review, use the matching binaries when available:

```sh
bin/macOS/nelson-lint --format text --include-subfolders true modules/<module>
bin/macOS/nelson-format --check modules/<module>
```

Static analysis is the first pass, not the whole review. Always inspect source code after the tool output.

## Review Checklist

### Correctness

| Check | What To Look For | Severity |
| --- | --- | --- |
| Argument validation | Missing required arguments, wrong scalar/vector assumptions, silent defaulting | High |
| Error handling | Exceptions swallowed, unclear messages, success reported after failure | High |
| State changes | Unexpected `cd`, path mutation, evaluator state changes, global state leaks | High |
| File paths | Unsafe relative paths, unquoted paths, paths outside workspace policy | High |
| Cleanup | Temp files, subprocesses, figures, or environment variables left behind | Medium |
| Edge cases | Empty input, path with spaces, non-ASCII text, missing files, disabled policy flags | Medium |

### Module Integration

| Check | Standard | Severity |
| --- | --- | --- |
| Module layout | Public `.m` files in `functions/`, tests in `tests/`, help in `help/`, config in `etc/` | Medium |
| Native builtins | Gateway entry, header, implementation, export target, and tests stay aligned | High |
| Installed data | CMake module data includes new shipped directories | High |
| Windows installer | `module.iss` includes new shipped data when needed | High |
| Help | User-facing behavior has EN and FR help updates when applicable | Medium |
| Tests | New behavior has focused tests in the owning module | High |

### Code Quality

| Check | Standard | Severity |
| --- | --- | --- |
| Function length | Keep functions below repository limits and split only for real clarity | Medium |
| Complexity | Prefer early validation and small helpers over deep nesting | Medium |
| Duplication | Shared policy, path, or protocol logic should have one owner | Medium |
| Naming | Names should describe Nelson behavior, not implementation accidents | Low |
| Comments | Comments explain non-obvious decisions, not line-by-line code | Low |
| Scope | Avoid unrelated refactors and metadata churn | Medium |

### Nelson .m Coding Standards

| Area | Review Rule | Severity |
| --- | --- | --- |
| Names | Prefer precise English identifiers for public APIs and user-facing code | Low |
| Abbreviations | Avoid unclear abbreviations in functions, methods, and public variables | Low |
| Shadowing | Do not name variables or files after existing functions on the Nelson path | High |
| Boolean names | Prefer positive names such as `isReady` or `hasData` over negated names | Low |
| Functions | Public function names should describe an action or returned concept | Low |
| Paired operations | Use complementary names for paired operations, such as create/destroy or start/stop | Low |
| File match | Primary function name should match the file name | High |
| Statements | Avoid multiple statements on one line in committed code | Medium |
| Literal values | Replace repeated magic numbers or strings with named variables | Medium |
| Floating point | Avoid exact equality checks for computed floating point values | High |
| Paths | Use path helpers rather than hardcoded separators when code must be portable | High |
| Globals | Avoid global variables and limit persistent state to justified caches | Medium |
| Structs | Define related struct fields together rather than mutating shape across distant blocks | Medium |
| Loops | Preallocate arrays before loops that fill indexed data | Medium |
| Iterators | Do not modify a loop iterator inside the loop body | High |
| Switch | Include a default branch or explain why no other case can occur | Medium |
| Functions with no inputs | Use an explicit call form when ambiguity with a variable is possible | Low |
| Dynamic code | Avoid `eval` and workspace mutation helpers except for narrowly justified tests | High |
| State changes | Restore current folder, path, environment, and warning state before exit | High |
| Formatting | Keep lines within the project limit and avoid trailing whitespace | Low |
| Comments | Put comments before the code they explain and explain why, not what | Low |
| Public help | Public functions should have concise user-facing help or XML help coverage | Medium |

### Function Interface Standards

| Check | Standard | Severity |
| --- | --- | --- |
| Inputs | Flag public functions with more than 6 positional inputs | Medium |
| Outputs | Flag public functions with more than 4 outputs | Medium |
| Optional arguments | Prefer explicit option parsing over fragile positional conventions | Medium |
| Shape behavior | Element-wise functions should preserve input shape where users expect it | High |
| Output meaning | Output meaning should not change based on `nargout` | High |
| Local functions | Keep local functions simple; move independently tested logic to its own file | Medium |
| Nested functions | Avoid nested functions unless closure behavior is essential | Medium |

### MCP-Specific Checks

| Check | What To Verify | Severity |
| --- | --- | --- |
| JSON-RPC shape | Responses include valid `jsonrpc`, `id`, `result` or `error` | High |
| Resource consistency | `resources/list`, `resources/read`, and `mcpinfo()` expose the same resource set | High |
| Tool policy | Execution and formatting policy flags are enforced before side effects | High |
| Stdio stability | Server starts with stable flags and returns one JSON response per line | High |
| Output limits | Large text is truncated with explicit flags | Medium |
| Workspace policy | Tools reject paths outside workspace root | High |

## High-Severity Flags

Always report these as defects:

- Code execution or formatting can run when policy disables it.
- A path with spaces is split incorrectly.
- A tool can read, write, or format outside the workspace root.
- `resources/list`, `resources/read`, and `mcpinfo()` disagree.
- New installed data is missing from CMake install rules or `module.iss`.
- A test can hang without an outer timeout.
- A subprocess command builds unquoted user-controlled input.
- A builtin changes evaluator quit or error state unexpectedly.
- Code uses dynamic evaluation on untrusted text.
- Public function output changes meaning when callers request fewer or more outputs.

## What Static Tools Miss

Static tools may not catch:

- Existing-function shadowing by local variable names.
- Exact floating-point equality in numerical code.
- Hardcoded platform separators in paths.
- Magic literals that should be named.
- Missing public help for a user-facing function.
- Protocol metadata drift between lists, info structs, and help.
- Startup script interference in command-line or stdio tests.
- Missing installer entries for non-code data.
- Temp directory leaks after assertion failures.
- Race conditions from tests that should be sequential.
- Behavior differences between `cli`, `adv-cli`, and graphical modes.
- Cross-platform path quoting and separator issues.

Do not stop after a clean lint result. Review packaging, tests, help, and runtime metadata manually.

## Review Patterns

### Naming And Shadowing Scan

For public `.m` files:

```text
file name matches primary function
function name describes an action or concept
variable names are descriptive in public-facing blocks
no variable shadows an existing Nelson function used nearby
boolean names are positive where practical
paired operations use paired names
```

### Function Interface Scan

For public functions:

```text
input count is modest or options are grouped
output count is modest and each output has a stable meaning
input validation fails early
state changes are restored by cleanup
shape behavior is explicit for arrays
help or XML docs describe syntax and side effects
```

### Resource Consistency

For any new MCP resource, check all places together:

```text
resources/list contains the URI
resources/read returns content for the URI
mcpinfo().resources contains the URI
tests assert discovery and read behavior
installed data rules include the backing file
```

### Stdio Smoke Test

Use one JSON-RPC request per line and deterministic startup flags:

```sh
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' \
  | bin/macOS/nelson-adv-cli --quiet --no-ipc --no-user-startup --no-user-modules --execute="mcpserver"
```

### Review Report Format

Return findings first:

```text
Findings
- [High] modules/ai/src/cpp/AiMcpServer.cpp:123: Path option parsing splits quoted workspace roots, so MCP tools reject valid folders with spaces.
- [Medium] modules/ai/module.iss:21: New shipped data is not included in the Windows installer, so installed MCP resources are unreadable.

Open Questions
- Is this resource intended to ship with minimal installations or only developer builds?

Validation
- make nlsAi nlsAi_builtin
- test_run('ai')
- bin/macOS/nelson-format --check modules/ai
```

If there are no findings, say that clearly and list residual risk or unrun checks.

## Conventions

- Review is read-only assessment unless the user asks to fix findings.
- Lead with bugs, regressions, missing tests, and packaging breaks.
- Do not bury defects behind a long summary.
- Prefer exact file and line references.
- Separate blocking defects from suggestions.
- Do not request broad refactors when a scoped fix is enough.
- Mention checks that could not be run.
