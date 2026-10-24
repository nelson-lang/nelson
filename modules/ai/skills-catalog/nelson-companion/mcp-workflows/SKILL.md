---
name: nelson-companion-mcp-workflows
description: Connect MCP clients to Nelson, validate stdio transport, inspect tools/resources/prompts, call Nelson MCP tools, debug JSON-RPC responses, and configure stable server startup.
---

# Nelson MCP Workflows

Use Nelson as an MCP stdio server with deterministic startup, discoverable tools, read-only resources, reusable prompts, and explicit execution policy.

## When To Use

- User needs to connect an MCP client to Nelson.
- An agent should call Nelson tools through MCP.
- Stdio startup, JSON-RPC shape, tools, resources, or prompts need validation.
- MCP policy flags, workspace root, or output limits need testing.
- A new MCP tool, resource, or prompt is added.

## When Not To Use

- User asks only for pure Nelson code without agent tooling.
- User wants a general code review: use `nelson-core-code-review`.
- User wants to debug a crash before protocol validation: use `nelson-core-debugging`.

## Stable Startup

Use this command shape for deterministic stdio:

```sh
bin/macOS/nelson-adv-cli --quiet --no-ipc --no-user-startup --no-user-modules --execute="mcpserver"
```

For client configuration, include the same startup flags. Avoid user startup scripts unless testing startup behavior.

## Request Order

1. Send `initialize`.
2. Send `initialized` notification if the client expects it.
3. Call `tools/list`, `resources/list`, and `prompts/list`.
4. Read relevant resources before calling mutating tools.
5. Call tools with explicit arguments.

Each JSON-RPC request is one line on stdin.

## Smoke Tests

Initialize:

```sh
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' \
  | bin/macOS/nelson-adv-cli --quiet --no-ipc --no-user-startup --no-user-modules --execute="mcpserver"
```

Read a skill resource:

```sh
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"skills://nelson-core/testing"}}' \
  | bin/macOS/nelson-adv-cli --quiet --no-ipc --no-user-startup --no-user-modules --execute="mcpserver"
```

Call a read-only tool:

```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"get_nelson_workspace","arguments":{}}}
```

## Options

| Option | Purpose | Review Point |
| --- | --- | --- |
| `--workspace-root=PATH` | Restrict tool paths | Quote values containing spaces |
| `--nelson-display-mode=adv-cli` | Select display mode | Use graphical mode only when needed |
| `--max-output-characters=N` | Cap output text | Check truncation flags |
| `--allow-execution=false` | Disable execution tools | Enforce before side effects |
| `--allow-format=false` | Disable formatting tools | Enforce before file writes |
| `--initial-working-folder=PATH` | Set startup folder | Keep inside expected workspace |

## Resource Consistency

For every resource:

- Add it to `resources/list`.
- Implement it in `resources/read`.
- Add it to `mcpinfo().resources`.
- Add tests for discovery and at least one read.
- Include backing data in CMake install rules and `module.iss` when it ships.

## Failure Patterns

| Symptom | Likely Cause | Fix Direction |
| --- | --- | --- |
| No output | User startup or user modules interfered | Add stable startup flags |
| Invalid JSON | Extra banner or logging on stdout | Keep server quiet and write diagnostics elsewhere |
| Unknown resource | Metadata drift or missing file | Check list/read/info paths |
| Path outside workspace | Workspace root mismatch | Inspect canonical path and options parsing |
| Execution runs when disabled | Policy check too late | Move policy check before tool side effects |

## Done When

- `initialize` returns server info.
- `tools/list`, `resources/list`, and `prompts/list` return valid JSON.
- A representative `resources/read` succeeds.
- A representative read-only `tools/call` succeeds.
- Policy-disabled execution and formatting are tested when touched.
