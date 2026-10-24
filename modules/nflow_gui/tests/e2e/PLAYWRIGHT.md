# Playwright E2E - Run instructions & coverage

This document explains how to run the Playwright E2E tests for the Web UI and summarizes what is currently covered by the suite (and what still needs work).

**Prerequisites**
- **Node.js**: Install a Node.js version compatible with the project (use the project's recommended Node version).
- **Dependencies**: from the repository root run `npm --prefix modules/nflow_gui ci` or `cd modules/nflow_gui && npm ci`.
- **Playwright browsers**: install with `npx playwright install` or run the provided install script (`npm --prefix modules/nflow_gui run test:e2e:install`).

**Environment**
- **NFLOW_URL**: tests expect the web UI to be reachable. Set `NFLOW_URL` to the running UI address. Recommended value used in local runs:

  - PowerShell (temporary for the command - set then run):
    ```powershell
    # set env vars for this PowerShell session, then run Playwright
    $env:NFLOW_GUI_DEBUGGING_PORT = '1973'
    $env:NFLOW_URL = 'http://localhost:1973/nflow.html'
    npx playwright test ...
    ```

  - Cross-platform (use `cross-env` in scripts):
    ```bash
    npx cross-env NFLOW_GUI_DEBUGGING_PORT=1973 NFLOW_URL=http://localhost:1973/nflow.html npx playwright test ...
    ```

The test `global-setup` will try `NFLOW_URL` and, if unreachable, will fall back to starting the built static server (`static-server.mjs`) so tests can run hostless.

**Quick commands**
- Build assets (if you changed code):
  ```bash
  cd modules/nflow_gui
  npm run build
  ```

- Run the full suite (example with 8 workers and traces):
  ```bash
  npx cross-env NFLOW_URL=http://localhost:1973/nflow.html npx playwright test --config tests/e2e/playwright.config.ts --project=chromium --workers=8 --reporter=list --trace=on
  ```

- Run with a single worker (serial):
  ```bash
  npx cross-env NFLOW_URL=http://localhost:1973/nflow.html npx playwright test --config tests/e2e/playwright.config.ts --project=chromium --workers=1 --reporter=list --trace=on
  ```

- Run a single spec file:
  ```bash
  npx playwright test tests/e2e/tests/simulation-panel.spec.ts --project=chromium --workers=1 --trace=on
  ```

- Run a specific test by name (grep):
  ```bash
  npx playwright test -g "Simulation panel › play / pause toggles"
  ```

**Traces and artifacts**
- When using `--trace=on`, Playwright saves trace files (trace.zip) and screenshots to `modules/nflow_gui/tests/e2e/test-results/` in per-run folders. Use `npx playwright show-trace <trace.zip>` to inspect a trace.

**Global setup & fixtures**
- `tests/e2e/playwright.config.ts` includes a `global-setup` step that checks `NFLOW_URL`. If the URL is unreachable the test setup will start `static-server.mjs` (hostless fallback) to serve built assets.
- Tests inject a deterministic fixture `webui-mock` (see `tests/e2e/fixtures/webui-mock.ts`) that:
  - Mocks file open/save dialogs (returns JSON-shaped responses expected by the app).
  - Auto-dismisses confirm-save modal to avoid blocking tests.
  - Provides a small simulation shim (emits `nf-sim-start`/`nf-sim-state`/`nf-sim-stop`) for hostless runs.

**Page Objects / helpers**
- Page objects live in [`pages/`](pages/): `AppPage.ts`, `LibraryPanel.ts`, `SimulationPanel.ts`, `SimulationToolbar.ts`, `InspectorPanel.ts`, `ContextMenu.ts`, `CanvasEditor.ts`, `DiagnosticsPanel.ts`.
- `CanvasEditor.ts`: block-modeling gestures a user makes - `insertBlock()` (quick-insert popup), `connect()` / `clickPort()` (port click-to-connect), `wireCount()`, `blockBoxes()`, `autoArrange()`. Plus `boxesOverlap()` for layout assertions.
- `DiagnosticsPanel.ts`: the bottom Diagnostics panel and its status-bar counters - `open()`, `errorCount()`, `waitForErrorCount()`, `items()`.

**What the suite currently covers (high level)**
- **File operations**: `New`, `Open` (mocked), `Save`, `Save As`, `Export SVG`.
- **Editor & canvas**: SVG canvas present, blocks and wires render, selection, delete, undo/redo, block creation via library drag-and-drop.
- **Library**: library groups render, first group auto-opened for tests, block dragging to canvas.
- **Simulation**: simulation panel and floating toolbar; play/pause behavior and UI `data-action` toggles are asserted; sim shim used for deterministic runs.
- **Keyboard shortcuts**: `Ctrl+N`, `Ctrl+O`, `Ctrl+Z`, `Ctrl+Y`, `Delete` and other accelerators are handled and tested.
- **Fixtures**: a broad set of diagram fixtures are exercised (math, logic, continuous, discrete, sources, sinks, etc.) - these are loaded and rendered as part of many tests.
- **POMs & helpers**: tests follow the Page Object pattern and use `AppPage.waitForReady()` and other helpers to reduce flakiness.
- **Modeling-workflow usability (nflow-specific)**:
  - `wiring-invalid.spec.ts` (N1): invalid-wiring feedback - output->output refused, occupied-input refused, same-block cancel, compatible-target highlight, live preview.
  - `wire-editing.spec.ts` (N1b): remove a wire (select + Delete), undo restores it, deleting a block takes its wire, and a freed input can be reconnected.
  - `inspector-params.spec.ts` (N3b): a valid parameter edit round-trips and a nonsensical entry does not break the editor (it recovers).
  - `subgraph-edit.spec.ts`: copying a wired selection pastes the internal wire too, and the align-left context-menu command lines up the selected blocks' origins.
  - `wire-extras.spec.ts`: double-clicking a wire names it (wire-labels), and the click-to-connect flow works across block types (gain->gain, sum->scope, etc.). Manual wire-corner dragging is documented in the file as not headless-triggerable.
  - `quick-insert-ranking.spec.ts` also covers keyboard navigation (ArrowDown moves the highlight, Enter inserts the highlighted block).
  - `quick-insert-ranking.spec.ts` (N5): quick-insert ranks the obvious block first, case-insensitive, sub-string, Enter inserts, unknown query is empty.
  - `auto-arrange.spec.ts` (N3): Edit > Auto-arrange yields no overlaps, left-to-right flow, preserved wires, idempotent layout; a separate case covers the acausal schematic-arrange path.
  - `model-diagnostics.spec.ts` (N2, host-less half): an error reaches the Diagnostics panel - counter, tab badge, readable row, severity filters, clear; the intentionally-broken fixtures still load; the nf-select-block navigation wiring selects a block, and a blockId-less diagnostic is not clickable.
  - `acausal-editor.spec.ts` (N4): an acausal circuit renders physical pins (with a domain), undirected physical wires, and component parameters.
- **Invalid fixtures**: intentionally-broken models live in [tests/invalid](../invalid) (`algebraic_loop.nflow`, `unconnected_input.nflow`); the engine's reaction to them is asserted by the b2b suite (`tests/b2b/tests/model-diagnostics.spec.ts`).

**What is NOT covered / TODO**
- Export to raster formats (PNG) is not exercised by the E2E suite - only `export-svg` is covered.
- Full coverage for accessibility (WCAG) beyond basic ARIA checks is missing (axe-core not yet wired).
- Interactive acausal wiring (dragging node-to-node between physical pins) is not driven yet - N4 covers acausal *rendering*; acausal *simulation* is covered by the b2b examples walk-through.
- On-canvas highlighting of the offending block (`.loop-highlight` via `nf-error-blocks`) is not asserted: the engine surfaces algebraic-loop / unconnected-input errors but does not return per-block `errorBlocks` for them, so no block is highlighted; that path fires for other error classes (e.g. `unsupported_block`). The diagnostic-to-block navigation *wiring* is covered host-less instead.
- Performance benchmarks and long-running stability tests are not implemented (some smoke checks exist).
- Native OS dialogs and multi-user/collaborative flows are not covered - tests rely on deterministic in-page mocks.

**Best practices for adding tests**
- Use existing Page Objects in `tests/e2e/pages` to interact with the app and keep selectors centralized.
- Avoid OS dialogs; extend `webui-mock` instead of relying on native dialogs.
- Prefer `await page.waitForSelector(...)` or POM helper waits (e.g. `AppPage.waitForReady()`) over arbitrary `waitForTimeout` delays.
- When producing per-worker temporary files, include `test.info().workerIndex` in filenames to avoid collisions.

**CI notes**
- Ensure Playwright browsers are installed in CI (`npx playwright install` or `npm run test:e2e:install`).
- Use `npx cross-env NFLOW_URL=...` in CI job steps to set `NFLOW_URL` where needed.
- Start with `--workers=2` in CI to detect shared-state issues, then scale up if stable.

**Debugging tips**
- Use `--trace=on` and `--reporter=list` to capture traces and concise output.
- Look in `modules/nflow_gui/tests/e2e/test-results/` for per-run traces/screenshots.

**Next steps**
- Expand `webui-mock` fixtures and add more assertions to the new scenario specs under `tests/e2e/tests`.
- Add CI steps (install browsers, run `npm run build`, run tests with `--workers`) and report results.
