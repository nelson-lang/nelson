//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Gallery walk-through: every shipped nflow example model is loaded into the
// real UI and simulated through the real Run control.
//
// One test is generated per `.nflow` example found on disk (the files the
// Example Browser lists as `kind === 'model'`), so a broken demo is reported by
// name. For each model the test asserts that:
//   1. the model parses and renders (blocks appear, no load-time error),
//   2. the Run control starts the engine (data-action -> pause),
//   3. no simulation error is logged to the console, and
//   4. the run makes progress and, for finite models, reaches "Done".
//
// Models are opened like the Example Browser "Open" does: the
// `nflow:open-model-path` document event -> menu-handlers.openModelFromPath,
// which reads the file by ABSOLUTE path (server-side, via nflow.file.readText)
// and binds it as the current file. That sets baseDir to the model's directory,
// so FMU demos find their `.fmu`; an upload (setInputFiles) carries only the base
// name and cannot do that.
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const currentDirectory = path.dirname(fileURLToPath(import.meta.url));
const modulesDir = path.resolve(currentDirectory, '..', '..', '..', '..');
const blocksExamples = path.join(modulesDir, 'nflow_blocks', 'examples');
const fmiExamples = path.join(modulesDir, 'nflow_fmi', 'examples');
// One proof PNG per model, `<category>__<name>.png`, under the Playwright output
// dir so it is cleaned with the other run artifacts.
const screenshotDir = path.resolve(currentDirectory, '..', 'test-results', 'screenshots');
function screenshotPathFor(model: ExampleModel): string {
  const slug = `${model.category}__${model.name.replace(/\.nflow$/i, '')}`.replace(
    /[^A-Za-z0-9_.-]+/g,
    '_'
  );
  return path.join(screenshotDir, `${slug}.png`);
}
//=============================================================================
// Demos are short by design: one that cannot start or progress within this
// window is a regression, not a slow machine.
const RUN_OBSERVE_MS = 60_000;
interface ExampleModel {
  file: string;
  category: string;
  name: string;
}
//=============================================================================
// Demos not required to reach "Done" in the window: interactive or free-running
// by design (dashboard, real-time loop), or too slow under an ASAN build (a
// Nelson-function block re-enters the interpreter every step). They must still
// start and stay error-free; the CLI sim() checks cover completion. Keyed by
// file base name.
const NO_COMPLETION = new Set<string>([
  'Dashboard_Runtime_Demo.nflow',
  'Nelson_Function_Demo.nflow',
]);
//=============================================================================
// Demos whose shipped runtime is too long for the window (e.g. a real-time demo
// paced to wall-clock over a 240s runtime). The walk-through bounds the runtime
// and forces fast execution so they still reach "Done" and get full coverage.
const RUNTIME_OVERRIDE_S = new Map<string, number>([
  ['Mechanical_Ventilation_Demo.nflow', 5],
]);
//=============================================================================
// Demos that need external setup this walk-through cannot provide; they are
// verified elsewhere (a companion .m script).
const REQUIRES_SETUP = new Map<string, string>([
  [
    'From_Workspace_Demo.nflow',
    'needs its companion From_Workspace_Demo.m to define the base-workspace signal "simin"',
  ],
  // FMI-LS-WASM packages run in the browser build only: the native engine has no WebAssembly runtime.
  ['Adder_C_Wasm_FMU.nflow', 'WebAssembly FMU, browser build only'],
  ['Adder_Rust_Wasm_FMU.nflow', 'WebAssembly FMU, browser build only'],
  ['Adder_WAT_Wasm_FMU.nflow', 'WebAssembly FMU, browser build only'],
]);
//=============================================================================
// These scopes must still contain their traces after the simulation reaches "Done".
const FINAL_SCOPE_TRACES = new Map<string, { blockId: string; count: number }>([
  ['Amplitude_Modulation_Demo.nflow', { blockId: 'sc', count: 3 }],
]);
//=============================================================================
// Must exceed the observation window; the default 30s is shorter.
const TEST_TIMEOUT_MS = RUN_OBSERVE_MS + 30_000;
test.describe('example gallery: every model simulates', () => {
  const models = discoverModels(blocksExamples);

  test.beforeEach(async ({ page }) => {
    await bootApp(page);
  });

  test.afterEach(async ({ page }) => {
    await stopSimulation(page);
  });

  for (const model of models) {
    test(`${model.category} / ${model.name}`, async ({ page }) => {
      test.setTimeout(TEST_TIMEOUT_MS);
      const setup = REQUIRES_SETUP.get(model.name);
      test.skip(Boolean(setup), setup || '');
      await runExampleModel(page, model);
    });
  }
});
//=============================================================================
test.describe('example gallery: FMU-backed models', () => {
  const fmuModels = fs.existsSync(fmiExamples) ? discoverModels(fmiExamples) : [];
  // FMU demos reference their .fmu relative to the model file; the open-by-path
  // loader sets baseDir to the model's directory so those references resolve.

  test.beforeEach(async ({ page }) => {
    await bootApp(page);
  });

  test.afterEach(async ({ page }) => {
    await stopSimulation(page);
  });

  for (const model of fmuModels) {
    test(`fmi / ${model.name}`, async ({ page }) => {
      test.setTimeout(TEST_TIMEOUT_MS);
      const setup = REQUIRES_SETUP.get(model.name);
      test.skip(Boolean(setup), setup || '');
      await runExampleModel(page, model);
    });
  }
});
//=============================================================================
test.describe('rigid triple pendulum subsystem', () => {
  const model = path.join(blocksExamples, 'acausal', 'Planar_Rigid_Triple_Pendulum_Demo.nflow');

  test.beforeEach(async ({ page }) => {
    await bootApp(page);
  });

  test('opens and displays the native planar mechanism', async ({ page }) => {
    await openModelByPath(page, model);
    const subsystem = page.locator('#blockLayer g.svg-block[data-block-id="mechanism"]');
    await expect(subsystem).toBeVisible({ timeout: 15_000 });
    await subsystem.dblclick();

    const mechanism = page.locator('#blockLayer g.type-planarMechanicalIsland');
    await expect(mechanism).toBeVisible({ timeout: 15_000 });
    await expect(mechanism).toContainText('Planar mechanism');
    await expect(page.locator('#missingBlocksModal')).not.toHaveClass(/show/);
  });
});
//=============================================================================
// Category is the parent folder, so failures read like the Example Browser groups.
function discoverModels(root: string): ExampleModel[] {
  const out: ExampleModel[] = [];
  const walk = (dir: string): void => {
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const full = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        walk(full);
      } else if (entry.isFile() && entry.name.toLowerCase().endsWith('.nflow')) {
        out.push({
          file: full,
          category: path.basename(path.dirname(full)),
          name: entry.name,
        });
      }
    }
  };
  if (fs.existsSync(root)) walk(root);
  out.sort((a, b) => `${a.category}/${a.name}`.localeCompare(`${b.category}/${b.name}`));
  return out;
}
//=============================================================================
// Opens the console panel so simulation error entries are in the DOM.
async function bootApp(page: Page): Promise<void> {
  await page.goto('/nflow.html');
  await page.waitForFunction(() => typeof (window as any).webui !== 'undefined', undefined, {
    timeout: 20_000,
  });
  await page.waitForFunction(
    () => {
      const webui = (window as any).webui;
      return typeof webui?.isConnected !== 'function' || webui.isConnected();
    },
    undefined,
    { timeout: 20_000 }
  );
  await page.waitForSelector('#svgCanvas', { timeout: 20_000 });
  // The host re-localizes the menubar asynchronously after boot, which can steal
  // focus mid-interaction.
  await page.waitForTimeout(1800);
  await page.locator('.bottom-panel-header').click();
  await expect(page.locator('[data-testid="console-panel"]')).toBeVisible();
}
//=============================================================================
// Dispatches `nflow:open-model-path`, which menu-handlers turns into
// openModelFromPath -> nflow.file.readText -> loadDiagram + setCurrentFilePath.
// The open is fire-and-forget; runExampleModel waits for the first block.
async function openModelByPath(page: Page, absolutePath: string): Promise<void> {
  await page.evaluate((p) => {
    document.dispatchEvent(
      new CustomEvent('nflow:open-model-path', { cancelable: true, detail: { path: p } })
    );
  }, absolutePath);
}
//=============================================================================
// Never throws: a missing image is "no capture", not a test failure.
async function captureProof(page: Page, model: ExampleModel): Promise<void> {
  try {
    fs.mkdirSync(screenshotDir, { recursive: true });
    await page.screenshot({ path: screenshotPathFor(model), fullPage: false });
  } catch {
    // Best-effort proof capture only.
  }
}
//=============================================================================
async function stopSimulation(page: Page): Promise<void> {
  await page
    .evaluate(async () => {
      const webui = (window as any).webui;
      if (typeof webui?.call !== 'function') return;
      try {
        await webui.call('rpc', JSON.stringify({ id: 99, method: 'nflow.sim.stop', params: {} }));
      } catch (_e) {
        // Best-effort cleanup only.
      }
    })
    .catch(() => undefined);
}
//=============================================================================
async function runExampleModel(page: Page, model: ExampleModel): Promise<void> {
  const errorEntries = page.locator('.nf-console-entry.nf-console-error');
  const errorsBefore = await errorEntries.count();

  await openModelByPath(page, model.file);

  // A parse failure renders no block.
  await expect(page.locator('#blockLayer g.svg-block').first()).toBeVisible({ timeout: 15_000 });

  const runtimeOverride = RUNTIME_OVERRIDE_S.get(model.name);
  if (runtimeOverride !== undefined) {
    await page.evaluate((seconds) => {
      const runtime = document.getElementById('runtimeInput') as HTMLInputElement | null;
      if (runtime) {
        runtime.value = String(seconds);
        runtime.dispatchEvent(new Event('input', { bubbles: true }));
        runtime.dispatchEvent(new Event('change', { bubbles: true }));
      }
      const mode = document.getElementById('realTimeMode') as HTMLSelectElement | null;
      if (mode) {
        mode.value = 'fast';
        mode.dispatchEvent(new Event('change', { bubbles: true }));
      }
    }, runtimeOverride);
  }

  const runButton = page.locator('#playPauseBtn');
  await expect(runButton).toHaveAttribute('data-action', 'run', { timeout: 10_000 });
  await runButton.click();
  await expect(runButton).toHaveAttribute('data-action', 'pause', { timeout: 15_000 });

  const wantsCompletion = !NO_COMPLETION.has(model.name);
  if (wantsCompletion) {
    await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: RUN_OBSERVE_MS });
    await expect(runButton).toHaveAttribute('data-action', 'run', { timeout: 5_000 });

    const finalScope = FINAL_SCOPE_TRACES.get(model.name);
    if (finalScope) {
      const paths = page.locator(`[data-block-id="${finalScope.blockId}"] .scope-path`);
      await expect(paths).toHaveCount(finalScope.count);
      for (let index = 0; index < finalScope.count; index += 1) {
        await expect(paths.nth(index)).toHaveAttribute('d', /[ML]/);
      }
    }
  } else {
    // Free-running demo: it must stay alive.
    await page.waitForTimeout(3_000);
    await expect(runButton).toHaveAttribute('data-action', 'pause');
  }

  await captureProof(page, model);

  const errorsAfter = await errorEntries.count();
  if (errorsAfter > errorsBefore) {
    const lastError = (await errorEntries.last().textContent())?.trim() || '(unreadable)';
    throw new Error(`simulation logged an error for ${model.name}: ${lastError}`);
  }
}
//=============================================================================
