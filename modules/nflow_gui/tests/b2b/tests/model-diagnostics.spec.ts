//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Engine half of the diagnostics tests: a broken model must not fail silently.
// Against the real Nelson engine, running an intentionally invalid model has to
// surface a console error entry and/or a bump in the Diagnostics error counter.
// The inverse of examples.spec, which asserts the shipped demos run clean.
// Fixtures live in tests/invalid/.
//
// Requires the b2b host (`npm run test:b2b`), not the mocked e2e static server.
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const currentDir = path.dirname(fileURLToPath(import.meta.url));
const invalidDir = path.resolve(currentDir, '..', '..', 'invalid');
// Positive control: the harness must tell a good model from a bad one.
const validModel = path.resolve(
  currentDir,
  '..',
  '..',
  '..',
  '..',
  'nflow_engine',
  'tests',
  'Command.nflow'
);
//=============================================================================
const OBSERVE_MS = 30_000;
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
  await page.waitForTimeout(1800);
  // Reveal the bottom console so error entries render in the DOM.
  await page.locator('.bottom-panel-header').click();
  await expect(page.locator('[data-testid="console-panel"]')).toBeVisible();
}
//=============================================================================
async function openModelByPath(page: Page, absolutePath: string): Promise<void> {
  await page.evaluate((p) => {
    document.dispatchEvent(
      new CustomEvent('nflow:open-model-path', { cancelable: true, detail: { path: p } })
    );
  }, absolutePath);
}
//=============================================================================
async function diagnosticErrorCount(page: Page): Promise<number> {
  return page.evaluate(() => {
    const el = document.getElementById('diagnosticErrorCount');
    return el ? Number(el.textContent || '0') || 0 : 0;
  });
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
        /* best-effort cleanup */
      }
    })
    .catch(() => undefined);
}
/** Returns whether a new console error or a diagnostics error appeared during the run. */
async function runAndDetectError(page: Page, absolutePath: string): Promise<boolean> {
  const errorEntries = page.locator('.nf-console-entry.nf-console-error');
  const consoleBefore = await errorEntries.count();
  const diagBefore = await diagnosticErrorCount(page);

  await openModelByPath(page, absolutePath);
  await expect(page.locator('#blockLayer g.svg-block').first()).toBeVisible({ timeout: 15_000 });

  const runButton = page.locator('#playPauseBtn');
  await expect(runButton).toHaveAttribute('data-action', 'run', { timeout: 10_000 });
  await runButton.click();

  const deadline = Date.now() + OBSERVE_MS;
  let surfaced = false;
  while (Date.now() < deadline) {
    const consoleAfter = await errorEntries.count();
    const diagAfter = await diagnosticErrorCount(page);
    if (consoleAfter > consoleBefore || diagAfter > diagBefore) {
      surfaced = true;
      break;
    }
    const action = await runButton.getAttribute('data-action');
    if (action === 'run') break;
    await page.waitForTimeout(500);
  }

  await stopSimulation(page);
  return surfaced;
}
//=============================================================================
test.describe('N2 - engine surfaces broken models', () => {
  test('an algebraic loop is reported, not run silently', async ({ page }) => {
    await bootApp(page);
    const surfaced = await runAndDetectError(page, path.join(invalidDir, 'algebraic_loop.nflow'));
    expect(surfaced, 'algebraic-loop model must surface a diagnostic or console error').toBe(true);
  });

  test('an unconnected input is reported, not run silently', async ({ page }) => {
    await bootApp(page);
    const surfaced = await runAndDetectError(
      page,
      path.join(invalidDir, 'unconnected_input.nflow')
    );
    expect(surfaced, 'unconnected-input model must surface a diagnostic or console error').toBe(
      true
    );
  });

  test('a valid model runs without surfacing an error (control)', async ({ page }) => {
    await bootApp(page);
    const surfaced = await runAndDetectError(page, validModel);
    expect(surfaced, 'a known-good model must not surface an error').toBe(false);
  });

  // On-canvas highlighting (.loop-highlight via nf-error-blocks) is not asserted:
  // the engine returns no per-block `errorBlocks` for these models. It fires for
  // other error classes (e.g. unsupported_block). The diagnostic-to-block wiring
  // is covered in tests/e2e/tests/model-diagnostics.spec.ts (nf-select-block).
});
//=============================================================================
