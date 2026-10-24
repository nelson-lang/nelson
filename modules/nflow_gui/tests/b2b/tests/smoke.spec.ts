//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Back-to-back smoke suite against the real Nelson engine (no mocks): the
// webui bridge, the native bindings, the C++ simulation engine and the
// native router are all exercised for real.
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
//=============================================================================
async function nflowRpc(page: Page, method: string, params: Record<string, unknown> = {}) {
  return page.evaluate(
    async ([rpcMethod, rpcParams]) => {
      const parseResult = (result: unknown) => {
        if (typeof result !== 'string') return result;
        const trimmed = result.trim();
        if (!trimmed || !'[{'.includes(trimmed[0])) return result;
        try {
          return JSON.parse(trimmed);
        } catch (_e) {
          return result;
        }
      };
      const webui = (window as any).webui;
      const raw = await webui.call(
        'rpc',
        JSON.stringify({ id: 1, method: rpcMethod, params: rpcParams })
      );
      const parsed = JSON.parse(String(raw));
      if (parsed.error) throw new Error(parsed.error.message || 'native RPC error');
      return parseResult(parsed.result);
    },
    [method, params] as const
  );
}
//=============================================================================
async function quickInsertBlock(
  page: Page,
  type: string,
  position: { x: number; y: number }
): Promise<void> {
  await page.locator('#svgCanvas').dblclick({ position });
  const input = page.locator('#quickInsertInput');
  await expect(input).toBeVisible({ timeout: 10_000 });
  await input.click();
  await input.fill(type);
  await page.keyboard.press('Enter');
}
//=============================================================================
test.beforeEach(async ({ page }) => {
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
});
//=============================================================================
test.afterEach(async ({ page }) => {
  await page
    .evaluate(async () => {
      const webui = (window as any).webui;
      if (typeof webui?.call !== 'function') return;
      try {
        await webui.call('rpc', JSON.stringify({ id: 99, method: 'nflow.sim.stop', params: {} }));
      } catch (_e) {
        // Best-effort cleanup only; individual tests assert their own outcome.
      }
    })
    .catch(() => undefined);
});
//=============================================================================
test('app boots against a real Nelson host', async ({ page }) => {
  await expect(page.locator('#svgCanvas')).toBeVisible();
  await expect(nflowRpc(page, 'nflow.ping')).resolves.toMatchObject({ pong: true });
});
//=============================================================================
test('native library RPC responds with an array', async ({ page }) => {
  const result = await nflowRpc(page, 'nflow.library.list');
  expect(Array.isArray(result)).toBe(true);
});
//=============================================================================
test('native expression engine evaluates expressions', async ({ page }) => {
  const res = await nflowRpc(page, 'nflow.expr.eval', {
    payload: JSON.stringify({ expr: '2*x+1', variables: { x: 4 } }),
  });
  expect(res.ok).toBe(true);
  expect(res.value).toBe(9);
});
//=============================================================================
test('native router routes a simple two-block snapshot', async ({ page }) => {
  const snapshot = {
    blocks: [
      { id: 'a', x: 0, y: 0, width: 40, height: 40, outputs: 1, inputs: 0 },
      { id: 'b', x: 200, y: 0, width: 40, height: 40, outputs: 0, inputs: 1 },
    ],
    connections: [{ from: 'a', fromPort: 0, to: 'b', toPort: 0 }],
    world: { w: 400, h: 200 },
  };
  const res = await nflowRpc(page, 'nflow.router.routeWires', {
    payload: JSON.stringify(snapshot),
  });
  expect(res.ok).toBe(true);
  expect(Array.isArray(res.routes)).toBe(true);
});
//=============================================================================
test('console panel logs real UI actions on the Nelson host', async ({ page }) => {
  await page.locator('.bottom-panel-header').click();
  await expect(page.locator('[data-testid="console-panel"]')).toBeVisible();
  // The host re-localizes the menubar asynchronously (host locale, up to
  // ~1s of retries) which closes dropdowns mid-interaction: let it settle.
  await page.waitForTimeout(1800);
  await page.locator('.menubar-item[data-menu="file"]').click();
  await page.locator('.menubar-item[data-action="new"]').click();
  await page.locator('#dontSaveConfirm').click();
  const entries = page.locator('.nf-console-entry');
  await expect(entries.last()).toContainText(/new diagram|nouveau/i, { timeout: 15_000 });
});
//=============================================================================
test('real sim error links the console entry to the block', async ({ page }) => {
  await page.waitForTimeout(1800);
  // Diverging algebraic loop: sine feeds a sum whose second input is its own
  // output through a unit gain. The Gauss-Seidel relaxation gains sin(t) per
  // sweep, so the first nonzero sample fails to converge.
  const loopModel = {
    name: 'alg_loop_smoke',
    blocks: [
      { id: 's', type: 'sine', x: 200, y: 200, params: { Amplitude: 1, Frequency: 1, Phase: 0 } },
      { id: 'a', type: 'sum', x: 420, y: 200, params: {} },
      { id: 'g', type: 'gain', x: 420, y: 380, params: { Gain: 1 } },
      { id: 'sc', type: 'scope', x: 640, y: 200, params: {} },
    ],
    connections: [
      { from: 's', to: 'a', fromIndex: 0, toIndex: 0 },
      { from: 'g', to: 'a', fromIndex: 0, toIndex: 1 },
      { from: 'a', to: 'g', fromIndex: 0, toIndex: 0 },
      { from: 'a', to: 'sc', fromIndex: 0, toIndex: 0 },
    ],
    variables: '',
    runtime: 1,
    sampleTime: 0.1,
  };
  const modelPath = path.join(os.tmpdir(), 'nf_b2b_alg_loop.nflow');
  fs.writeFileSync(modelPath, JSON.stringify(loopModel));
  await page.evaluate(async () => {
    const p = (window as any).__nf_lib_ready_promise as Promise<unknown> | undefined;
    if (p && typeof p.then === 'function') {
      try {
        await p;
      } catch (_e) {
        /* surfaced later by loadDiagram */
      }
    }
  });
  await page.locator('#fileOpenInput').setInputFiles(modelPath);
  await expect(page.locator('#blockLayer g.svg-block')).toHaveCount(4);
  await page.locator('.bottom-panel-header').click();
  await page.locator('#svgCanvas').click({ position: { x: 50, y: 50 } });
  await page.keyboard.press('Control+t');
  const errorEntry = page.locator('.nf-console-entry.nf-console-error').last();
  await expect(errorEntry).toContainText(/algebraic loop did not converge/i, {
    timeout: 15_000,
  });
  await expect(errorEntry).toHaveClass(/nf-console-link/);
  await expect(page.locator('#blockLayer g.svg-block.loop-highlight')).not.toHaveCount(0);
  await errorEntry.click();
  await expect(page.locator('#blockLayer g.svg-block.selected')).toHaveCount(1);
});
//=============================================================================
test('quick-connect + real sim: time display, signal badge on wire hover', async ({ page }) => {
  await page.waitForTimeout(1800);
  await quickInsertBlock(page, 'sine', { x: 380, y: 260 });
  await quickInsertBlock(page, 'scope', { x: 620, y: 260 });
  const blocks = page.locator('#blockLayer g.svg-block');
  await expect(blocks).toHaveCount(2);
  await blocks.first().click();
  await blocks.nth(1).dispatchEvent('click', { bubbles: true, ctrlKey: true });
  await expect(page.locator('#wireLayer path')).not.toHaveCount(0);
  await page.evaluate(() => {
    const runtime = document.getElementById('runtimeInput') as HTMLInputElement | null;
    if (runtime) {
      runtime.value = '1';
      runtime.dispatchEvent(new Event('input', { bubbles: true }));
      runtime.dispatchEvent(new Event('change', { bubbles: true }));
    }
  });
  await page.locator('#svgCanvas').click({ position: { x: 50, y: 50 } });
  await page.keyboard.press('Control+t');
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 20_000 });
  await page.locator('#wireLayer path').first().hover({ force: true });
  const badge = page.locator('#signalBadge');
  await expect(badge).toBeVisible();
  await expect(badge).toHaveText(/^-?[\d.e+-]+$/);
});
//=============================================================================
test('quick-connect + real sim renders scope traces in the UI', async ({ page }) => {
  await page.waitForTimeout(1800);
  await quickInsertBlock(page, 'sine', { x: 380, y: 260 });
  await quickInsertBlock(page, 'scope', { x: 620, y: 260 });
  const blocks = page.locator('#blockLayer g.svg-block');
  await expect(blocks).toHaveCount(2);
  await blocks.first().click();
  await blocks.nth(1).dispatchEvent('click', { bubbles: true, ctrlKey: true });
  await expect(page.locator('#wireLayer path')).not.toHaveCount(0);

  await page.evaluate(() => {
    const runtime = document.getElementById('runtimeInput') as HTMLInputElement | null;
    if (runtime) {
      runtime.value = '1';
      runtime.dispatchEvent(new Event('input', { bubbles: true }));
      runtime.dispatchEvent(new Event('change', { bubbles: true }));
    }
  });
  await page.locator('#svgCanvas').click({ position: { x: 50, y: 50 } });
  await page.keyboard.press('Control+t');
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 20_000 });

  const scopePath = page.locator('#blockLayer g.svg-block').nth(1).locator('.scope-path').first();
  await expect
    .poll(async () => (await scopePath.getAttribute('d')) || '', { timeout: 5_000 })
    .toMatch(/[ML]/);
  await expect(scopePath).toHaveAttribute('d', /[ML][^"]+\s[ML]/);
});
//=============================================================================
test('quick-connect + real-time sim can pause before completion', async ({ page }) => {
  await page.waitForTimeout(1800);
  await quickInsertBlock(page, 'sine', { x: 380, y: 260 });
  await quickInsertBlock(page, 'scope', { x: 620, y: 260 });
  const blocks = page.locator('#blockLayer g.svg-block');
  await expect(blocks).toHaveCount(2);
  await blocks.first().click();
  await blocks.nth(1).dispatchEvent('click', { bubbles: true, ctrlKey: true });
  await expect(page.locator('#wireLayer path')).not.toHaveCount(0);

  await page.evaluate(() => {
    const runtime = document.getElementById('runtimeInput') as HTMLInputElement | null;
    if (runtime) {
      runtime.value = '10';
      runtime.dispatchEvent(new Event('input', { bubbles: true }));
      runtime.dispatchEvent(new Event('change', { bubbles: true }));
    }
    const mode = document.getElementById('realTimeMode') as HTMLSelectElement | null;
    if (mode) {
      mode.value = 'real-time';
      mode.dispatchEvent(new Event('change', { bubbles: true }));
    }
  });

  await page.locator('#svgCanvas').click({ position: { x: 50, y: 50 } });
  await page.keyboard.press('Control+t');
  await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'pause', {
    timeout: 10_000,
  });
  await page.waitForTimeout(500);
  await page.locator('#playPauseBtn').click();
  await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'run', {
    timeout: 2_000,
  });
  const simText = (await page.locator('#simTimeDisplay').textContent()) || '';
  expect(simText).not.toContain('100%');
});
//=============================================================================
test('quick-connect + real-time sim refreshes scope traces before completion', async ({ page }) => {
  await page.waitForTimeout(1800);
  await quickInsertBlock(page, 'sine', { x: 380, y: 260 });
  await quickInsertBlock(page, 'scope', { x: 620, y: 260 });
  const blocks = page.locator('#blockLayer g.svg-block');
  await expect(blocks).toHaveCount(2);
  await blocks.first().click();
  await blocks.nth(1).dispatchEvent('click', { bubbles: true, ctrlKey: true });
  await expect(page.locator('#wireLayer path')).not.toHaveCount(0);

  await page.evaluate(() => {
    const runtime = document.getElementById('runtimeInput') as HTMLInputElement | null;
    if (runtime) {
      runtime.value = '10';
      runtime.dispatchEvent(new Event('input', { bubbles: true }));
      runtime.dispatchEvent(new Event('change', { bubbles: true }));
    }
    const mode = document.getElementById('realTimeMode') as HTMLSelectElement | null;
    if (mode) {
      mode.value = 'real-time';
      mode.dispatchEvent(new Event('change', { bubbles: true }));
    }
  });

  const scopePath = page.locator('#blockLayer g.svg-block').nth(1).locator('.scope-path').first();
  await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'run', {
    timeout: 5_000,
  });
  await page.locator('#playPauseBtn').click();
  await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'pause', {
    timeout: 10_000,
  });
  await expect
    .poll(
      async () => {
        const simText = (await page.locator('#simTimeDisplay').textContent()) || '';
        const pathData = (await scopePath.getAttribute('d')) || '';
        return { running: !simText.includes('100%'), hasTrace: /[ML]/.test(pathData), simText };
      },
      { timeout: 8_000 }
    )
    .toMatchObject({ running: true, hasTrace: true });

  await page.locator('#playPauseBtn').click();
  await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'run', {
    timeout: 2_000,
  });
});
//=============================================================================
test('real C++ simulation produces scope data', async ({ page }) => {
  const diagram = {
    blocks: [
      {
        id: 'src1',
        type: 'sine',
        x: 0,
        y: 0,
        params: { amplitude: 1, frequency: 1, phase: 0, offset: 0 },
      },
      { id: 'scope1', type: 'scope', x: 200, y: 0, params: {} },
    ],
    connections: [{ from: 'src1', fromPort: 0, to: 'scope1', toPort: 0 }],
    settings: { runTime: 1, sampleTime: 0.01 },
  };
  const res = await nflowRpc(page, 'nflow.sim.run', { diagramJson: JSON.stringify(diagram) });
  expect(res.status).toBe('done');
  expect(Array.isArray(res.scopes)).toBe(true);
  expect(res.scopes.length).toBeGreaterThan(0);
  const scope = res.scopes[0];
  expect(Array.isArray(scope.times)).toBe(true);
  expect(scope.times.length).toBeGreaterThan(10);
});
//=============================================================================
