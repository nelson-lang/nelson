//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Opening a model must auto-fit the canvas: every block fully inside the
// viewport and roughly centred, without clicking the "fit" button. See
// diagram-handlers loadDiagram (deferred fit) and app.getViewportSize (zero-size
// guard).
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const currentDirectory = path.dirname(fileURLToPath(import.meta.url));
const modulesDir = path.resolve(currentDirectory, '..', '..', '..', '..');
const blocksExamples = path.join(modulesDir, 'nflow_blocks', 'examples');
// A spread of aspect ratios: two causal models and the tall/wide acausal pendulum.
const MODELS = [
  path.join(blocksExamples, 'causal', 'PID_Demo.nflow'),
  path.join(blocksExamples, 'causal', 'Signal_Utilities_Demo.nflow'),
  path.join(blocksExamples, 'acausal', 'Planar_Double_Pendulum_Demo.nflow'),
];
//=============================================================================
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
// Both boxes are in the SVG user-coordinate system the viewBox maps.
async function readFrame(
  page: Page
): Promise<{ vb: { x: number; y: number; w: number; h: number }; bb: { x: number; y: number; w: number; h: number }; count: number } | null> {
  return page.evaluate(() => {
    const svg = document.getElementById('svgCanvas') as SVGSVGElement | null;
    const layer = document.getElementById('blockLayer') as SVGGElement | null;
    if (!svg || !layer) return null;
    const raw = svg.getAttribute('viewBox');
    if (!raw) return null;
    const parts = raw.split(/\s+/).map(Number);
    if (parts.length !== 4 || parts.some((n) => !Number.isFinite(n))) return null;
    const blocks = layer.querySelectorAll('g.svg-block');
    if (blocks.length === 0) return null;
    const bb = (layer as unknown as SVGGraphicsElement).getBBox();
    return {
      vb: { x: parts[0], y: parts[1], w: parts[2], h: parts[3] },
      bb: { x: bb.x, y: bb.y, w: bb.width, h: bb.height },
      count: blocks.length,
    };
  });
}
//=============================================================================
async function assertFramed(page: Page): Promise<void> {
  // The deferred fit runs on a double rAF: poll until the frame has settled.
  await expect
    .poll(
      async () => {
        const f = await readFrame(page);
        if (!f) return false;
        const eps = 1;
        return (
          f.bb.x >= f.vb.x - eps &&
          f.bb.y >= f.vb.y - eps &&
          f.bb.x + f.bb.w <= f.vb.x + f.vb.w + eps &&
          f.bb.y + f.bb.h <= f.vb.y + f.vb.h + eps
        );
      },
      { timeout: 10_000, message: 'viewBox never framed the blocks' }
    )
    .toBe(true);

  const f = await readFrame(page);
  expect(f, 'frame data available').not.toBeNull();
  if (!f) return;

  const eps = 1;
  expect(f.bb.x, 'left edge inside').toBeGreaterThanOrEqual(f.vb.x - eps);
  expect(f.bb.y, 'top edge inside').toBeGreaterThanOrEqual(f.vb.y - eps);
  expect(f.bb.x + f.bb.w, 'right edge inside').toBeLessThanOrEqual(f.vb.x + f.vb.w + eps);
  expect(f.bb.y + f.bb.h, 'bottom edge inside').toBeLessThanOrEqual(f.vb.y + f.vb.h + eps);

  const vbCx = f.vb.x + f.vb.w / 2;
  const vbCy = f.vb.y + f.vb.h / 2;
  const bbCx = f.bb.x + f.bb.w / 2;
  const bbCy = f.bb.y + f.bb.h / 2;
  expect(Math.abs(bbCx - vbCx), 'centred horizontally').toBeLessThan(f.vb.w * 0.25);
  expect(Math.abs(bbCy - vbCy), 'centred vertically').toBeLessThan(f.vb.h * 0.25);

  // Zoomed to the content, not a speck in a huge viewBox.
  const fillX = f.bb.w / f.vb.w;
  const fillY = f.bb.h / f.vb.h;
  expect(Math.max(fillX, fillY), 'content fills the viewport').toBeGreaterThan(0.4);
}
//=============================================================================
test.describe('fit-on-open: every opened model is framed', () => {
  test.beforeEach(async ({ page }) => {
    await bootApp(page);
  });

  for (const file of MODELS) {
    test(`${path.basename(file)} is centred and fully visible after open`, async ({ page }) => {
      await openModelByPath(page, file);
      await expect(page.locator('#blockLayer g.svg-block').first()).toBeVisible({ timeout: 15_000 });
      await assertFramed(page);
    });
  }
});
//=============================================================================
// Clicking a card's "Open" loads the model and unmounts the modal in the same
// tick, so the overlay is still tearing down while the load-time fit runs. Only
// driving the real modal (not the raw open-model event) exercises that race.
test.describe('fit-on-open: opening through the Example Browser modal frames the model', () => {
  const SEARCHES = ['PID', 'Pendulum'];

  test.beforeEach(async ({ page }) => {
    await bootApp(page);
  });

  for (const term of SEARCHES) {
    test(`example matching "${term}" is framed after opening from the gallery`, async ({ page }) => {
      await page.evaluate(() => {
        document.dispatchEvent(
          new CustomEvent('nflow:example-browser-show', { cancelable: true })
        );
      });
      const dialog = page.locator('.nf-ex-dialog[role="dialog"]');
      await expect(dialog).toBeVisible({ timeout: 10_000 });

      await dialog.locator('.example-gallery-search').fill(term);
      const openBtn = dialog.locator('.example-gallery-card button.example-gallery-primary', { hasText: 'Open' }).first();
      await expect(openBtn).toBeVisible({ timeout: 10_000 });
      await openBtn.click();

      await expect(dialog).toBeHidden({ timeout: 10_000 });
      await expect(page.locator('#blockLayer g.svg-block').first()).toBeVisible({ timeout: 15_000 });
      await assertFramed(page);
    });
  }
});
//=============================================================================
