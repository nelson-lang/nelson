//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// N3 - Auto-arrange readability. The Edit > Auto-arrange command (runAutoArrange,
// bound to the nflow:auto-arrange event and the Edit menu) must lay a signal
// diagram out cleanly: no overlapping blocks, left-to-right flow, connections
// preserved, and a stable (idempotent) result.
//=============================================================================
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor, boxesOverlap } from '../pages/CanvasEditor.js';
//=============================================================================
const currentDir = path.dirname(fileURLToPath(import.meta.url));
const RLC = path.resolve(
  currentDir,
  '..',
  '..',
  '..',
  '..',
  'nflow_blocks',
  'examples',
  'acausal',
  'Acausal_RLC_Circuit_Demo.nflow'
);
//=============================================================================
interface Built {
  editor: CanvasEditor;
  ids: { sine: string; gain: string; scope: string };
}
//=============================================================================
async function buildChain(page: Page): Promise<Built> {
  const editor = new CanvasEditor(page);
  // Spread the initial placements far apart (and out of final order) so each
  // double-click lands on empty canvas and auto-arrange has something to tidy.
  const sine = await editor.insertBlock('sine', { x: 150, y: 180 });
  const gain = await editor.insertBlock('gain', { x: 520, y: 200 });
  const scope = await editor.insertBlock('scope', { x: 320, y: 380 });
  await editor.connect(sine, 0, gain, 0);
  await editor.connect(gain, 0, scope, 0);
  await expect.poll(() => editor.wireCount(), { timeout: 4000 }).toBe(2);
  return { editor, ids: { sine, gain, scope } };
}
//=============================================================================
function centerX(box: { x: number; w: number }): number {
  return box.x + box.w / 2;
}
//=============================================================================
test.describe('N3 - auto-arrange', () => {
  test('arranged blocks do not overlap and keep their wires', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const { editor } = await buildChain(page);

    await editor.autoArrange();

    const boxes = Object.values(await editor.blockBoxes());
    for (let i = 0; i < boxes.length; i++) {
      for (let j = i + 1; j < boxes.length; j++) {
        expect(
          boxesOverlap(boxes[i], boxes[j]),
          `blocks ${i} and ${j} overlap after auto-arrange`
        ).toBe(false);
      }
    }
    expect(await editor.wireCount()).toBe(2);
  });

  test('auto-arrange is idempotent', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const { editor } = await buildChain(page);

    await editor.autoArrange();
    const first = await editor.blockBoxes();
    await editor.autoArrange();
    const second = await editor.blockBoxes();

    for (const id of Object.keys(first)) {
      expect(Math.abs(centerX(first[id]) - centerX(second[id]))).toBeLessThan(2);
      expect(Math.abs(first[id].y - second[id].y)).toBeLessThan(2);
    }
  });

  test('a signal chain flows left to right', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const { editor, ids } = await buildChain(page);

    await editor.autoArrange();

    const boxes = await editor.blockBoxes();
    expect(centerX(boxes[ids.sine])).toBeLessThan(centerX(boxes[ids.gain]));
    expect(centerX(boxes[ids.gain])).toBeLessThan(centerX(boxes[ids.scope]));
  });

  test('the Edit menu command arranges the diagram', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const { editor } = await buildChain(page);

    await app.openMenu('edit');
    await app.clickMenuAction('autoArrange');
    await page.waitForTimeout(200);

    const boxes = Object.values(await editor.blockBoxes());
    let overlaps = 0;
    for (let i = 0; i < boxes.length; i++) {
      for (let j = i + 1; j < boxes.length; j++) {
        if (boxesOverlap(boxes[i], boxes[j])) overlaps++;
      }
    }
    expect(overlaps).toBe(0);
    expect(await editor.wireCount()).toBe(2);
  });
});
//=============================================================================
// Acausal circuits take a different arrange path (schematicArrangeDiagram, via
// diagramHasPhysicalPins) than causal signal flow, so cover it separately.
test.describe('N3 - auto-arrange (acausal)', () => {
  test('an acausal circuit lays out without overlaps and keeps its physical wires', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);

    await page.locator('#fileOpenInput').setInputFiles(RLC);
    await expect(page.locator('#blockLayer > *')).toHaveCount(9, { timeout: 15_000 });
    const wiresBefore = await page.locator('#wireLayer path.physical-wire').count();
    expect(wiresBefore).toBeGreaterThan(0);

    await editor.autoArrange();

    const boxes = Object.values(await editor.blockBoxes());
    for (let i = 0; i < boxes.length; i++) {
      for (let j = i + 1; j < boxes.length; j++) {
        expect(
          boxesOverlap(boxes[i], boxes[j]),
          `acausal blocks ${i} and ${j} overlap after schematic arrange`
        ).toBe(false);
      }
    }
    expect(await page.locator('#wireLayer path.physical-wire').count()).toBe(wiresBefore);
  });
});
//=============================================================================
