//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Connection usability: ports stay discoverable on hover without a prior
// selection (P3), and an existing wire end can be re-targeted onto another
// compatible port by dragging it, instead of deleting and recreating (P5).
//
// Segment-grab tolerance (P4) is a pixel-threshold change exercised by the
// Vitest unit suite; like wire-corner dragging it is not reproduced here,
// because synthetic wires in the headless harness keep a 2-point route and the
// interior-segment geometry is not deterministic enough for a stable e2e.
//=============================================================================
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
//=============================================================================
const currentDir = path.dirname(fileURLToPath(import.meta.url));
// b1 sine -> b2 gain -> b3 scope, laid out left-to-right at the same height.
const THREE_CHAIN = path.resolve(currentDir, '..', 'fixtures', 'three-block-chain.nflow');

async function loadChain(page: Page, editor: CanvasEditor): Promise<void> {
  await page.locator('#fileOpenInput').setInputFiles(THREE_CHAIN);
  await expect(page.locator('#blockLayer > *')).toHaveCount(3, { timeout: 15_000 });
  await expect.poll(() => editor.wireCount(), { timeout: 5000 }).toBe(2);
}
//=============================================================================
type SerializedConn = { from: string; to: string; fromIndex?: number; toIndex?: number };

async function connections(page: import('@playwright/test').Page): Promise<SerializedConn[]> {
  const json = await page.evaluate(() => (window as any).__nf_serializeDiagram?.());
  if (!json) return [];
  const parsed = JSON.parse(json as string);
  return Array.isArray(parsed.connections) ? parsed.connections : [];
}

/** Screen-space centre of a port's group box. */
async function portCenter(
  editor: CanvasEditor,
  blockId: string,
  type: 'in' | 'out',
  index = 0
): Promise<{ x: number; y: number }> {
  const box = await editor.portGroup(blockId, type, index).boundingBox();
  if (!box) throw new Error(`no box for ${blockId}.${type}${index}`);
  return { x: box.x + box.width / 2, y: box.y + box.height / 2 };
}
//=============================================================================
test.describe('connection usability', () => {
  test('P3 - a port is revealed on hover with no block selected', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await editor.insertBlock('gain', { x: 260, y: 220 });

    // Clear any selection the insert left behind so ports are hidden at rest.
    await editor.canvas.click({ position: { x: 20, y: 20 } });
    await expect
      .poll(() => editor.canvas.evaluate((svg) => svg.classList.contains('ports-visible')))
      .toBe(false);

    const outPort = page.locator('.port-group[data-port-type="out"]').first();
    // The hit area advertises wiring with a crosshair cursor at all times.
    await expect(outPort.locator('.port-hit')).toHaveCSS('cursor', 'crosshair');

    // Hovering the port fades it fully in even though nothing is selected.
    await outPort.hover();
    await expect
      .poll(() =>
        outPort
          .locator('.port')
          .first()
          .evaluate((el) => getComputedStyle(el).opacity)
      )
      .toBe('1');
  });

  test('P5 - dragging a wire end re-targets it to another port', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // Grab the b2->b3 wire just past b2's output port (clear of its 12px hit
    // circle, inside the end-grab band) and drop the source end on b1's output.
    // Outputs fan out, so b1 now drives b3 as well.
    const b2Out = await portCenter(editor, 'b2', 'out');
    const b1Out = await portCenter(editor, 'b1', 'out');
    await page.mouse.move(b2Out.x + 18, b2Out.y);
    await page.mouse.down();
    await page.mouse.move(b2Out.x + 40, b2Out.y - 30, { steps: 6 });
    await page.mouse.move(b1Out.x, b1Out.y, { steps: 10 });
    await page.mouse.up();

    // Still two wires; the moved end now starts at b1, and b2->b3 is gone.
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(2);
    await expect
      .poll(async () => (await connections(page)).some((c) => c.from === 'b1' && c.to === 'b3'), {
        timeout: 3000,
      })
      .toBe(true);
    expect((await connections(page)).some((c) => c.from === 'b2' && c.to === 'b3')).toBe(false);
  });

  test('P5 - dropping a wire end in empty space leaves it unchanged', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // Grab the b1->b2 wire near its input end, drop it on empty canvas: no change.
    const b2In = await portCenter(editor, 'b2', 'in');
    await page.mouse.move(b2In.x - 18, b2In.y);
    await page.mouse.down();
    await page.mouse.move(b2In.x - 120, b2In.y + 180, { steps: 8 });
    await page.mouse.up();

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(2);
    expect((await connections(page)).some((c) => c.from === 'b1' && c.to === 'b2')).toBe(true);
  });
});
//=============================================================================
