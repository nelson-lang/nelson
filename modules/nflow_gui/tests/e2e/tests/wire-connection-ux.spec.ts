//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Connection usability: hover reveals ports, a wire end re-targets by drag,
// fan-out, block on wire insert and Shift chaining. Pixel-exact gestures
// (segment tolerance, align to connect) live in the unit suites instead.
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
  test('a port is revealed on hover with no block selected', async ({ mockPage: page }) => {
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

  test('dragging a wire end re-targets it to another port', async ({ mockPage: page }) => {
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

  test('pressing a connected input port and dragging detaches and re-targets the wire', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // A fresh sink below the chain offers a free input to move the end to.
    const sink = await editor.insertBlock('scope', { x: 320, y: 440 });
    await editor.canvas.click({ position: { x: 20, y: 20 } });
    const sinkIn = await portCenter(editor, sink, 'in');

    // Press directly on b3's already-connected input port and drag to the sink.
    // (Not the wire band: this exercises the port grab-to-detach path.)
    const b3In = await portCenter(editor, 'b3', 'in');
    await page.mouse.move(b3In.x, b3In.y);
    await page.mouse.down();
    await page.mouse.move(b3In.x - 40, b3In.y + 80, { steps: 6 });
    await page.mouse.move(sinkIn.x, sinkIn.y, { steps: 10 });
    await page.mouse.up();

    // The wire detached from b3 and now feeds the sink; b3's input is free.
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(2);
    await expect
      .poll(async () => (await connections(page)).some((c) => c.from === 'b2' && c.to === sink), {
        timeout: 3000,
      })
      .toBe(true);
    expect((await connections(page)).some((c) => c.from === 'b2' && c.to === 'b3')).toBe(false);
  });

  test('after re-targeting a wire the align hint does not re-propose it', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // A sink well below the chain gives the detached end a new home.
    const sink = await editor.insertBlock('scope', { x: 320, y: 440 });
    await editor.canvas.click({ position: { x: 20, y: 20 } });
    const sinkIn = await portCenter(editor, sink, 'in');

    // Detach the b2->b3 wire at b3's input and drop it on the sink. b3's input is
    // freed and lines up with b2's output, so without the Simulink-style
    // suppression the align hint would pop up proposing to recreate b2->b3.
    const b3In = await portCenter(editor, 'b3', 'in');
    await page.mouse.move(b3In.x - 18, b3In.y);
    await page.mouse.down();
    await page.mouse.move(b3In.x - 60, b3In.y + 120, { steps: 6 });
    await page.mouse.move(sinkIn.x, sinkIn.y, { steps: 10 });
    await page.mouse.up();

    // The re-target took effect: b2 now drives the sink, and b2->b3 is gone.
    await expect
      .poll(async () => (await connections(page)).some((c) => c.from === 'b2' && c.to === sink), {
        timeout: 3000,
      })
      .toBe(true);
    expect((await connections(page)).some((c) => c.from === 'b2' && c.to === 'b3')).toBe(false);

    // Precondition that makes this discriminating: b2's output and the freed
    // b3 input are aligned, so without the suppression the hint WOULD appear.
    const b2Out = await portCenter(editor, 'b2', 'out');
    expect(Math.abs(b2Out.y - b3In.y)).toBeLessThanOrEqual(8);

    // Simulink leaves a re-targeted port silently unconnected: no align hint
    // nags the user to undo the move they just made.
    await expect(page.locator('#_nflow-align-hint')).toHaveCount(0);
  });

  test('endpoint handles on a selected wire re-target either end, output included', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // Select the b2->b3 wire by clicking its midpoint.
    const b2Out = await portCenter(editor, 'b2', 'out');
    const b3In = await portCenter(editor, 'b3', 'in');
    await page.mouse.click((b2Out.x + b3In.x) / 2, (b2Out.y + b3In.y) / 2);
    await expect(page.locator('#wireLayer path.wire.selected')).toHaveCount(1);

    // A selected wire shows a handle at each end; grab the source ('from') one.
    await expect(page.locator('.wire-endpoint-handle')).toHaveCount(2);
    const handleBox = await page.locator('.wire-endpoint-handle[data-end="from"]').boundingBox();
    if (!handleBox) throw new Error('no from-handle box');
    const b1Out = await portCenter(editor, 'b1', 'out');

    // Drag the output end onto b1's output: the wire's source becomes b1.
    await page.mouse.move(handleBox.x + handleBox.width / 2, handleBox.y + handleBox.height / 2);
    await page.mouse.down();
    await page.mouse.move(b1Out.x, b1Out.y - 30, { steps: 6 });
    await page.mouse.move(b1Out.x, b1Out.y, { steps: 8 });
    await page.mouse.up();

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(2);
    await expect
      .poll(async () => (await connections(page)).some((c) => c.from === 'b1' && c.to === 'b3'), {
        timeout: 3000,
      })
      .toBe(true);
    expect((await connections(page)).some((c) => c.from === 'b2' && c.to === 'b3')).toBe(false);
  });

  test('a near-miss drop snaps the wire end to the nearest compatible port', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // Select the b2->b3 wire, grab its source handle.
    const b2Out = await portCenter(editor, 'b2', 'out');
    const b3In = await portCenter(editor, 'b3', 'in');
    await page.mouse.click((b2Out.x + b3In.x) / 2, (b2Out.y + b3In.y) / 2);
    await expect(page.locator('.wire-endpoint-handle')).toHaveCount(2);
    const handleBox = await page.locator('.wire-endpoint-handle[data-end="from"]').boundingBox();
    if (!handleBox) throw new Error('no from-handle box');
    const b1Out = await portCenter(editor, 'b1', 'out');

    // Release NEAR b1's output, not exactly on it: the snap still re-targets.
    await page.mouse.move(handleBox.x + handleBox.width / 2, handleBox.y + handleBox.height / 2);
    await page.mouse.down();
    await page.mouse.move(b1Out.x, b1Out.y - 40, { steps: 6 });
    await page.mouse.move(b1Out.x, b1Out.y + 15, { steps: 8 }); // ~15px below the port
    await page.mouse.up();

    await expect
      .poll(async () => (await connections(page)).some((c) => c.from === 'b1' && c.to === 'b3'), {
        timeout: 3000,
      })
      .toBe(true);
    expect((await connections(page)).some((c) => c.from === 'b2' && c.to === 'b3')).toBe(false);
  });

  test('the branch handle on a selected wire fans the source out to another input', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    const sink = await editor.insertBlock('scope', { x: 320, y: 440 });
    await editor.canvas.click({ position: { x: 20, y: 20 } });
    const sinkIn = await portCenter(editor, sink, 'in');

    // Select the b1->b2 wire, then grab its branch ("+") handle at the middle.
    const b1Out = await portCenter(editor, 'b1', 'out');
    const b2In = await portCenter(editor, 'b2', 'in');
    await page.mouse.click((b1Out.x + b2In.x) / 2, (b1Out.y + b2In.y) / 2);
    await expect(page.locator('.wire-branch-handle')).toHaveCount(1);
    const branchBox = await page.locator('.wire-branch-handle').boundingBox();
    if (!branchBox) throw new Error('no branch-handle box');

    // Drag the handle to the sink's input: b1 now drives b2 (unchanged) and the sink.
    await page.mouse.move(branchBox.x + branchBox.width / 2, branchBox.y + branchBox.height / 2);
    await page.mouse.down();
    await page.mouse.move(branchBox.x, branchBox.y + 40, { steps: 6 });
    await page.mouse.move(sinkIn.x, sinkIn.y, { steps: 10 });
    await page.mouse.up();

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(3);
    await expect
      .poll(async () => (await connections(page)).some((c) => c.from === 'b1' && c.to === sink), {
        timeout: 3000,
      })
      .toBe(true);
    expect((await connections(page)).some((c) => c.from === 'b1' && c.to === 'b2')).toBe(true);
  });

  test('dropping a wire end in empty space leaves it unchanged', async ({ mockPage: page }) => {
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

  test('Ctrl drag from a wire branches its source to another input', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // A fresh scope gives a free input to fan out into.
    const sink = await editor.insertBlock('scope', { x: 320, y: 360 });
    await editor.canvas.click({ position: { x: 20, y: 20 } });
    const sinkIn = await portCenter(editor, sink, 'in');

    // Ctrl+drag the middle of the b1->b2 wire onto the new input: b1 now drives
    // both b2 (unchanged) and the sink, so a third wire appears.
    const b1Out = await portCenter(editor, 'b1', 'out');
    const b2In = await portCenter(editor, 'b2', 'in');
    const mid = { x: (b1Out.x + b2In.x) / 2, y: (b1Out.y + b2In.y) / 2 };
    await page.keyboard.down('Control');
    await page.mouse.move(mid.x, mid.y);
    await page.mouse.down();
    await page.mouse.move(mid.x + 20, mid.y + 30, { steps: 6 });
    await page.mouse.move(sinkIn.x, sinkIn.y, { steps: 10 });
    await page.mouse.up();
    await page.keyboard.up('Control');

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(3);
    await expect
      .poll(async () => (await connections(page)).some((c) => c.from === 'b1' && c.to === sink), {
        timeout: 3000,
      })
      .toBe(true);
    // The original b1->b2 wire is untouched.
    expect((await connections(page)).some((c) => c.from === 'b1' && c.to === 'b2')).toBe(true);
  });

  test('Ctrl drag from a wire dropped in empty space adds nothing', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    const b1Out = await portCenter(editor, 'b1', 'out');
    const b2In = await portCenter(editor, 'b2', 'in');
    const mid = { x: (b1Out.x + b2In.x) / 2, y: (b1Out.y + b2In.y) / 2 };
    await page.keyboard.down('Control');
    await page.mouse.move(mid.x, mid.y);
    await page.mouse.down();
    await page.mouse.move(mid.x - 80, mid.y + 200, { steps: 8 });
    await page.mouse.up();
    await page.keyboard.up('Control');

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(2);
  });

  test('dragging a block onto a wire splices it into the chain', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // Widen the b2->b3 wire by sliding b3 far right, so the dropped block has a
    // long free span to land on without overlapping either neighbour.
    const b3box = (await editor.blockBoxes())['b3'];
    const b3c = { x: b3box.x + b3box.w / 2, y: b3box.y + b3box.h / 2 };
    await page.mouse.move(b3c.x, b3c.y);
    await page.mouse.down();
    await page.mouse.move(b3c.x + 20, b3c.y, { steps: 3 });
    await page.mouse.move(b3c.x + 340, b3c.y, { steps: 12 });
    await page.mouse.up();

    // A spare gain to drop onto the middle of the b2->b3 wire.
    const g = await editor.insertBlock('gain', { x: 360, y: 440 });
    await editor.canvas.click({ position: { x: 20, y: 40 } });

    const b2Out = await portCenter(editor, 'b2', 'out');
    const b3In = await portCenter(editor, 'b3', 'in');
    const mid = { x: (b2Out.x + b3In.x) / 2, y: (b2Out.y + b3In.y) / 2 };
    const box = (await editor.blockBoxes())[g];
    const from = { x: box.x + box.w / 2, y: box.y + box.h / 2 };

    // Drag the block body (not a port) onto the middle of the wire.
    await page.mouse.move(from.x, from.y);
    await page.mouse.down();
    await page.mouse.move(from.x + 12, from.y - 12, { steps: 4 });
    await page.mouse.move(mid.x, mid.y, { steps: 14 });
    await page.mouse.up();

    // The wire is cut and the block threaded in: b2->g and g->b3 replace b2->b3.
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(3);
    await expect
      .poll(
        async () => {
          const conns = await connections(page);
          return (
            conns.some((c) => c.from === 'b2' && c.to === g) &&
            conns.some((c) => c.from === g && c.to === 'b3')
          );
        },
        { timeout: 3000 }
      )
      .toBe(true);
    expect((await connections(page)).some((c) => c.from === 'b2' && c.to === 'b3')).toBe(false);
  });

  test('holding Shift chains several connections from one source', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadChain(page, editor);

    // One atan2 block offers two free inputs to fan the Sine (b1) output into.
    const sink = await editor.insertBlock('atan2', { x: 340, y: 380 });
    await editor.canvas.click({ position: { x: 20, y: 20 } });

    // Click the source output to start, then Shift-click both inputs in a row:
    // the source stays pending between them, so both wires come from b1.
    await editor.portGroup('b1', 'out', 0).click();
    await editor.portGroup(sink, 'in', 0).click({ modifiers: ['Shift'] });
    await editor.portGroup(sink, 'in', 1).click({ modifiers: ['Shift'] });

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(4);
    await expect
      .poll(
        async () =>
          (await connections(page)).filter((c) => c.from === 'b1' && c.to === sink).length,
        {
          timeout: 3000,
        }
      )
      .toBe(2);
  });
});
//=============================================================================
