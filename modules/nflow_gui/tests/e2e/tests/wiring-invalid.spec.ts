//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// N1 - Invalid-wiring feedback. The click-to-connect flow (renderer-port.ts +
// renderer-connection.ts) must guide the user: highlight compatible targets,
// refuse output->output, refuse a second wire into an occupied input, and never
// silently create a bad wire. Each rejection surfaces a status message, which in
// turn raises a diagnostic (status-service pushes error-level status as a
// diagnostic), so we assert both the wire count and the user-visible feedback.
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
import { DiagnosticsPanel } from '../pages/DiagnosticsPanel.js';
//=============================================================================
test.describe('N1 - invalid wiring feedback', () => {
  test('output -> output is rejected with a message and no wire', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const diagnostics = new DiagnosticsPanel(page);

    const source = await editor.insertBlock('sine', { x: 200, y: 200 });
    const other = await editor.insertBlock('constant', { x: 200, y: 340 });
    expect(await editor.wireCount()).toBe(0);
    const errorsBefore = await diagnostics.errorCount();

    await editor.clickPort(source, 'out', 0);
    await editor.clickPort(other, 'out', 0);

    expect(await editor.wireCount()).toBe(0);
    expect(await app.statusText()).toContain('output port to an input port');
    await diagnostics.waitForErrorCount(errorsBefore + 1);
  });

  test('output -> input creates exactly one wire and clears the pending state', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);

    const source = await editor.insertBlock('sine', { x: 200, y: 200 });
    const sink = await editor.insertBlock('scope', { x: 460, y: 200 });

    await editor.connect(source, 0, sink, 0);

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(1);
    expect(await editor.isConnectionPending()).toBe(false);
    void app;
  });

  test('a second wire into an occupied input is refused', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const diagnostics = new DiagnosticsPanel(page);

    const source = await editor.insertBlock('sine', { x: 200, y: 200 });
    const sink = await editor.insertBlock('scope', { x: 460, y: 200 });
    const second = await editor.insertBlock('constant', { x: 200, y: 360 });

    await editor.connect(source, 0, sink, 0);
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(1);
    const errorsBefore = await diagnostics.errorCount();

    await editor.connect(second, 0, sink, 0);

    expect(await editor.wireCount()).toBe(1);
    expect(await app.statusText().then((t) => t.toLowerCase())).toContain(
      'already has a connection'
    );
    await diagnostics.waitForErrorCount(errorsBefore + 1);
  });

  test('clicking two ports of the same block cancels without wiring', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);

    const block = await editor.insertBlock('gain', { x: 260, y: 220 });

    await editor.clickPort(block, 'out', 0);
    expect(await editor.isConnectionPending()).toBe(true);
    await editor.clickPort(block, 'in', 0);

    expect(await editor.wireCount()).toBe(0);
    await expect.poll(() => editor.isConnectionPending(), { timeout: 3000 }).toBe(false);
    void app;
  });

  test('pending connection highlights compatible inputs and shows a preview', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);

    const source = await editor.insertBlock('sine', { x: 200, y: 200 });
    const sink = await editor.insertBlock('scope', { x: 460, y: 200 });

    await editor.startConnection(source, 0);

    // Every input port is flagged compatible; a live preview path is drawn.
    await expect(editor.portGroup(sink, 'in', 0)).toHaveClass(/compatible/);
    await expect(page.locator('#overlayLayer .connection-preview')).toHaveCount(1);

    await editor.cancelConnection();
    await expect.poll(() => editor.isConnectionPending(), { timeout: 3000 }).toBe(false);
    await expect(page.locator('#overlayLayer .connection-preview')).toHaveCount(0);
    void app;
  });
});
//=============================================================================
