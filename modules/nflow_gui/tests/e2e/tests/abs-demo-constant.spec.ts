//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// abs_demo: the Constant(1) -> |x| -> Scope chain. Checks that the Constant "1"
// is a well-formed, runnable source in the editor: it is created with Value 1,
// the chain wires through to the scope, the model raises no diagnostics, and a
// run starts cleanly.
//
// Note: in the hostless e2e harness the simulation RPC is answered by the in-page
// shim (webui-mock), so this scenario asserts editor correctness and a clean run
// start, NOT the numeric scope trace. The numeric output (the scope sees a
// constant 1) is proven engine-side by __nflow_simulate__.
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
import { InspectorPanel } from '../pages/InspectorPanel.js';
import { DiagnosticsPanel } from '../pages/DiagnosticsPanel.js';
import { SimulationToolbar } from '../pages/SimulationToolbar.js';
//=============================================================================
test.describe('abs_demo - Constant(1) -> |x| -> Scope', () => {
  test('the Constant 1 is wired through |x| to the scope and the model runs', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    const editor = new CanvasEditor(page);
    await app.open();

    // Build the chain the user drew: Constant -> abs -> Scope.
    const cId = await editor.insertBlock('constant', { x: 180, y: 200 });
    const aId = await editor.insertBlock('abs', { x: 380, y: 200 });
    const sId = await editor.insertBlock('scope', { x: 560, y: 200 });
    await expect(editor.blockLayer.locator('> *')).toHaveCount(3);

    // Constant output -> abs input, abs output -> scope input.
    await editor.connect(cId, 0, aId, 0);
    await editor.connect(aId, 0, sId, 0);
    await expect.poll(() => editor.wireCount(), { timeout: 5000 }).toBe(2);

    // The "1": the Constant carries Value = 1.
    await page.locator(`#blockLayer > [data-block-id="${cId}"]`).click();
    const inspector = new InspectorPanel(page);
    await inspector.open();
    await inspector.setParam('Value', 1);
    expect(await inspector.getParam('Value')).toBe('1');

    // A Constant -> abs -> Scope chain is a valid model: no error diagnostics.
    const diagnostics = new DiagnosticsPanel(page);
    expect(await diagnostics.errorCount()).toBe(0);

    // Run it: the simulation starts cleanly (toolbar enters the running state).
    const sim = new SimulationToolbar(page);
    await page.locator('#playPauseBtn').click();
    await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'pause', {
      timeout: 10_000,
    });
    expect(await sim.isRunning()).toBe(true);

    // Still no error surfaced by starting the run.
    expect(await diagnostics.errorCount()).toBe(0);
  });
});
//=============================================================================
