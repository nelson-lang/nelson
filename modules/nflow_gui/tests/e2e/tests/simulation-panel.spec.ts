//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Simulation panel E2E tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { SimulationPanel } from '../pages/SimulationPanel.js';
import { SimulationToolbar } from '../pages/SimulationToolbar.js';
//=============================================================================
test.describe('Simulation panel', () => {
  test('opens simulation panel and applies variables', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    const sim = new SimulationPanel(page);
    await sim.open();

    await sim.setRuntime(5);
    await sim.setSampleTime(0.1);
    await sim.setDiagramName('e2e-test-diagram');
    await sim.setVariables('a=1\nb=2');
    await sim.applyVariables();

    const preview = await sim.variablesPreviewText();
    expect(preview.length).toBeGreaterThan(0);
  });

  test('play / pause toggles via floating toolbar', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const toolbar = new SimulationToolbar(page);

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    await toolbar.clickPlayPause();
    await page.waitForTimeout(200);
    expect(await toolbar.isRunning()).toBe(true);

    await toolbar.clickPlayPause();
    await page.waitForTimeout(200);
    expect(await toolbar.isStopped()).toBe(true);
  });
});
//=============================================================================
