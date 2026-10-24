//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/**
 * Tests for the simulation toolbar controls (play/pause) and the running/stopped
 * state the UI shows. Simulation calls go to the live C++ backend, not a mock.
 */
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { SimulationToolbar } from '../pages/SimulationToolbar.js';
//=============================================================================
test.describe('Simulation controls', () => {
  async function openSimpleDiagram(app: AppPage): Promise<void> {
    await app.open();
    const insert = async (type: string, x: number) => {
      await app.page.locator('#svgCanvas').dblclick({ position: { x, y: 200 } });
      await app.page.locator('#quickInsertInput').fill(type);
      await app.page.locator('#quickInsertInput').press('Enter');
    };
    await insert('sine', 180);
    await insert('scope', 430);
    const blocks = app.page.locator('#blockLayer > *');
    await expect(blocks).toHaveCount(2, { timeout: 10_000 });
    await blocks.first().click();
    await blocks.nth(1).click({ modifiers: ['Control'] });
    await expect(app.page.locator('#wireLayer path')).not.toHaveCount(0);
  }

  test('floating toolbar is present in the DOM', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await expect(page.locator('#floatingToolbar')).toBeAttached();
    await expect(page.locator('#playPauseBtn')).toBeAttached();
  });

  test('simulation panel is accessible via activity bar', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await page.locator('.activity-icon[data-panel="simulation"]').click();

    const sidebar = page.locator('.sidebar');
    const isCollapsed = await sidebar.evaluate((el) => el.classList.contains('collapsed'));
    if (isCollapsed) {
      await page.locator('.activity-icon[data-panel="simulation"]').click();
    }

    await expect(page.locator('#panel-simulation')).toBeVisible({ timeout: 5_000 });
  });

  test('simulation panel contains runtime and sample-time inputs', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await page.locator('.activity-icon[data-panel="simulation"]').click();
    const sidebar = page.locator('.sidebar');
    const isCollapsed = await sidebar.evaluate((el) => el.classList.contains('collapsed'));
    if (isCollapsed) {
      await page.locator('.activity-icon[data-panel="simulation"]').click();
    }

    await expect(page.locator('#runtimeInput')).toBeVisible({ timeout: 5_000 });
    await expect(page.locator('#simDt')).toBeVisible({ timeout: 5_000 });
    await expect(page.locator('#realTimeMode')).toBeVisible({ timeout: 5_000 });
  });

  test('playPauseBtn starts with data-action="run" (stopped state)', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    const sim = new SimulationToolbar(page);
    await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'run', {
      timeout: 5_000,
    });
    expect(await sim.isStopped()).toBe(true);
  });

  test('clicking play triggers simulation start', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await openSimpleDiagram(app);

    const sim = new SimulationToolbar(page);

    await page.locator('#playPauseBtn').click();

    await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'pause', {
      timeout: 10_000,
    });
    expect(await sim.isRunning()).toBe(true);
  });

  test('clicking pause while running pauses the simulation', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await openSimpleDiagram(app);

    await page.locator('#playPauseBtn').click();
    await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'pause', {
      timeout: 10_000,
    });

    await page.locator('#playPauseBtn').click();

    await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'run', {
      timeout: 10_000,
    });
  });

  test('Stop preserves the session display and Reset returns time to zero', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await openSimpleDiagram(app);

    await page.locator('#playPauseBtn').click();
    await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'pause');
    await expect(page.locator('#stopSimToolbarBtn')).toBeEnabled();
    await page.locator('#stopSimToolbarBtn').click();
    await expect(page.locator('#playPauseBtn')).toHaveAttribute('data-action', 'run');
    await expect(page.locator('.nf-console-entry').last()).toContainText(/results preserved/i);

    await page.locator('#resetSimToolbarBtn').click();
    await expect(page.locator('#simTimeDisplay')).toContainText('0.00 /');
    await expect(page.locator('.nf-console-entry').last()).toContainText('Reset');
  });
});
//=============================================================================
