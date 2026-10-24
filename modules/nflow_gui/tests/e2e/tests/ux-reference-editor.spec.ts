//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// NFlow editor UX batch: quick-connect, Ctrl+drag duplicate, wire labels,
// minimap, subsystem breadcrumb, and simulation time display.
//=============================================================================
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage';
//=============================================================================
const quickConnectModifier = process.platform === 'darwin' ? 'Meta' : 'Control';
async function quickInsert(page: Page, type: string, x: number, y: number): Promise<void> {
  const blocks = page.locator('#blockLayer g.svg-block');
  const before = await blocks.count();
  let lastError: unknown = null;

  for (let attempt = 0; attempt < 3; attempt++) {
    try {
      await page.locator('#svgCanvas').dblclick({ position: { x, y } });
      const input = page.locator('#quickInsertInput').first();
      await expect(input).toBeVisible({ timeout: 5000 });
      await input.fill(type);
      await input.press('Enter');
      await expect(blocks).toHaveCount(before + 1, { timeout: 5000 });
      return;
    } catch (error) {
      lastError = error;
      if ((await blocks.count()) > before) return;
      await page.keyboard.press('Escape').catch(() => {});
      await page
        .locator('#quickInsertPopup')
        .waitFor({ state: 'detached', timeout: 1000 })
        .catch(() => {});
    }
  }

  throw lastError;
}
//=============================================================================
test.describe('NFlow editor UX', () => {
  test.beforeEach(async ({ page }) => {
    const app = new AppPage(page);
    await app.goto();
  });

  test('minimap is mounted and toggles from the View menu', async ({ page }) => {
    const minimap = page.locator('#minimap');
    await expect(minimap).toHaveCount(1);
    await expect(minimap).toBeVisible();
    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="toggleMinimap"]').click();
    await expect(minimap).toBeHidden();
    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="toggleMinimap"]').click();
    await expect(minimap).toBeVisible();
  });

  test('view controls toggle from the View menu and persist', async ({ page }) => {
    const controls = page.getByRole('toolbar', { name: 'Diagram view controls' });
    await expect(controls).toBeVisible();

    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="toggleViewControls"]').click();
    await expect(controls).toBeHidden();
    await expect
      .poll(() => page.evaluate(() => localStorage.getItem('nf-canvas-navigation-open')))
      .toBe('0');

    await page.reload();
    // Menus are wired once the app is ready: a click before that is lost.
    await new AppPage(page).waitForReady();
    await expect(controls).toBeHidden();

    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="toggleViewControls"]').click();
    await expect(controls).toBeVisible();
  });

  test('simulation toolbar toggles from the View menu and persists', async ({ page }) => {
    const toolbar = page.locator('#floatingToolbar');
    await expect(toolbar).toBeVisible();

    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="toggleSimulationToolbar"]').click();
    await expect(toolbar).toBeHidden();
    await expect
      .poll(() => page.evaluate(() => localStorage.getItem('nf-simulation-toolbar-open')))
      .toBe('0');

    await page.reload();
    // Menus are wired once the app is ready: a click before that is lost.
    await new AppPage(page).waitForReady();
    await expect(toolbar).toBeHidden();

    await page.locator('.menubar-item[data-menu="simulation"]').click();
    await expect(page.locator('.menubar-item[data-action="run"]')).toBeVisible();

    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="toggleSimulationToolbar"]').click();
    await expect(toolbar).toBeVisible();
  });

  test('restores settings and resets UI preferences without changing the diagram', async ({
    page,
  }) => {
    await quickInsert(page, 'gain', 260, 180);
    const blocks = page.locator('#blockLayer g.svg-block');
    await expect(blocks).toHaveCount(1);

    await page.locator('.activity-icon[data-panel="settings"]').click();
    const includeMain = page.locator('#codegenIncludeMain');
    await includeMain.uncheck();
    await expect
      .poll(() => page.evaluate(() => localStorage.getItem('nf-codegen-include-main')))
      .toBe('0');

    await page.reload();
    // Menus are wired once the app is ready: a click before that is lost.
    await new AppPage(page).waitForReady();
    await page.locator('.activity-icon[data-panel="settings"]').click();
    await expect(includeMain).not.toBeChecked();
    await quickInsert(page, 'gain', 260, 180);
    await expect(blocks).toHaveCount(1);

    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="resetUiPreferences"]').click();
    await expect
      .poll(() => page.evaluate(() => localStorage.getItem('nf-codegen-include-main')))
      .toBeNull();
    await expect(blocks).toHaveCount(1);
  });

  test('subsystem breadcrumb is hidden on the root diagram', async ({ page }) => {
    await expect(page.locator('#subsystemBreadcrumb')).toBeHidden();
  });

  test('library search ignores accents and inserts a selected block', async ({ page }) => {
    const search = page.getByRole('searchbox', { name: 'Search blocks' });
    await expect(search).toBeVisible();
    await search.fill('gáin');
    const result = page.getByRole('button', { name: 'Gain Math' });
    await expect(result).toBeVisible();
    await result.click();

    await expect(page.locator('#blockLayer g.svg-block')).toHaveCount(1);
    await expect(page.locator('.inspector-pane')).toBeVisible();
    await expect(page.locator('#inspectorBody')).toContainText('Gain');
    const gain = page.getByRole('textbox', { name: 'Gain', exact: true });
    await gain.fill('2.5');
    await expect(gain).toHaveValue('2.5');

    await search.fill('not-a-real-block');
    await expect(page.locator('#blockLibraryEmpty')).toBeVisible();
  });

  test('Simulation menu exposes one coherent command group', async ({ page }) => {
    await page.locator('.menubar-item[data-menu="simulation"]').click();
    const menu = page.locator('#menu-simulation');
    await expect(menu.locator('[data-action="run"]')).toContainText(/Run|Resume/);
    await expect(menu.locator('[data-action="pause"]')).toContainText('Pause');
    await expect(menu.locator('[data-action="stop"]')).toContainText('Stop');
    await expect(menu.locator('[data-action="reset"]')).toContainText('Reset');
    await expect(menu.locator('[data-action="simulationSettings"]')).toContainText(
      'Simulation Settings'
    );
  });

  test('workspace panels do not overlap at supported widths', async ({ page }) => {
    for (const viewport of [
      { width: 1440, height: 900 },
      { width: 1280, height: 800 },
      { width: 1024, height: 768 },
    ]) {
      await page.setViewportSize(viewport);
      await page.waitForTimeout(100);
      const layout = await page.evaluate(() => {
        const visibleRect = (selector: string): DOMRect | null => {
          const element = document.querySelector<HTMLElement>(selector);
          if (!element || getComputedStyle(element).display === 'none' || element.offsetWidth === 0)
            return null;
          return element.getBoundingClientRect();
        };
        const sidebar = visibleRect('.sidebar');
        const canvas = visibleRect('#canvas');
        const inspector = visibleRect('.inspector-pane');
        return {
          sidebarRight: sidebar?.right ?? null,
          canvasLeft: canvas?.left ?? null,
          canvasRight: canvas?.right ?? null,
          canvasWidth: canvas?.width ?? 0,
          inspectorLeft: inspector?.left ?? null,
          visibleSidePanels: Number(Boolean(sidebar)) + Number(Boolean(inspector)),
        };
      });
      expect(layout.canvasWidth).toBeGreaterThan(0);
      if (layout.sidebarRight !== null)
        expect(layout.sidebarRight).toBeLessThanOrEqual(layout.canvasLeft! + 1);
      if (layout.inspectorLeft !== null)
        expect(layout.canvasRight).toBeLessThanOrEqual(layout.inspectorLeft + 1);
      if (viewport.width === 1024) expect(layout.visibleSidePanels).toBeLessThanOrEqual(1);
    }
  });

  test('Ctrl+click quick-connects the selected block to a target', async ({ page }) => {
    await quickInsert(page, 'gain', 180, 200);
    await quickInsert(page, 'scope', 430, 200);
    const blocks = page.locator('#blockLayer g.svg-block');
    await expect(blocks).toHaveCount(2);
    await blocks.first().click();
    await blocks.nth(1).click({ modifiers: [quickConnectModifier] });
    await expect(page.locator('#wireLayer path')).not.toHaveCount(0);
    await page.locator('.bottom-panel-header').click();
    await expect(page.locator('.nf-console-entry').last()).toContainText(/connected/i);
  });

  test('Ctrl+drag leaves a duplicate at the source position', async ({ page }) => {
    await quickInsert(page, 'gain', 300, 200);
    const blocks = page.locator('#blockLayer g.svg-block');
    await expect(blocks).toHaveCount(1);
    const box = await blocks.first().boundingBox();
    expect(box).not.toBeNull();
    const cx = box!.x + box!.width / 2;
    const cy = box!.y + box!.height / 2;
    await page.keyboard.down('Control');
    await page.mouse.move(cx, cy);
    await page.mouse.down();
    await page.mouse.move(cx + 140, cy + 90, { steps: 8 });
    await page.mouse.up();
    await page.keyboard.up('Control');
    await expect(blocks).toHaveCount(2);
  });

  test('double-click a wire to set a signal label', async ({ page }) => {
    await quickInsert(page, 'gain', 180, 200);
    await quickInsert(page, 'scope', 430, 200);
    const blocks = page.locator('#blockLayer g.svg-block');
    await blocks.first().click();
    await blocks.nth(1).click({ modifiers: [quickConnectModifier] });
    const wire = page.locator('#wireLayer path').first();
    await expect(wire).toHaveCount(1);
    await wire.dblclick({ force: true });
    const input = page.locator('#wireLabelInput');
    await expect(input).toBeVisible();
    await input.fill('speed');
    await input.press('Enter');
    await expect(page.locator('#overlayLayer .wire-label')).toHaveText('speed');
  });

  test('simulation time display appears in the floating toolbar', async ({ page }) => {
    await quickInsert(page, 'sine', 180, 200);
    await quickInsert(page, 'scope', 430, 200);
    const blocks = page.locator('#blockLayer g.svg-block');
    await blocks.first().click();
    await blocks.nth(1).click({ modifiers: [quickConnectModifier] });
    // Toolbar becomes visible once blocks exist.
    await expect(page.locator('#floatingToolbar')).toBeVisible();
    await expect(page.locator('#simTimeDisplay')).toHaveCount(1);
    // Starting a run reveals the counter (the 100% end state is asserted in
    // the b2b suite against the real engine).
    await page.locator('#svgCanvas').click({ position: { x: 50, y: 50 } });
    await page.keyboard.press('Control+t');
    await expect(page.locator('#simTimeDisplay')).toContainText('/', { timeout: 10_000 });
  });
});
//=============================================================================
