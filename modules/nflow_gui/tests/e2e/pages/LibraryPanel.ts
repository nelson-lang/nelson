//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { type Page, type Locator, expect } from '@playwright/test';
//=============================================================================
/**
 * Page Object for the block library sidebar panel, injected by
 * mountSidebarPanels() as id="panel-library". Its activity-bar toggle has
 * data-panel="library".
 */
export class LibraryPanel {
  readonly page: Page;
  readonly toggleButton: Locator;
  readonly panel: Locator;
  readonly libraryGroups: Locator;
  constructor(page: Page) {
    this.page = page;
    this.toggleButton = page.locator('.activity-icon[data-panel="library"]');
    this.panel = page.locator('#panel-library');
    this.libraryGroups = page.locator('#blockLibraryGroups');
  }
  //=============================================================================
  async open(): Promise<void> {
    const isActive = await this.toggleButton.evaluate((el) =>
      el.classList.contains('active')
    );
    if (!isActive) {
      await this.toggleButton.click();
    }
    // The panel can be active while the sidebar is collapsed.
    const sidebar = this.page.locator('.sidebar');
    const isCollapsed = await sidebar.evaluate((el) => el.classList.contains('collapsed'));
    if (isCollapsed) {
      await this.toggleButton.click();
    }
    await expect(this.panel).toBeVisible({ timeout: 5_000 });
  }
  //=============================================================================
  async close(): Promise<void> {
    const sidebar = this.page.locator('.sidebar');
    const isCollapsed = await sidebar.evaluate((el) => el.classList.contains('collapsed'));
    if (!isCollapsed) {
      await this.toggleButton.click();
    }
  }
  /** Case-insensitive substring match on the visible text. */
  blockByName(name: string): Locator {
    return this.libraryGroups.locator(`[draggable="true"]`, { hasText: new RegExp(name, 'i') });
  }
  /**
   * Drags a block onto the SVG canvas at (targetX, targetY), relative to the
   * canvas. The panel must be open (call open() first).
   */
  async dragBlockToCanvas(blockName: string, targetX: number, targetY: number): Promise<void> {
    const block = this.blockByName(blockName);
    await expect(block.first()).toBeVisible({ timeout: 5_000 });

    const svgCanvas = this.page.locator('#svgCanvas');
    const canvasBox = await svgCanvas.boundingBox();
    if (!canvasBox) throw new Error('SVG canvas not visible');

    const absoluteX = canvasBox.x + targetX;
    const absoluteY = canvasBox.y + targetY;

    await block.first().dragTo(svgCanvas, {
      targetPosition: { x: targetX, y: targetY },
    });

    void absoluteX; // suppress unused warning
  }
}
//=============================================================================
