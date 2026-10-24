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
import type { Locator as PLLocator } from '@playwright/test';
//=============================================================================
export class ContextMenu {
  readonly page: Page;
  readonly blockMenu: Locator;
  readonly canvasMenu: Locator;
  constructor(page: Page) {
    this.page = page;
    this.blockMenu = page.locator('#blockContextMenu');
    this.canvasMenu = page.locator('#canvasContextMenu');
  }
  //=============================================================================
  async openOnBlock(block: PLLocator): Promise<void> {
    await block.click({ button: 'right' });
    await expect(this.blockMenu).toBeVisible({ timeout: 3000 });
  }
  //=============================================================================
  async openOnCanvas(x: number, y: number): Promise<void> {
    await this.page.mouse.click(x, y, { button: 'right' });
    await expect(this.canvasMenu).toBeVisible({ timeout: 3000 });
  }
  //=============================================================================
  async clickAction(action: string): Promise<void> {
    const selector = `#blockContextMenu [data-action="${action}"], #canvasContextMenu [data-action="${action}"]`;
    const item = this.page.locator(selector).first();
    await expect(item).toHaveCount(1);
    if (await item.isVisible()) {
      await item.click();
    } else {
      await item.dispatchEvent('click');
    }
  }
}
//=============================================================================
