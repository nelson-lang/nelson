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
export class InspectorPanel {
  readonly page: Page;
  readonly panel: Locator;
  readonly inspectorBody: Locator;
  readonly rotateBtn: Locator;
  constructor(page: Page) {
    this.page = page;
    this.panel = page.locator('#panel-properties');
    this.inspectorBody = page.locator('#inspectorBody');
    // The app uses `rotateSelection` as the element id; be tolerant of historical names.
    this.rotateBtn = page.locator('#rotateSelection, #rotateSelectionBtn');
  }
  //=============================================================================
  async open(): Promise<void> {
    await this.page.locator('.activity-icon[data-panel="properties"]').click();
    await expect(this.panel).toBeVisible({ timeout: 5000 });
  }
  //=============================================================================
  async getParam(name: string): Promise<string | null> {
    const el = this.inspectorBody.locator(`[data-edit="${name}"]`).first();
    const tag = await el.evaluate((e) => (e ? e.tagName : null));
    if (!tag) return null;
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return await el.inputValue();
    return (await el.textContent())?.trim() ?? null;
  }
  //=============================================================================
  async setParam(name: string, value: string | boolean | number): Promise<void> {
    const el = this.inspectorBody.locator(`[data-edit="${name}"]`).first();
    const tag = await el.evaluate((e) => (e ? e.tagName : null));
    if (!tag) throw new Error(`Parameter element not found: ${name}`);

    if (tag === 'INPUT') {
      const type = (await el.getAttribute('type')) || '';
      if (type === 'checkbox') {
        if (value) await el.check(); else await el.uncheck();
        return;
      }
      await el.fill(String(value));
      return;
    }

    if (tag === 'TEXTAREA') {
      await el.fill(String(value));
      return;
    }

    if (tag === 'SELECT') {
      await el.selectOption(String(value));
      return;
    }

    await this.page.evaluate(
      ({ selector, v }: { selector: string; v: string }) => {
        const node = document.querySelector(selector);
        if (node) node.textContent = v;
      },
      { selector: `[data-edit="${name}"]`, v: String(value) }
    );
  }
  //=============================================================================
  async rotateBlock(): Promise<void> {
    try {
      const c = await this.rotateBtn.count();
      if (c > 0) {
        if (await this.rotateBtn.isVisible()) {
          await this.rotateBtn.click();
          return;
        }
      }
    } catch (_e) {
      // ignore and attempt fallback
    }

    // Fallback: synthesize a `contextmenu` event on the selected block (more reliable
    // than Playwright's right-click for SVG elements) and then click the rotate action.
    await this.page.evaluate(() => {
      const el = document.querySelector('#blockLayer > *') as Element | null;
      if (!el) return;
      const r = el.getBoundingClientRect();
      const ev = new MouseEvent('contextmenu', {
        bubbles: true,
        cancelable: true,
        clientX: Math.round(r.left + r.width / 2),
        clientY: Math.round(r.top + r.height / 2),
        button: 2,
      });
      el.dispatchEvent(ev);
    });
    const rotateMenu = this.page.locator('.context-menu-item[data-action="rotateRight"]');
    await rotateMenu.waitFor({ state: 'visible', timeout: 3000 });
    await rotateMenu.click();
  }
}
//=============================================================================
