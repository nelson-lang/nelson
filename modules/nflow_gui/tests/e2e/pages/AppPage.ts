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
 * Page Object for the nflow application. Selectors come from nflow.html and from
 * the component HTML injected by mountSidebarPanels().
 */
export class AppPage {
  readonly page: Page;
  readonly svgCanvas: Locator;
  readonly canvasContainer: Locator;
  readonly blockLayer: Locator;
  readonly loadingOverlay: Locator;
  readonly statusBar: Locator;
  readonly floatingToolbar: Locator;
  readonly playPauseBtn: Locator;
  constructor(page: Page) {
    this.page = page;
    this.svgCanvas = page.locator('#svgCanvas');
    this.canvasContainer = page.locator('#canvas');
    this.blockLayer = page.locator('#blockLayer');
    this.loadingOverlay = page.locator('.nf-loading-overlay');
    this.statusBar = page.locator('.status-bar');
    this.floatingToolbar = page.locator('#floatingToolbar');
    this.playPauseBtn = page.locator('#playPauseBtn');
  }
  //=============================================================================
  async goto(): Promise<void> {
    // Prefer the explicit NFLOW_URL env var when present (safer for direct
    // `npx playwright` runs). Fall back to '/' which will resolve against
    // Playwright's `baseURL` when configured.
    const target = process.env['NFLOW_URL'] ?? '/';
    await this.page.goto(target);
    await this.waitForReady();
  }
  /**
   * Navigate to the URL (or the configured baseURL) and wait until the app is
   * ready. Call it at the start of every test instead of page.goto().
   */
  async open(url?: string): Promise<void> {
    const target = url ?? process.env['NFLOW_URL'] ?? '/';
    // Stub optional host-injected assets (e.g. webui bridge) to avoid
    // benign 404 console errors when running against the static `dist/`
    // server used in CI/local tests.
    try {
      await this.page.route('**/webui.js', async (route) => {
        await route.fulfill({
          status: 200,
          contentType: 'application/javascript',
          body: '// webui stub for tests\n',
        });
      });
    } catch (_e) {}

    try {
      await this.page.route('**/favicon.ico', async (route) => {
        await route.fulfill({ status: 200, contentType: 'image/x-icon', body: '' });
      });
    } catch (_e) {}

    await this.page.goto(target);
    await this.waitForReady();
  }
  /**
   * Wait until the loading overlay is gone (i.e. the `nf-loading` class has
   * been removed from `<html>`) and the SVG canvas is visible.
   *
   * The overlay is removed when the `nf-ready` custom event fires.  A
   * 15-second fallback timer in head-init.ts also removes it.
   */
  async waitForReady(): Promise<void> {
    await this.page.waitForFunction(
      () => !document.documentElement.classList.contains('nf-loading'),
      undefined,
      { timeout: 20_000 }
    );
    await expect(this.svgCanvas).toBeVisible({ timeout: 5_000 });

    // Wait briefly for the library-ready promise (block templates) to
    // complete so tests that load diagrams do not abort due to missing
    // block types. Limit the wait to 10s to avoid long stalls.
    try {
      await this.page.evaluate(async (timeoutMs: number) => {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const p = (window as any).__nf_lib_ready_promise as Promise<unknown> | undefined;
        if (!p || typeof (p as any).then !== 'function') return;
        const timeout = new Promise((res) => setTimeout(res, timeoutMs));
        try {
          await Promise.race([p, timeout]);
        } catch (_e) {
          // swallow
        }
      }, 10_000);
    } catch (_e) {
      // ignore any evaluation errors - tests will proceed and surface
      // missing-blocks if they occur.
    }

    // Ensure the document has focus so keyboard shortcuts are delivered
    // reliably across environments.
    try {
      await this.page.focus('body');
    } catch (_e) {
      // ignore: focus may fail in some headless contexts
    }
  }
  //=============================================================================
  async openMenu(name: 'file' | 'edit' | 'view' | 'help'): Promise<void> {
    const menu = this.page.locator(`.menubar-item[data-menu="${name}"]`);
    await menu.click();
    // Wait shortly for the dropdown/actions to become visible. This
    // reduces flakiness where submenu items are present but hidden by
    // animation or style toggles.
    try {
      await this.page.waitForSelector(
        `.menubar-item[data-menu="${name}"] [data-action]`,
        { state: 'visible', timeout: 3_000 }
      );
    } catch (_e) {
      // ignore - fallback clicks will attempt to trigger actions anyway
    }
  }
  //=============================================================================
  async clickMenuAction(action: string): Promise<void> {
    const selector = `[data-action="${action}"]`;
    const locator = this.page.locator(selector).first();
    try {
      await locator.waitFor({ state: 'visible', timeout: 3_000 });
      await locator.click();
      return;
    } catch (err) {
      // Fallback: trigger a DOM click inside the page context which will
      // invoke attached event listeners even if the element is not visible
      // yet (useful while UI initialization or animations complete).
      try {
        await this.page.evaluate((sel: string) => {
          const el = document.querySelector(sel) as HTMLElement | null;
          if (el) el.click();
        }, selector);
        return;
      } catch (_e) {
        // rethrow original error if fallback also fails
        throw err;
      }
    }
  }
  //=============================================================================
  async undo(): Promise<void> {
    await this.page.keyboard.press('Control+z');
  }
  //=============================================================================
  async redo(): Promise<void> {
    await this.page.keyboard.press('Control+y');
  }
  //=============================================================================
  async deleteSelection(): Promise<void> {
    await this.page.keyboard.press('Delete');
  }
  //=============================================================================
  async title(): Promise<string> {
    return this.page.title();
  }
  //=============================================================================
  async statusText(): Promise<string> {
    const txt = await this.statusBar.textContent();
    return (txt || '').trim();
  }
}
//=============================================================================
