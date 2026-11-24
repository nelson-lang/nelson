//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, type Page } from '@playwright/test';
//=============================================================================
// steps shared by the real-engine specs; the console and the editor share one tab group
export async function runLine(page: Page, line: string, waitForReady = true) {
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });
  const input = page.locator('.console-input');
  if (!(await input.isVisible())) {
    await page.getByText('Command Window', { exact: true }).first().click();
  }
  await input.click();
  await input.fill(line);
  await input.press('Enter');
  if (waitForReady) {
    await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });
  }
}
//=============================================================================
export async function terminalText(page: Page) {
  return page
    .locator('.console-host')
    .first()
    .evaluate((element) => {
      const copy = element.cloneNode(true) as HTMLElement;
      copy.querySelectorAll('.console-input-stack').forEach((prompt) => prompt.remove());
      return copy.innerText.replace(/\u00a0/g, ' ');
    });
}
//=============================================================================
// fronts the tab the next step reads or clicks
export async function showTab(page: Page, name: string) {
  const tab = page.locator('.dv-tab', { hasText: name }).first();
  if (await tab.count()) await tab.click();
}
//=============================================================================
// hovers the first whole-word occurrence of `word` in the editor text and returns the hover text
export async function hoverWord(page: Page, tabName: string, word: string): Promise<string> {
  // the console may have been fronted by runLine: show the editor tab again
  await showTab(page, tabName);
  // Monaco lays a fronted tab out asynchronously: measure the word only once it is painted
  await expect(page.locator('.editor-panel:visible .view-lines').first()).toContainText(word);
  await page.waitForTimeout(300);
  const point = await page
    .locator('.editor-panel:visible')
    .first()
    .evaluate((panel, w) => {
      const pattern = new RegExp('(^|[^A-Za-z0-9_])(' + w + ')(?![A-Za-z0-9_])');
      for (const line of Array.from(panel.querySelectorAll('.view-line'))) {
        const walker = document.createTreeWalker(line, NodeFilter.SHOW_TEXT);
        let node: Node | null = walker.nextNode();
        while (node) {
          const text = node.textContent ?? '';
          const m = pattern.exec(text);
          if (m) {
            const begin = m.index + m[1].length;
            const range = document.createRange();
            range.setStart(node, begin);
            range.setEnd(node, begin + w.length);
            const r = range.getBoundingClientRect();
            return { x: r.left + r.width / 2, y: r.top + r.height / 2 };
          }
          node = walker.nextNode();
        }
      }
      return null;
    }, word);
  if (!point) throw new Error(`word ${word} not found in the editor`);
  // Monaco keeps an empty glyph hover container in the DOM: take the visible, filled one
  const hover = page.locator('.monaco-hover-content:visible').first();
  for (let attempt = 0; attempt < 3; attempt++) {
    await page.mouse.move(point.x - 40, point.y + 40);
    await page.waitForTimeout(150);
    await page.mouse.move(point.x, point.y, { steps: 4 });
    await page.waitForTimeout(150);
    await page.mouse.move(point.x + 1, point.y);
    const shown = await hover
      .waitFor({ state: 'visible', timeout: 10_000 })
      .then(() => true)
      .catch(() => false);
    if (shown) break;
  }
  await expect(hover).toBeVisible({ timeout: 10_000 });
  await expect
    .poll(async () => (await hover.innerText()).trim(), { timeout: 20_000 })
    .not.toMatch(/^(Loading\.\.\.)?$/);
  return (await hover.innerText()).trim();
}
//=============================================================================
// the text of the line holding the editor cursor, read from the painted view
export async function cursorLineText(page: Page): Promise<string | null> {
  return page.evaluate(() => {
    const panel = Array.from(document.querySelectorAll('.editor-panel')).find(
      (p) => (p as HTMLElement).offsetParent !== null
    );
    if (!panel) return null;
    const cursor = panel.querySelector('.monaco-editor .cursor');
    if (!cursor) return null;
    const cy = cursor.getBoundingClientRect().top + 2;
    const line = Array.from(panel.querySelectorAll('.view-line')).find((l) => {
      const r = l.getBoundingClientRect();
      return cy >= r.top && cy < r.bottom;
    });
    // Monaco paints spaces as no-break spaces
    return line ? (line.textContent ?? '').replace(/\u00a0/g, ' ') : null;
  });
}
