//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Page Object for the block-diagram editing surface: inserting blocks through
// the quick-insert popup, drawing (and mis-drawing) wires through the port
// click-to-connect flow, and reading back the resulting geometry. Selectors are
// derived from the real renderer (renderer-port.ts / renderer-connection.ts) and
// quick-insert.tsx, so the usability specs exercise the same DOM a user drives.
//=============================================================================
import { type Page, type Locator, expect } from '@playwright/test';
//=============================================================================
export interface Point {
  x: number;
  y: number;
}
//=============================================================================
export type PortType = 'in' | 'out';
export class CanvasEditor {
  readonly page: Page;
  readonly canvas: Locator;
  readonly blockLayer: Locator;
  readonly wireLayer: Locator;
  constructor(page: Page) {
    this.page = page;
    this.canvas = page.locator('#svgCanvas');
    this.blockLayer = page.locator('#blockLayer');
    this.wireLayer = page.locator('#wireLayer');
  }
  //=============================================================================
  /** Ids of the top-level blocks currently rendered, in document order. */
  async blockIds(): Promise<string[]> {
    return this.page.evaluate(() => {
      const layer = document.getElementById('blockLayer');
      if (!layer) return [];
      return Array.from(layer.children)
        .map((el) => el.getAttribute('data-block-id'))
        .filter((id): id is string => Boolean(id));
    });
  }
  //=============================================================================
  async blockCount(): Promise<number> {
    return this.blockLayer.locator('> *').count();
  }
  //=============================================================================
  /** Number of visible wires (excludes the transparent .wire-hit hit areas). */
  async wireCount(): Promise<number> {
    return this.wireLayer.locator('path.wire').count();
  }
  /**
   * Insert a block of `type` at canvas position `at` via the quick-insert popup
   * (double-click empty canvas -> type -> Enter). Returns the id of the block
   * that appeared. Retries the popup a couple of times to absorb focus races.
   */
  async insertBlock(type: string, at: Point): Promise<string> {
    const before = await this.blockIds();
    let lastError: unknown = null;

    for (let attempt = 0; attempt < 3; attempt++) {
      try {
        await this.canvas.dblclick({ position: at });
        const input = this.page.locator('#quickInsertInput').first();
        await expect(input).toBeVisible({ timeout: 5000 });
        await input.fill(type);
        await input.press('Enter');
        await expect(this.blockLayer.locator('> *')).toHaveCount(before.length + 1, {
          timeout: 5000,
        });
        const after = await this.blockIds();
        const fresh = after.find((id) => !before.includes(id));
        if (fresh) return fresh;
        return after[after.length - 1];
      } catch (error) {
        lastError = error;
        if ((await this.blockCount()) > before.length) {
          const after = await this.blockIds();
          const fresh = after.find((id) => !before.includes(id));
          if (fresh) return fresh;
        }
        await this.page.keyboard.press('Escape').catch(() => {});
        await this.page
          .locator('#quickInsertPopup')
          .waitFor({ state: 'detached', timeout: 1000 })
          .catch(() => {});
      }
    }
    throw lastError ?? new Error(`Failed to insert block of type "${type}"`);
  }
  //=============================================================================
  portGroup(blockId: string, type: PortType, index = 0): Locator {
    return this.page.locator(
      `.port-group[data-block-id="${blockId}"][data-port-type="${type}"][data-port-index="${index}"]`
    );
  }
  //=============================================================================
  /** Click a port (its always-clickable hit area). */
  async clickPort(blockId: string, type: PortType, index = 0): Promise<void> {
    const port = this.portGroup(blockId, type, index);
    await expect(port).toHaveCount(1);
    await port.click();
  }
  //=============================================================================
  /** True while a connection is being drawn (an output port is pending). */
  async isConnectionPending(): Promise<boolean> {
    return this.canvas.evaluate((svg) => svg.classList.contains('connection-pending'));
  }
  //=============================================================================
  /** Begin a connection from an output port and assert it entered pending mode. */
  async startConnection(blockId: string, index = 0): Promise<void> {
    await this.clickPort(blockId, 'out', index);
    await expect.poll(() => this.isConnectionPending(), { timeout: 3000 }).toBe(true);
  }
  /**
   * Draw a connection from an output port to an input port by clicking both.
   * Does not assert success -- callers check wireCount()/status for valid vs
   * rejected attempts.
   */
  async connect(fromId: string, fromIndex: number, toId: string, toIndex: number): Promise<void> {
    await this.clickPort(fromId, 'out', fromIndex);
    await this.clickPort(toId, 'in', toIndex);
  }
  //=============================================================================
  async cancelConnection(): Promise<void> {
    await this.page.keyboard.press('Escape').catch(() => {});
    await this.canvas.click({ position: { x: 5, y: 5 } }).catch(() => {});
  }
  /**
   * Screen-space bounding boxes of every rendered block, by id. Uses
   * getBoundingClientRect so pan/zoom/block transforms are all folded in and the
   * boxes are directly comparable to each other (getBBox would drop the group's
   * own translate and make every block look co-located).
   */
  async blockBoxes(): Promise<Record<string, { x: number; y: number; w: number; h: number }>> {
    return this.page.evaluate(() => {
      const layer = document.getElementById('blockLayer');
      const out: Record<string, { x: number; y: number; w: number; h: number }> = {};
      if (!layer) return out;
      for (const el of Array.from(layer.children)) {
        const id = el.getAttribute('data-block-id');
        if (!id) continue;
        const r = (el as SVGGraphicsElement).getBoundingClientRect();
        out[id] = { x: r.left, y: r.top, w: r.width, h: r.height };
      }
      return out;
    });
  }
  /**
   * Screen-space ORIGIN (transform translate) of each block, by id, via
   * getScreenCTM. Unlike blockBoxes this ignores the block's content extent
   * (ports sticking out), so it is the right measure for alignment: align-left
   * lines up block origins, not bounding boxes.
   */
  async blockOrigins(): Promise<Record<string, { x: number; y: number }>> {
    return this.page.evaluate(() => {
      const layer = document.getElementById('blockLayer');
      const out: Record<string, { x: number; y: number }> = {};
      if (!layer) return out;
      for (const el of Array.from(layer.children)) {
        const id = el.getAttribute('data-block-id');
        if (!id) continue;
        const ctm = (el as SVGGraphicsElement).getScreenCTM?.();
        if (ctm) out[id] = { x: ctm.e, y: ctm.f };
      }
      return out;
    });
  }
  //=============================================================================
  /** Trigger a full-diagram auto-arrange via the app event and settle routing. */
  async autoArrange(): Promise<void> {
    await this.page.evaluate(() => {
      document.dispatchEvent(new CustomEvent('nflow:auto-arrange'));
    });
    await this.page.waitForTimeout(200);
  }
}
//=============================================================================
/** True when two screen-space rectangles overlap (with a small tolerance). */
export function boxesOverlap(
  a: { x: number; y: number; w: number; h: number },
  b: { x: number; y: number; w: number; h: number },
  tolerance = 1
): boolean {
  return (
    a.x < b.x + b.w - tolerance &&
    a.x + a.w > b.x + tolerance &&
    a.y < b.y + b.h - tolerance &&
    a.y + a.h > b.y + tolerance
  );
}
//=============================================================================
