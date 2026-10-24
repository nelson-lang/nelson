//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Back-to-back wire routing smoke: build a two-block diagram against the
// real Nelson host, drag a block with the pointer and assert the wire stays
// healthy the whole way (orthogonal, endpoints following, no page errors).
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
//=============================================================================
async function quickInsertBlock(
  page: Page,
  type: string,
  position: { x: number; y: number }
): Promise<void> {
  await page.locator('#svgCanvas').dblclick({ position });
  const input = page.locator('#quickInsertInput');
  await expect(input).toBeVisible({ timeout: 10_000 });
  await input.click();
  await input.fill(type);
  await page.keyboard.press('Enter');
}
//=============================================================================
/** Extract SVG command endpoints of a wire path. */
function parsePathPoints(d: string): Array<{ x: number; y: number }> {
  const out: Array<{ x: number; y: number }> = [];
  let current = { x: 0, y: 0 };
  const re = /([MLAQCaqlmc])([^MLAQCaqlmc]*)/g;
  let m: RegExpExecArray | null;
  while ((m = re.exec(d))) {
    const cmd = m[1];
    const nums = (m[2].match(/-?\d+(?:\.\d+)?/g) || []).map(Number);
    const relative = cmd === cmd.toLowerCase();
    if (cmd.toLowerCase() === 'm' || cmd.toLowerCase() === 'l') {
      for (let i = 0; i + 1 < nums.length; i += 2) {
        current = {
          x: relative ? current.x + nums[i] : nums[i],
          y: relative ? current.y + nums[i + 1] : nums[i + 1],
        };
        out.push({ ...current });
      }
    } else if (cmd.toLowerCase() === 'a') {
      for (let i = 0; i + 6 < nums.length; i += 7) {
        current = {
          x: relative ? current.x + nums[i + 5] : nums[i + 5],
          y: relative ? current.y + nums[i + 6] : nums[i + 6],
        };
        out.push({ x: Number.NaN, y: Number.NaN });
        out.push({ ...current });
      }
    } else if (cmd.toLowerCase() === 'q') {
      for (let i = 0; i + 3 < nums.length; i += 4) {
        current = {
          x: relative ? current.x + nums[i + 2] : nums[i + 2],
          y: relative ? current.y + nums[i + 3] : nums[i + 3],
        };
        out.push({ x: Number.NaN, y: Number.NaN });
        out.push({ ...current });
      }
    } else if (cmd.toLowerCase() === 'c') {
      for (let i = 0; i + 5 < nums.length; i += 6) {
        current = {
          x: relative ? current.x + nums[i + 4] : nums[i + 4],
          y: relative ? current.y + nums[i + 5] : nums[i + 5],
        };
        out.push({ x: Number.NaN, y: Number.NaN });
        out.push({ ...current });
      }
    }
  }
  return out;
}
//=============================================================================
function isOrthogonal(points: Array<{ x: number; y: number }>): boolean {
  for (let i = 1; i < points.length; i += 1) {
    if (
      !Number.isFinite(points[i - 1].x) ||
      !Number.isFinite(points[i - 1].y) ||
      !Number.isFinite(points[i].x) ||
      !Number.isFinite(points[i].y)
    ) {
      continue;
    }
    const dx = points[i].x - points[i - 1].x;
    const dy = points[i].y - points[i - 1].y;
    if (dx !== 0 && dy !== 0) return false;
  }
  return true;
}
//=============================================================================
test.beforeEach(async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForFunction(() => typeof (window as any).webui !== 'undefined', undefined, {
    timeout: 20_000,
  });
  await page.waitForFunction(
    () => {
      const webui = (window as any).webui;
      return typeof webui?.isConnected !== 'function' || webui.isConnected();
    },
    undefined,
    { timeout: 20_000 }
  );
  await page.waitForSelector('#svgCanvas', { timeout: 20_000 });
});
//=============================================================================
test('dragging a block keeps its wire orthogonal and attached', async ({ page }) => {
  const pageErrors: string[] = [];
  page.on('pageerror', (err) => pageErrors.push(String(err)));

  // Let the host-locale i18n settle before interacting.
  await page.waitForTimeout(1800);

  await quickInsertBlock(page, 'sine', { x: 300, y: 300 });
  await quickInsertBlock(page, 'scope', { x: 620, y: 300 });
  const blocks = page.locator('#blockLayer g.svg-block');
  await expect(blocks).toHaveCount(2);
  await blocks.nth(0).click();
  await blocks.nth(1).dispatchEvent('click', { bubbles: true, ctrlKey: true });

  const wire = page.locator('#wireLayer path.wire').first();
  await expect(wire).toHaveCount(1);
  const initialD = (await wire.getAttribute('d')) ?? '';
  expect(initialD).not.toContain('NaN');
  const initialPts = parsePathPoints(initialD);
  expect(initialPts.length).toBeGreaterThanOrEqual(2);

  // Block position in SVG units (same space as the path data).
  const readTranslate = async () => {
    const transform = (await blocks.nth(0).getAttribute('transform')) ?? '';
    const m = /translate\(\s*(-?\d+(?:\.\d+)?)[ ,]+(-?\d+(?:\.\d+)?)\s*\)/.exec(transform);
    expect(m).not.toBeNull();
    return { x: Number(m![1]), y: Number(m![2]) };
  };
  const startPos = await readTranslate();

  // Drag the sine block with the pointer (down → several moves → up).
  const sine = blocks.nth(0);
  const box = await sine.boundingBox();
  expect(box).not.toBeNull();
  const startX = box!.x + box!.width / 2;
  const startY = box!.y + box!.height / 2;
  await page.mouse.move(startX, startY);
  await page.mouse.down();
  for (let step = 1; step <= 8; step += 1) {
    await page.mouse.move(startX + step * 5, startY + step * 12);
    // The wire must stay a valid path on every frame of the drag.
    const midD = (await wire.getAttribute('d')) ?? '';
    expect(midD).not.toContain('NaN');
    expect(midD).not.toContain('undefined');
  }
  await page.mouse.up();

  // Let the drop-time (async, native) routing settle.
  await page.waitForTimeout(1500);

  const finalD = (await wire.getAttribute('d')) ?? '';
  expect(finalD).not.toContain('NaN');
  const pts = parsePathPoints(finalD);
  expect(pts.length).toBeGreaterThanOrEqual(2);
  // Routed wires are orthogonal (M/L skeleton; arcs are excluded by parsing).
  expect(isOrthogonal(pts), `wire path: ${finalD}; points: ${JSON.stringify(pts)}`).toBe(true);

  // The wire follows the moved block: the wire's start point moved by the
  // same delta as the block's SVG transform (both in SVG units; the client
  // bounding box would be zoom/pan dependent). Grid-snap tolerance: 1 cell.
  const endPos = await readTranslate();
  const blockDelta = { x: endPos.x - startPos.x, y: endPos.y - startPos.y };
  const wireDelta = { x: pts[0].x - initialPts[0].x, y: pts[0].y - initialPts[0].y };
  expect(Math.abs(wireDelta.x - blockDelta.x)).toBeLessThanOrEqual(12);
  expect(Math.abs(wireDelta.y - blockDelta.y)).toBeLessThanOrEqual(12);
  // And the block actually moved (the drag was real).
  expect(Math.abs(blockDelta.x) + Math.abs(blockDelta.y)).toBeGreaterThan(20);

  expect(pageErrors).toEqual([]);
});
//=============================================================================
