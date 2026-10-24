//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import { pickRotatableAxes } from '../../src/panels/FigurePanel';
import type { RotatableAxes } from '../../src/rpc/types';
//=============================================================================
// A 640x400 overlay whose top-left is at (10, 20), matching a getBoundingClientRect.
const overlay = { left: 10, top: 20, width: 640, height: 400 };
// A 2x3 subplot grid: three top-row axes, three bottom-row axes.
// rect = [left, top, width, height] in normalized top-origin figure fractions.
const grid: RotatableAxes[] = [
  { handle: 1, rect: [0.05, 0.05, 0.25, 0.4], view: [0, 90] }, // top-left
  { handle: 2, rect: [0.375, 0.05, 0.25, 0.4], view: [10, 80] }, // top-mid
  { handle: 3, rect: [0.7, 0.05, 0.25, 0.4], view: [20, 70] }, // top-right
  { handle: 4, rect: [0.05, 0.55, 0.25, 0.4], view: [30, 60] }, // bottom-left
  { handle: 5, rect: [0.375, 0.55, 0.25, 0.4], view: [-37.5, 30] }, // bottom-mid
  { handle: 6, rect: [0.7, 0.55, 0.25, 0.4], view: [45, 45] }, // bottom-right
];
//=============================================================================
// Convert a normalized figure point to client coordinates inside `overlay`.
function at(fx: number, fy: number): [number, number] {
  return [overlay.left + fx * overlay.width, overlay.top + fy * overlay.height];
}
//=============================================================================
describe('pickRotatableAxes', () => {
  it('picks the subplot whose rect contains the pointer', () => {
    for (const axes of grid) {
      const [left, top, w, h] = axes.rect;
      const [cx, cy] = at(left + w / 2, top + h / 2);
      expect(pickRotatableAxes(grid, overlay, cx, cy)?.handle).toBe(axes.handle);
    }
  });

  it('distinguishes adjacent subplots (only one rotates)', () => {
    const [cx, cy] = at(0.8, 0.25); // inside top-right (handle 3)
    const picked = pickRotatableAxes(grid, overlay, cx, cy);
    expect(picked?.handle).toBe(3);
    expect(picked?.view).toEqual([20, 70]);
  });

  it('returns that axes own View as the drag base, including a 2D axes', () => {
    const [cx, cy] = at(0.175, 0.25); // top-left, still 2D [0, 90]
    expect(pickRotatableAxes(grid, overlay, cx, cy)?.view).toEqual([0, 90]);
  });

  it('falls back to the first axes over a margin (never rotates all at once)', () => {
    const [cx, cy] = at(0.5, 0.5); // gutter between subplots
    expect(pickRotatableAxes(grid, overlay, cx, cy)?.handle).toBe(1);
  });

  it('returns null when there is no per-axes info (legacy engine)', () => {
    const [cx, cy] = at(0.5, 0.5);
    expect(pickRotatableAxes(undefined, overlay, cx, cy)).toBeNull();
    expect(pickRotatableAxes([], overlay, cx, cy)).toBeNull();
  });

  it('falls back to the first axes when the overlay has no size', () => {
    const degenerate = { left: 0, top: 0, width: 0, height: 0 };
    expect(pickRotatableAxes(grid, degenerate, 5, 5)?.handle).toBe(1);
  });
});
//=============================================================================
