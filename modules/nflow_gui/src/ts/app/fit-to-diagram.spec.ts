//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { beforeEach, test, expect, vi } from 'vitest';
//=============================================================================
const setViewBox = vi.fn();
const setZoomScale = vi.fn();
const initViewBox = vi.fn();
// Partial mock: spy on the view setters, but keep the real getViewportSize and
// clampZoomScale so fit-to-diagram exercises the single shared implementation
// (getViewportSize reads the #canvas rect set by mockCanvas below).
vi.mock('./workspace-handlers.js', async (importOriginal) => {
  const actual = await importOriginal<typeof import('./workspace-handlers.js')>();
  return {
    ...actual,
    setViewBox: (vb: any) => setViewBox(vb),
    setZoomScale: (s: any) => setZoomScale(s),
    initViewBox: () => initViewBox(),
  };
});
//=============================================================================
import { fitToDiagram } from './fit-to-diagram.js';
import { state } from './state.js';
//=============================================================================
function mockCanvas(w: number, h: number): void {
  const canvas = document.createElement('div');
  canvas.setAttribute('id', 'canvas');
  // @ts-ignore - jsdom leaves getBoundingClientRect at zero size.
  canvas.getBoundingClientRect = () => ({ width: w, height: h });
  document.body.appendChild(canvas);
}
//=============================================================================
function lastViewBox(): { x: number; y: number; w: number; h: number } {
  return setViewBox.mock.calls[setViewBox.mock.calls.length - 1][0];
}
//=============================================================================
function viewCenter(): { x: number; y: number } {
  const vb = lastViewBox();
  return { x: vb.x + vb.w / 2, y: vb.y + vb.h / 2 };
}
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
  state.blocks.clear();
  state.connections = [];
  setViewBox.mockClear();
  setZoomScale.mockClear();
  initViewBox.mockClear();
});
//=============================================================================
test('empty diagram falls back to initViewBox', () => {
  mockCanvas(800, 600);
  fitToDiagram();
  expect(initViewBox).toHaveBeenCalledTimes(1);
  expect(setViewBox).not.toHaveBeenCalled();
});
//=============================================================================
test('centres the view on the block bounding box', () => {
  mockCanvas(800, 600);
  state.blocks.set(1, { x: 0, y: 0, width: 100, height: 100 } as any);
  state.blocks.set(2, { x: 300, y: 0, width: 100, height: 100 } as any);
  fitToDiagram();
  // Block bbox is x:0..400, y:0..100 -> centre (200, 50).
  const c = viewCenter();
  expect(c.x).toBeCloseTo(200, 3);
  expect(c.y).toBeCloseTo(50, 3);
});
//=============================================================================
test('frames wire routing points that extend beyond the blocks', () => {
  mockCanvas(800, 600);
  // One small block near the origin; a wire loops far above and below it.
  state.blocks.set(1, { x: 0, y: 0, width: 80, height: 80 } as any);
  state.connections = [
    {
      points: [
        { x: 0, y: -200 },
        { x: 0, y: 500 },
      ],
    },
  ] as any;
  fitToDiagram();
  // Combined bbox y spans -200..500 -> centre y = 150, not the block-only 40.
  const c = viewCenter();
  expect(c.y).toBeCloseTo(150, 3);
  const vb = lastViewBox();
  // The whole vertical extent (with padding) must sit inside the viewBox.
  expect(vb.y).toBeLessThanOrEqual(-200);
  expect(vb.y + vb.h).toBeGreaterThanOrEqual(500);
});
//=============================================================================
test('also accepts wire points stored as [x, y] tuples', () => {
  mockCanvas(800, 600);
  state.blocks.set(1, { x: 0, y: 0, width: 80, height: 80 } as any);
  state.connections = [
    {
      points: [
        [600, 0],
        [800, 0],
      ],
    },
  ] as any;
  fitToDiagram();
  const vb = lastViewBox();
  expect(vb.x + vb.w).toBeGreaterThanOrEqual(800);
});
//=============================================================================
test('a large model zooms out below the old 0.1 floor so it fully fits', () => {
  mockCanvas(800, 600);
  // ~20000 wide model: a fixed 0.1 zoom floor would give an 8000 wide viewBox and clip it.
  state.blocks.set(1, { x: 0, y: 0, width: 100, height: 100 } as any);
  state.blocks.set(2, { x: 20000, y: 0, width: 100, height: 100 } as any);
  fitToDiagram();
  const scale = setZoomScale.mock.calls[setZoomScale.mock.calls.length - 1][0];
  expect(scale).toBeLessThan(0.1);
  const vb = lastViewBox();
  // Model spans 0..20100; the viewBox must contain it.
  expect(vb.x).toBeLessThanOrEqual(0);
  expect(vb.x + vb.w).toBeGreaterThanOrEqual(20100);
});
//=============================================================================
test('never magnifies a tiny model past 3x', () => {
  mockCanvas(800, 600);
  state.blocks.set(1, { x: 0, y: 0, width: 10, height: 10 } as any);
  fitToDiagram();
  const scale = setZoomScale.mock.calls[setZoomScale.mock.calls.length - 1][0];
  expect(scale).toBeLessThanOrEqual(3);
});
//=============================================================================
test('ignores a block with a non-finite size instead of freezing on NaN', () => {
  mockCanvas(800, 600);
  state.blocks.set(1, { x: 0, y: 0, width: 100, height: 100 } as any);
  // Unresolved template: width/height undefined -> Number(...) is NaN.
  state.blocks.set(2, { x: 200, y: 50, width: undefined, height: undefined } as any);
  fitToDiagram();
  const vb = lastViewBox();
  expect(Number.isFinite(vb.x)).toBe(true);
  expect(Number.isFinite(vb.y)).toBe(true);
  expect(Number.isFinite(vb.w)).toBe(true);
  expect(Number.isFinite(vb.h)).toBe(true);
});
//=============================================================================
test('falls back to initViewBox when no block has finite coordinates', () => {
  mockCanvas(800, 600);
  state.blocks.set(1, { x: NaN, y: NaN, width: NaN, height: NaN } as any);
  fitToDiagram();
  expect(initViewBox).toHaveBeenCalledTimes(1);
  expect(setViewBox).not.toHaveBeenCalled();
});
//=============================================================================
export {};
//=============================================================================
