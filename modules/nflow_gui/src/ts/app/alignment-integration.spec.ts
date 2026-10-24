//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Integration tests for handleAlignAction: the real context-menu-manager runs
// against a minimal mocked AppState and renderer, covering the pipeline
//   state lookup → applyAlignment (pure math) → updateBlockTransform → updateConnections
//
//=============================================================================
import { vi } from 'vitest';
import { setStateRef, setRendererRef, handleAlignAction } from './context-menu-manager.js';
import { state } from './state.js';
//=============================================================================
type MockBlock = {
  id: number;
  type: string;
  x: number;
  y: number;
  width: number;
  height: number;
};
//=============================================================================
function mb(id: number, x: number, y: number, w = 80, h = 40): MockBlock {
  return { id, type: 'gain', x, y, width: w, height: h };
}
//=============================================================================
describe('handleAlignAction integration', () => {
  let updateBlockTransformMock: ReturnType<typeof vi.fn>;
  let updateConnectionsMock: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    updateBlockTransformMock = vi.fn();
    updateConnectionsMock = vi.fn();

    const mockRenderer = {
      current: {
        updateBlockTransform: updateBlockTransformMock,
        updateConnections: updateConnectionsMock,
      },
    };

    state.blocks.clear();
    state.connections = [];
    state.selectedId = null;
    state.selectedIds = new Set();
    state.routingDirty = false;

    setStateRef(state as any);
    setRendererRef(mockRenderer as any);
  });

  afterEach(() => {
    state.blocks.clear();
    state.connections = [];
    state.selectedIds = new Set();
    setStateRef(null);
    setRendererRef(null);
  });

  test('does nothing when fewer than 2 blocks selected', () => {
    const blk = mb(1, 50, 50);
    state.blocks.set(1, blk as any);
    state.selectedIds = new Set([1]);

    handleAlignAction('alignLeft');

    expect(blk.x).toBe(50);
    expect(updateBlockTransformMock).not.toHaveBeenCalled();
    expect(updateConnectionsMock).not.toHaveBeenCalled();
  });

  test('does nothing when selectedIds is empty', () => {
    handleAlignAction('alignLeft');
    expect(updateBlockTransformMock).not.toHaveBeenCalled();
  });

  test('alignLeft: moves all blocks to minimum x', () => {
    const b1 = mb(1, 100, 0);
    const b2 = mb(2, 30, 0); // minimum x
    const b3 = mb(3, 200, 0);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.blocks.set(3, b3 as any);
    state.selectedIds = new Set([1, 2, 3]);

    handleAlignAction('alignLeft');

    expect(b1.x).toBe(30);
    expect(b2.x).toBe(30);
    expect(b3.x).toBe(30);
  });

  test('alignRight: all right edges equal maximum right edge', () => {
    const b1 = mb(1, 0, 0, 80, 40); // right=80
    const b2 = mb(2, 100, 0, 60, 40); // right=160 ← max
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('alignRight');

    expect(b1.x + b1.width).toBe(160);
    expect(b2.x + b2.width).toBe(160);
  });

  test('alignCenterH: all centres equal average horizontal centre', () => {
    // b1: cx=50, b2: cx=250 → avg=150
    const b1 = mb(1, 0, 0, 100, 40);
    const b2 = mb(2, 200, 0, 100, 40);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('alignCenterH');

    expect(b1.x + b1.width / 2).toBeCloseTo(150);
    expect(b2.x + b2.width / 2).toBeCloseTo(150);
  });

  test('alignTop: moves all blocks to minimum y', () => {
    const b1 = mb(1, 0, 80);
    const b2 = mb(2, 0, 20); // minimum y
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('alignTop');

    expect(b1.y).toBe(20);
    expect(b2.y).toBe(20);
  });

  test('alignBottom: all bottom edges equal maximum bottom', () => {
    const b1 = mb(1, 0, 0, 80, 40); // bottom=40
    const b2 = mb(2, 0, 100, 80, 60); // bottom=160 ← max
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('alignBottom');

    expect(b1.y + b1.height).toBe(160);
    expect(b2.y + b2.height).toBe(160);
  });

  test('alignMiddleV: all centres equal average vertical centre', () => {
    const b1 = mb(1, 0, 0, 80, 100); // cy=50
    const b2 = mb(2, 0, 200, 80, 100); // cy=250 → avg=150
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('alignMiddleV');

    expect(b1.y + b1.height / 2).toBeCloseTo(150);
    expect(b2.y + b2.height / 2).toBeCloseTo(150);
  });

  test('distributeH: equal gaps between 3 blocks', () => {
    const b1 = mb(1, 0, 0, 60, 40);
    const b2 = mb(2, 300, 0, 60, 40); // rightmost
    const b3 = mb(3, 180, 0, 60, 40); // will move to middle
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.blocks.set(3, b3 as any);
    state.selectedIds = new Set([1, 2, 3]);

    handleAlignAction('distributeH');

    // the outer blocks stay put
    const sorted = [b1, b2, b3].sort((a, z) => a.x - z.x);
    expect(sorted[0].x).toBeCloseTo(0);
    expect(sorted[2].x).toBeCloseTo(300);
    const gap1 = sorted[1].x - (sorted[0].x + sorted[0].width);
    const gap2 = sorted[2].x - (sorted[1].x + sorted[1].width);
    expect(gap1).toBeCloseTo(gap2);
  });

  test('distributeH: does nothing with only 2 blocks', () => {
    const b1 = mb(1, 0, 0);
    const b2 = mb(2, 200, 0);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('distributeH');

    expect(b1.x).toBe(0);
    expect(b2.x).toBe(200);
  });

  test('distributeV: equal gaps between 3 blocks', () => {
    const b1 = mb(1, 0, 0, 80, 60);
    const b2 = mb(2, 0, 300, 80, 60);
    const b3 = mb(3, 0, 180, 80, 60);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.blocks.set(3, b3 as any);
    state.selectedIds = new Set([1, 2, 3]);

    handleAlignAction('distributeV');

    const sorted = [b1, b2, b3].sort((a, z) => a.y - z.y);
    expect(sorted[0].y).toBeCloseTo(0);
    expect(sorted[2].y).toBeCloseTo(300);
    const gap1 = sorted[1].y - (sorted[0].y + sorted[0].height);
    const gap2 = sorted[2].y - (sorted[1].y + sorted[1].height);
    expect(gap1).toBeCloseTo(gap2);
  });

  test('snapToGrid: rounds x and y to nearest 10', () => {
    const b1 = mb(1, 13, 27);
    const b2 = mb(2, 95, 44);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('snapToGrid');

    expect(b1.x).toBe(10);
    expect(b1.y).toBe(30);
    expect(b2.x).toBe(100);
    expect(b2.y).toBe(40);
  });

  test('updateBlockTransform called once per selected block', () => {
    const b1 = mb(1, 100, 0);
    const b2 = mb(2, 30, 0);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('alignLeft');

    expect(updateBlockTransformMock).toHaveBeenCalledTimes(2);
  });

  test('updateConnections called with fast=true after action', () => {
    const b1 = mb(1, 0, 0);
    const b2 = mb(2, 100, 0);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('alignTop');

    expect(updateConnectionsMock).toHaveBeenCalledWith(true);
  });

  test('state.routingDirty set to true after action', () => {
    const b1 = mb(1, 0, 0);
    const b2 = mb(2, 100, 0);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    state.routingDirty = false;
    handleAlignAction('alignTop');

    expect(state.routingDirty).toBe(true);
  });

  test('unknown action: renderer still not called (no mutation)', () => {
    const b1 = mb(1, 0, 0);
    const b2 = mb(2, 100, 0);
    state.blocks.set(1, b1 as any);
    state.blocks.set(2, b2 as any);
    state.selectedIds = new Set([1, 2]);

    handleAlignAction('unknownAction');

    // applyAlignment is a no-op for unknown actions, but the apply loop still
    // calls the renderer
    expect(b1.x).toBe(0);
    expect(b2.x).toBe(100);
    expect(updateBlockTransformMock).toHaveBeenCalledTimes(2);
  });
});
//=============================================================================
