//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { GRID_SIZE } from './geometry.js';
//=============================================================================
/** Minimal block description required by the alignment functions. */
export type AlignBlock = {
  x: number;
  y: number;
  width: number;
  height: number;
};
//=============================================================================
/** All supported alignment/distribution actions. */
export type AlignAction =
  | 'alignLeft'
  | 'alignRight'
  | 'alignCenterH'
  | 'alignTop'
  | 'alignBottom'
  | 'alignMiddleV'
  | 'distributeH'
  | 'distributeV'
  | 'snapToGrid';
/** Distribute actions require at least 3 blocks; all others require at least 2. */
export function minBlocksRequired(action: AlignAction): number {
  return action === 'distributeH' || action === 'distributeV' ? 3 : 2;
}
/**
 * Apply an alignment/distribution action to an array of blocks **in place**.
 * Only `x` and `y` (and never `width`/`height`) are mutated.
 * Returns the same array so it can be chained.
 *
 * No DOM, no state, no renderer: purely geometric math.
 */
export function applyAlignment<T extends AlignBlock>(blocks: T[], action: AlignAction): T[] {
  if (blocks.length < minBlocksRequired(action)) return blocks;

  switch (action) {
    case 'alignLeft': {
      const minX = Math.min(...blocks.map((b) => b.x));
      blocks.forEach((b) => {
        b.x = minX;
      });
      break;
    }
    case 'alignRight': {
      const maxRight = Math.max(...blocks.map((b) => b.x + b.width));
      blocks.forEach((b) => {
        b.x = maxRight - b.width;
      });
      break;
    }
    case 'alignCenterH': {
      const avgCX = blocks.reduce((sum, b) => sum + b.x + b.width / 2, 0) / blocks.length;
      blocks.forEach((b) => {
        b.x = avgCX - b.width / 2;
      });
      break;
    }
    case 'alignTop': {
      const minY = Math.min(...blocks.map((b) => b.y));
      blocks.forEach((b) => {
        b.y = minY;
      });
      break;
    }
    case 'alignBottom': {
      const maxBottom = Math.max(...blocks.map((b) => b.y + b.height));
      blocks.forEach((b) => {
        b.y = maxBottom - b.height;
      });
      break;
    }
    case 'alignMiddleV': {
      const avgCY = blocks.reduce((sum, b) => sum + b.y + b.height / 2, 0) / blocks.length;
      blocks.forEach((b) => {
        b.y = avgCY - b.height / 2;
      });
      break;
    }
    case 'distributeH': {
      const sorted = [...blocks].sort((a, b) => a.x - b.x);
      const leftmost = sorted[0].x;
      const rightmost = sorted[sorted.length - 1].x + sorted[sorted.length - 1].width;
      const totalW = sorted.reduce((s, b) => s + b.width, 0);
      const gap = (rightmost - leftmost - totalW) / (sorted.length - 1);
      let cursor = leftmost;
      sorted.forEach((b) => {
        b.x = cursor;
        cursor += b.width + gap;
      });
      break;
    }
    case 'distributeV': {
      const sorted = [...blocks].sort((a, b) => a.y - b.y);
      const topmost = sorted[0].y;
      const bottommost = sorted[sorted.length - 1].y + sorted[sorted.length - 1].height;
      const totalH = sorted.reduce((s, b) => s + b.height, 0);
      const gap = (bottommost - topmost - totalH) / (sorted.length - 1);
      let cursor = topmost;
      sorted.forEach((b) => {
        b.y = cursor;
        cursor += b.height + gap;
      });
      break;
    }
    case 'snapToGrid': {
      blocks.forEach((b) => {
        b.x = Math.round(b.x / GRID_SIZE) * GRID_SIZE;
        b.y = Math.round(b.y / GRID_SIZE) * GRID_SIZE;
      });
      break;
    }
  }

  return blocks;
}
//=============================================================================
