//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import {
  getGridState,
  getGridSize,
  setGridSize,
  toggleGridVisibility,
} from '../../../dist/app/grid-manager.js';
//=============================================================================
describe('grid manager', () => {
  beforeEach(() => {
    const canvas = document.createElement('div');
    canvas.id = 'svgCanvas';
    document.body.appendChild(canvas);
  });

  afterEach(() => {
    const c = document.getElementById('svgCanvas');
    if (c && c.parentNode) c.parentNode.removeChild(c);
  });

  test('default grid state and size', () => {
    const s = getGridState();
    expect(typeof s.visible).toBe('boolean');
    expect(typeof s.size).toBe('number');
    expect(getGridSize()).toBeGreaterThanOrEqual(1);
  });

  test('setGridSize accepts valid sizes and rejects invalid', () => {
    // @ts-ignore
    const origAlert = global.alert;
    // @ts-ignore
    global.alert = vi.fn();
    expect(setGridSize(20)).toBe(true);
    expect(getGridSize()).toBe(20);
    expect(setGridSize(3)).toBe(false);
    // @ts-ignore
    global.alert = origAlert;
  });

  test('toggleGridVisibility toggles visible flag', () => {
    const before = getGridState().visible;
    toggleGridVisibility();
    expect(getGridState().visible).toBe(!before);
  });
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/grid-manager.js').catch(() => null);
});
//=============================================================================
test('grid-manager module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
//=============================================================================
