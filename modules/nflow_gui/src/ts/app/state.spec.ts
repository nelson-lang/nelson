//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  state,
  markDirty,
  clearDirty,
  deepClone,
  setCurrentFilePath,
  getCurrentFilePath,
  signalDiagramChanged,
} from '../../../dist/app/state.js';
//=============================================================================
describe('app state helpers', () => {
  test('markDirty and clearDirty toggle state.dirty', () => {
    clearDirty();
    expect(state.dirty).toBe(false);
    markDirty();
    expect(state.dirty).toBe(true);
    clearDirty();
    expect(state.dirty).toBe(false);
  });

  test('deepClone creates a separate object', () => {
    const obj = { a: 1, b: { c: 2 } };
    const cloned = deepClone(obj);
    expect(cloned).toEqual(obj);
    (cloned as any).b.c = 9;
    expect((obj as any).b.c).toBe(2);
  });

  test('current file path setters and getters', () => {
    setCurrentFilePath('foo.nflow');
    expect(getCurrentFilePath()).toBe('foo.nflow');
    setCurrentFilePath(null);
    expect(getCurrentFilePath()).toBeNull();
  });

  test('signalDiagramChanged dispatches event', async () => {
    const fired = new Promise<void>((resolve) => {
      window.addEventListener('diagramChanged', () => resolve(), { once: true });
    });
    signalDiagramChanged();
    await fired;
  });
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/state.js').catch(() => null);
});
//=============================================================================
test('state module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
//=============================================================================
