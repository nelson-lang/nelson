//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from 'vitest';
import { clampZoomScale, MIN_ZOOM_SCALE, MAX_ZOOM_SCALE } from './workspace-handlers.js';
//=============================================================================
test('leaves an in-range scale untouched', () => {
  expect(clampZoomScale(1)).toBe(1);
  expect(clampZoomScale(0.5)).toBe(0.5);
});
//=============================================================================
test('caps zoom-in at MAX_ZOOM_SCALE', () => {
  expect(clampZoomScale(100)).toBe(MAX_ZOOM_SCALE);
  expect(MAX_ZOOM_SCALE).toBe(3);
});
//=============================================================================
test('floors zoom-out at MIN_ZOOM_SCALE', () => {
  expect(clampZoomScale(0)).toBe(MIN_ZOOM_SCALE);
  expect(clampZoomScale(-5)).toBe(MIN_ZOOM_SCALE);
});
//=============================================================================
test('allows zooming out below the old 0.1 floor so large models fit', () => {
  expect(MIN_ZOOM_SCALE).toBeLessThan(0.1);
  expect(clampZoomScale(0.04)).toBeCloseTo(0.04, 5);
});
//=============================================================================
export {};
//=============================================================================
