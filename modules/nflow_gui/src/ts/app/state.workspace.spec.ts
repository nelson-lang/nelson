//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { setUpdateViewBoxWithAnchor, getUpdateViewBoxWithAnchor } from './state';
import {
  ensureLeftMargin,
  setViewBox,
  getViewBox,
  setZoomScale,
  updateGrid,
} from './workspace-handlers';
import { state } from './state';
import { beforeEach, test, expect } from 'vitest';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
  state.blocks.clear();
});
//=============================================================================
test('set/get UpdateViewBoxWithAnchor stores and returns function', () => {
  let called = false;
  setUpdateViewBoxWithAnchor(() => {
    called = true;
  });
  const fn = getUpdateViewBoxWithAnchor();
  expect(typeof fn).toBe('function');
  if (fn) fn({});
  expect(called).toBe(true);
});
//=============================================================================
test('updateGrid sets canvas CSS variables for grid size and offsets', () => {
  const canvas = document.createElement('div');
  document.body.appendChild(canvas);
  updateGrid(canvas as HTMLElement, 2, { x: 0, y: 0, w: 100, h: 100 });
  expect(canvas.style.getPropertyValue('--grid-size')).toBe('10px');
  expect(canvas.style.getPropertyValue('--grid-offset-x')).toBe('0px');
  expect(canvas.style.getPropertyValue('--grid-offset-y')).toBe('0px');
});
//=============================================================================
test('ensureLeftMargin moves viewBox when blocks too close to left edge', () => {
  state.blocks.set(1, { id: 1, x: 50 });
  setZoomScale(1);
  setViewBox({ x: 0, y: 0, w: 200, h: 100 });
  ensureLeftMargin(200, 0);
  const vb = getViewBox();
  // desired new x = minX - desiredPx/zoomScale = 50 - 200 = -150
  expect(vb.x).toBe(-150);
});
//=============================================================================
export {};
//=============================================================================
