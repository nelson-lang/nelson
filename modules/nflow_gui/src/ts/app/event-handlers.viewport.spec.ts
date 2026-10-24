//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getViewportSize } from './event-handlers';
import { beforeEach, test, expect } from 'vitest';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('getViewportSize returns defaults when no canvas', () => {
  const sz = getViewportSize();
  expect(sz.w).toBeGreaterThan(0);
  expect(sz.h).toBeGreaterThan(0);
});
//=============================================================================
test('getViewportSize reads canvas bounding rect', () => {
  const canvas = document.createElement('div');
  canvas.setAttribute('id', 'canvas');
  // JSDOM doesn't populate getBoundingClientRect size; set clientWidth/Height
  // but getViewportSize uses getBoundingClientRect, so we mock it
  // @ts-ignore
  canvas.getBoundingClientRect = () => ({ width: 300, height: 200 });
  document.body.appendChild(canvas);
  const sz = getViewportSize();
  expect(sz.w).toBe(300);
  expect(sz.h).toBe(200);
});
//=============================================================================
export {};
//=============================================================================
