//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getViewportSize } from './event-handlers.js';
//=============================================================================
describe('event-handlers getViewportSize', () => {
  beforeEach(() => {
    const existing = document.getElementById('canvas');
    if (existing && existing.parentNode) existing.parentNode.removeChild(existing);
  });

  test('returns default when no canvas present', () => {
    const size = getViewportSize();
    expect(size).toEqual({ w: 800, h: 600 });
  });

  test('returns canvas bounding rect when canvas present', () => {
    const canvas = document.createElement('div');
    canvas.id = 'canvas';
    canvas.getBoundingClientRect = () =>
      ({ width: 640, height: 480, top: 0, left: 0, right: 0, bottom: 0 } as any);
    document.body.appendChild(canvas);
    const size = getViewportSize();
    expect(size).toEqual({ w: 640, h: 480 });
  });
});
//=============================================================================
