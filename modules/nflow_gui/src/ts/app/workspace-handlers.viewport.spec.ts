//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, describe, test, expect } from 'vitest';
import { getViewportSize, setSvg, FALLBACK_VIEWPORT } from './workspace-handlers.js';
//=============================================================================
// getViewportSize is the single source of truth for fit-to-diagram, canvas
// events and the zoom handlers. A real canvas size passes through unchanged;
// a missing or zero-size canvas gives a non-zero fallback viewport.
describe('workspace-handlers getViewportSize', () => {
  const makeCanvas = (width: number, height: number): void => {
    const canvas = document.createElement('div');
    canvas.id = 'canvas';
    canvas.getBoundingClientRect = () =>
      ({ width, height, top: 0, left: 0, right: 0, bottom: 0, x: 0, y: 0 } as DOMRect);
    document.body.appendChild(canvas);
  };

  beforeEach(() => {
    document.body.innerHTML = '';
    setSvg(null);
  });

  test('reads the #canvas bounding rect when it has a real size', () => {
    makeCanvas(640, 480);
    expect(getViewportSize()).toEqual({ w: 640, h: 480 });
  });

  test('falls back to a shared non-zero default (never a 1px viewport) with no canvas', () => {
    expect(getViewportSize()).toEqual({ w: FALLBACK_VIEWPORT.w, h: FALLBACK_VIEWPORT.h });
    expect(FALLBACK_VIEWPORT.w).toBeGreaterThan(1);
    expect(FALLBACK_VIEWPORT.h).toBeGreaterThan(1);
  });

  test('ignores a zero-size canvas rather than returning 0x0', () => {
    makeCanvas(0, 0);
    expect(getViewportSize()).toEqual({ w: FALLBACK_VIEWPORT.w, h: FALLBACK_VIEWPORT.h });
  });

  test('uses the svg client size before the fixed default when the canvas is unusable', () => {
    setSvg({ clientWidth: 500, clientHeight: 400 } as unknown as SVGSVGElement);
    expect(getViewportSize()).toEqual({ w: 500, h: 400 });
  });
});
//=============================================================================
