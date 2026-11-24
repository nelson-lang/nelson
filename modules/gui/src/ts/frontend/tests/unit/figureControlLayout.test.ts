//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import type { UIControl } from '../../src/rpc/types';
import {
  figureWindowNeedsResize,
  positionFigureControl,
} from '../../src/panels/figureControlLayout';
//=============================================================================
const control = (units?: string): UIControl => ({
  handle: 1,
  style: 'pushbutton',
  text: 'Run',
  units,
  x: 20,
  y: 30,
  w: 100,
  h: 24,
  value: 0,
  min: 0,
  max: 1,
  fontSize: 12,
  enabled: true,
});
//=============================================================================
describe('positionFigureControl', () => {
  it('retains pixel sizes and bottom-left anchors when the drawing surface grows', () => {
    expect(positionFigureControl(control('pixels'), 150)).toMatchObject({
      x: 20,
      y: 180,
      w: 100,
      h: 24,
    });
  });

  it('retains the anchor when the drawing surface shrinks', () => {
    expect(positionFigureControl(control('pixels'), -20).y).toBe(10);
  });

  it('leaves controls converted from relative units unchanged', () => {
    const normalized = control('normalized');
    expect(positionFigureControl(normalized, 150)).toBe(normalized);
  });
});
//=============================================================================
describe('figureWindowNeedsResize', () => {
  it('requests a native resize when a callback constrains the figure size', () => {
    expect(figureWindowNeedsResize(780, 520, 840, 520)).toBe(true);
  });

  it('accepts the canvas once it matches within rounding tolerance', () => {
    expect(figureWindowNeedsResize(780, 520, 781, 519)).toBe(false);
  });
});
//=============================================================================
