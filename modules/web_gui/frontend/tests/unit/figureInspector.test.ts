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
import { parseFigureProperty } from '../../src/panels/FigureInspector';
import { mockFigureAction } from '../../src/rpc/mockFigureActions';
import { figureStateArrays } from '../../src/rpc/figureTypes';
import type { FigureInspectorState } from '../../src/rpc/figureTypes';
//=============================================================================
describe('figure property editing', () => {
  it('preserves text and parses finite scalar and vector values', () => {
    expect(parseFigureProperty("A 'quoted' title", 'text')).toBe("A 'quoted' title");
    expect(parseFigureProperty('2.5', 'number', 1)).toBe(2.5);
    expect(parseFigureProperty('[-1, 2e3]', 'vector', 2)).toEqual([-1, 2000]);
    expect(parseFigureProperty('0 0.5 1', 'color', 3)).toEqual([0, 0.5, 1]);
    expect(parseFigureProperty('#2266ad', 'color')).toBe('#2266ad');
    expect(parseFigureProperty('none', 'color')).toBe('none');
  });
  it('rejects incomplete, non-finite and incorrectly sized edits', () => {
    expect(() => parseFigureProperty('', 'number', 1)).toThrow();
    expect(() => parseFigureProperty('NaN', 'number', 1)).toThrow();
    expect(() => parseFigureProperty('1, Infinity', 'vector', 2)).toThrow();
    expect(() => parseFigureProperty('1, 2, 3', 'vector', 2)).toThrow();
  });
  it('normalizes scalar and empty collections returned by the engine JSON encoder', () => {
    const state = {
      axes: { handle: 42, objects: { handle: 77 } },
    } as unknown as FigureInspectorState;
    expect(figureStateArrays(state).axes[0].objects[0].handle).toBe(77);
    expect(figureStateArrays({ axes: [] } as unknown as FigureInspectorState).axes).toEqual([]);
  });
  it('keeps a named figure unchanged during a mock state read', () => {
    const figure = { handle: 0, title: 'Control demo', kind: 'line' };
    const result = mockFigureAction(
      figure,
      { action: 'state' },
      { handle: 0, width: 640, height: 480, primitives: [] }
    );
    expect(figure.title).toBe('Control demo');
    expect(result).toMatchObject({ state: { name: 'Control demo', numberTitle: 'off' } });
  });
});
//=============================================================================
