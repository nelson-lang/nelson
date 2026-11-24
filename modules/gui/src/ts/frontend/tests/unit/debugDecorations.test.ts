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
import {
  normalizePathKey,
  breakpointGlyphClass,
  breakpointMarkerState,
  breakpointMoves,
  dataTipExpression,
  debugLineDecorations,
  pausedLineFor,
  pendingBreakpoints,
  samePath,
  type EditorBreakpoint,
} from '../../src/editor/debugDecorations';
import type { DebuggerState } from '../../src/rpc/types';
//=============================================================================
// Stand-in for the monaco namespace used by the decoration builders.
class FakeRange {
  constructor(
    public startLineNumber: number,
    public startColumn: number,
    public endLineNumber: number,
    public endColumn: number
  ) {}
}
const monaco = {
  Range: FakeRange,
  editor: { TrackedRangeStickiness: { NeverGrowsWhenTypingAtEdges: 1 } },
} as unknown as typeof import('monaco-editor');
//=============================================================================
const paused: DebuggerState = {
  running: true,
  paused: true,
  currentFile: 'C:\\work\\demo.m',
  currentLine: 4,
  currentColumn: 0,
  function: 'demo',
  reason: 'breakpoint',
  errorIdentifier: '',
  errorMessage: '',
  canStep: true,
  stack: [{ file: 'C:\\work\\demo.m', line: 4, function: 'demo' }],
  revision: 3,
  breakpointsRevision: 1,
};
//=============================================================================
describe('breakpoint markers', () => {
  it('maps the breakpoint attributes to the marker states', () => {
    expect(breakpointMarkerState({ enabled: true, condition: '', evaluatorLine: 3 }, false)).toBe(
      'active'
    );
    expect(breakpointMarkerState({ enabled: false, condition: '', evaluatorLine: 3 }, false)).toBe(
      'disabled'
    );
    expect(
      breakpointMarkerState({ enabled: true, condition: 'x > 2', evaluatorLine: 3 }, false)
    ).toBe('conditional');
    // unsaved changes or a pending breakpoint: dark gray whatever the attributes
    expect(
      breakpointMarkerState({ enabled: true, condition: 'x > 2', evaluatorLine: 3 }, true)
    ).toBe('invalid');
    expect(breakpointMarkerState({ enabled: true, condition: '', evaluatorLine: 0 }, false)).toBe(
      'invalid'
    );
  });

  it('gives each state its gutter class', () => {
    expect(breakpointGlyphClass('active')).toBe('bp-glyph');
    expect(breakpointGlyphClass('disabled')).toBe('bp-glyph bp-glyph--disabled');
    expect(breakpointGlyphClass('invalid')).toBe('bp-glyph bp-glyph--invalid');
    expect(breakpointGlyphClass('conditional')).toBe('bp-glyph bp-glyph--conditional');
  });
});
//=============================================================================
describe('breakpoints following the edits', () => {
  const items: EditorBreakpoint[] = [
    { decorationId: 'a', evaluatorLine: 3, enabled: true, condition: '' },
    { decorationId: 'b', evaluatorLine: 7, enabled: false, condition: 'k == 2' },
    { decorationId: 'c', evaluatorLine: 0, enabled: true, condition: 'x > 1' },
  ];
  const lines: Record<string, number> = { a: 3, b: 9, c: 12 };
  const currentLineOf = (id: string) => lines[id] ?? null;

  it('reports only the breakpoints whose line changed', () => {
    expect(breakpointMoves(items, currentLineOf)).toEqual([{ from: 7, to: 9 }]);
  });

  it('lists the breakpoints added while the file was modified', () => {
    expect(pendingBreakpoints(items, currentLineOf)).toEqual([
      { line: 12, enabled: true, condition: 'x > 1' },
    ]);
  });

  it('drops a pending breakpoint whose decoration disappeared', () => {
    expect(pendingBreakpoints(items, () => null)).toEqual([]);
  });
});
//=============================================================================
describe('paused line decorations', () => {
  it('matches the paused file whatever the separators and the case', () => {
    expect(samePath('c:/work/DEMO.m', 'C:\\work\\demo.m')).toBe(true);
    expect(pausedLineFor(paused, '/work/demo.m')).toBe(4);
    expect(pausedLineFor(paused, '/work/other.m')).toBeNull();
    expect(pausedLineFor({ ...paused, paused: false }, '/work/demo.m')).toBeNull();
  });

  it('paints the green line, and the red variant on an error stop', () => {
    const plain = debugLineDecorations(monaco, paused, 'demo.m');
    expect(plain).toHaveLength(1);
    expect(plain[0].options.className).toBe('debug-current-line');
    expect(plain[0].options.glyphMarginClassName).toBe('debug-current-glyph');
    const error = debugLineDecorations(
      monaco,
      { ...paused, reason: 'error', errorMessage: 'Undefined variable: z', canStep: false },
      'demo.m'
    );
    expect(error[0].options.className).toContain('debug-current-line--error');
    expect(error[0].options.glyphMarginHoverMessage).toEqual({
      value: 'Paused on error at line 4\nUndefined variable: z',
    });
  });

  it('marks the statement about to run on a multi-statement line', () => {
    const decorations = debugLineDecorations(monaco, { ...paused, currentColumn: 8 }, 'demo.m', 20);
    expect(decorations).toHaveLength(2);
    expect(decorations[1].range).toMatchObject({
      startLineNumber: 4,
      startColumn: 8,
      endColumn: 9,
    });
    expect(decorations[1].options.inlineClassName).toBe('debug-current-statement');
    // no caret at the start of the line or past its end
    expect(
      debugLineDecorations(monaco, { ...paused, currentColumn: 1 }, 'demo.m', 20)
    ).toHaveLength(1);
    expect(
      debugLineDecorations(monaco, { ...paused, currentColumn: 25 }, 'demo.m', 20)
    ).toHaveLength(1);
  });
});
//=============================================================================
describe('data tip expressions', () => {
  it('evaluates the whole dotted path under the pointer', () => {
    // columns are 1-based: in "x = s.a.b;" the b sits at column 9
    expect(dataTipExpression('x = s.a.b;', { word: 'b', startColumn: 9, endColumn: 10 })).toBe(
      's.a.b'
    );
    expect(dataTipExpression('x = s.a.b;', { word: 'a', startColumn: 7, endColumn: 8 })).toBe(
      's.a'
    );
    expect(dataTipExpression('x = s.a.b;', { word: 's', startColumn: 5, endColumn: 6 })).toBe('s');
  });

  it('ignores numbers and keeps a plain identifier as is', () => {
    expect(dataTipExpression('y = 42;', { word: '42', startColumn: 5, endColumn: 7 })).toBeNull();
    expect(dataTipExpression('y = x + 1;', { word: 'x', startColumn: 5, endColumn: 6 })).toBe('x');
  });
});
//=============================================================================

describe('normalizePathKey', () => {
  it('gives one spelling to separators and case', () => {
    expect(normalizePathKey('D:\\Work\\A.m')).toBe('d:/work/a.m');
    expect(normalizePathKey('d:/work/a.m')).toBe('d:/work/a.m');
  });
});
