//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// pure helpers of the editor debugger: marker classes, paused line, line moves on save
//=============================================================================
import type * as Monaco from 'monaco-editor';
import type { BreakpointInfo, DebuggerState } from '../rpc/types';
import { t, tf } from '../i18n';
//=============================================================================
// a breakpoint as the editor tracks it: evaluator line (0 while pending) and decoration id
export interface EditorBreakpoint {
  decorationId: string;
  evaluatorLine: number;
  enabled: boolean;
  condition: string;
}
//=============================================================================
export type BreakpointMarkerState = 'active' | 'disabled' | 'invalid' | 'conditional';
//=============================================================================
// red active, light gray disabled, dark gray while unsaved, orange conditional
export function breakpointMarkerState(
  bp: { enabled: boolean; condition: string; evaluatorLine?: number },
  dirty: boolean
): BreakpointMarkerState {
  if (dirty || bp.evaluatorLine === 0) return 'invalid';
  if (!bp.enabled) return 'disabled';
  if (bp.condition) return 'conditional';
  return 'active';
}
//=============================================================================
export function breakpointGlyphClass(state: BreakpointMarkerState): string {
  switch (state) {
    case 'disabled':
      return 'bp-glyph bp-glyph--disabled';
    case 'invalid':
      return 'bp-glyph bp-glyph--invalid';
    case 'conditional':
      return 'bp-glyph bp-glyph--conditional';
    default:
      return 'bp-glyph';
  }
}
//=============================================================================
export function breakpointHoverText(
  bp: { line: number; enabled: boolean; condition: string },
  state: BreakpointMarkerState
): string {
  const parts = [
    bp.enabled
      ? tf('Breakpoint on line %1', bp.line)
      : tf('Breakpoint on line %1 (disabled)', bp.line),
  ];
  if (bp.condition) parts.push(tf('Condition: %1', bp.condition));
  if (state === 'invalid') parts.push(t('Invalid until the file is saved.'));
  return parts.join('\n');
}
//=============================================================================
export function breakpointDecoration(
  monaco: typeof Monaco,
  line: number,
  bp: { enabled: boolean; condition: string },
  state: BreakpointMarkerState
): Monaco.editor.IModelDeltaDecoration {
  return {
    range: new monaco.Range(line, 1, line, 1),
    options: {
      isWholeLine: false,
      glyphMarginClassName: breakpointGlyphClass(state),
      glyphMarginHoverMessage: { value: breakpointHoverText({ line, ...bp }, state) },
      stickiness: monaco.editor.TrackedRangeStickiness.NeverGrowsWhenTypingAtEdges,
    },
  };
}
//=============================================================================
// one spelling for a path, whatever the separators and the case the engine or the user wrote
export function normalizePathKey(path: string): string {
  return path.replace(/\\/g, '/').toLowerCase();
}
//=============================================================================
// Same-path test tolerant to the separators and the case of the two sides.
export function samePath(a: string, b: string): boolean {
  const na = normalizePathKey(a);
  const nb = normalizePathKey(b);
  return na.endsWith(nb) || nb.endsWith(na);
}
//=============================================================================
// The paused line of this file, or null when the debugger is paused elsewhere.
export function pausedLineFor(state: DebuggerState | null, path: string): number | null {
  if (!state || !state.paused || !state.currentFile || !state.currentLine) return null;
  return samePath(path, state.currentFile) ? state.currentLine : null;
}
//=============================================================================
export function debugLineDecorations(
  monaco: typeof Monaco,
  state: DebuggerState | null,
  path: string,
  lineMaxColumn = 1
): Monaco.editor.IModelDeltaDecoration[] {
  const line = pausedLineFor(state, path);
  if (!line || !state) return [];
  const isError = state.reason === 'error';
  const hover = isError
    ? `Paused on error at line ${line}\n${state.errorMessage}`
    : `Paused at line ${line}`;
  const decorations: Monaco.editor.IModelDeltaDecoration[] = [
    {
      range: new monaco.Range(line, 1, line, 1),
      options: {
        isWholeLine: true,
        className: isError ? 'debug-current-line debug-current-line--error' : 'debug-current-line',
        glyphMarginClassName: 'debug-current-glyph',
        glyphMarginHoverMessage: { value: hover },
      },
    },
  ];
  // several statements on the line: a caret marks the one about to run
  const column = state.currentColumn;
  if (column > 1 && column < lineMaxColumn) {
    decorations.push({
      range: new monaco.Range(line, column, line, column + 1),
      options: { inlineClassName: 'debug-current-statement' },
    });
  }
  return decorations;
}
//=============================================================================
// expression of a data tip: the identifier or the whole dotted path (b in s.a.b gives s.a.b)
export function dataTipExpression(
  lineText: string,
  word: { word: string; startColumn: number; endColumn: number }
): string | null {
  if (!/^[A-Za-z_]\w*$/.test(word.word)) return null;
  let start = word.startColumn - 1;
  while (start >= 2 && lineText[start - 1] === '.') {
    const m = /([A-Za-z_]\w*)$/.exec(lineText.slice(0, start - 1));
    if (!m) break;
    start -= m[0].length + 1;
  }
  const expr = lineText.slice(start, word.endColumn - 1);
  return expr.trim() ? expr : null;
}
//=============================================================================
// moves to push to the evaluator on save: each known breakpoint now on another line
export function breakpointMoves(
  items: EditorBreakpoint[],
  currentLineOf: (decorationId: string) => number | null
): { from: number; to: number }[] {
  const moves: { from: number; to: number }[] = [];
  for (const item of items) {
    if (item.evaluatorLine === 0) continue;
    const line = currentLineOf(item.decorationId);
    if (line && line !== item.evaluatorLine) {
      moves.push({ from: item.evaluatorLine, to: line });
    }
  }
  return moves;
}
//=============================================================================
// Breakpoints added while the file was modified: they are created on save.
export function pendingBreakpoints(
  items: EditorBreakpoint[],
  currentLineOf: (decorationId: string) => number | null
): { line: number; enabled: boolean; condition: string }[] {
  const pending: { line: number; enabled: boolean; condition: string }[] = [];
  for (const item of items) {
    if (item.evaluatorLine !== 0) continue;
    const line = currentLineOf(item.decorationId);
    if (line) pending.push({ line, enabled: item.enabled, condition: item.condition });
  }
  return pending;
}
//=============================================================================
export function toEditorBreakpoints(
  breakpoints: BreakpointInfo[],
  decorationIds: string[]
): EditorBreakpoint[] {
  return breakpoints.map((bp, index) => ({
    decorationId: decorationIds[index] ?? '',
    evaluatorLine: bp.line,
    enabled: bp.enabled,
    condition: bp.condition ?? '',
  }));
}
//=============================================================================
