//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { makeDebugHandlers } from './core/renderer-debug';
//=============================================================================
const makeCtx = (ext: any = {}) => ({
  state: { connections: [] },
  ...ext,
});
//=============================================================================
describe('writeDebug', () => {
  test('appends text to logEl.textContent', () => {
    const { writeDebug } = makeDebugHandlers(makeCtx() as any);
    const el = document.createElement('pre');
    el.textContent = '';
    writeDebug(el, 'hello');
    expect(el.textContent).toContain('hello');
  });

  test('skips messages starting with [wire', () => {
    const { writeDebug } = makeDebugHandlers(makeCtx() as any);
    const el = document.createElement('pre');
    el.textContent = '';
    writeDebug(el, '[wire something');
    expect(el.textContent).toBe('');
  });

  test('no-ops when logEl is null', () => {
    const { writeDebug } = makeDebugHandlers(makeCtx() as any);
    expect(() => writeDebug(null, 'text')).not.toThrow();
  });
});
//=============================================================================
describe('formatWireIssue', () => {
  test('formats issue string with conn ids and issues joined', () => {
    const { formatWireIssue } = makeDebugHandlers(makeCtx() as any);
    const conn: any = { from: 'src', to: 'dst', points: [] };
    const result = formatWireIssue(conn, ['bad angle', 'crosses block']);
    expect(result).toContain('src->dst');
    expect(result).toContain('bad angle');
    expect(result).toContain('crosses block');
  });
});
//=============================================================================
describe('formatWireDebugInfo', () => {
  test('returns empty or minimal string for conn with no points', () => {
    const { formatWireDebugInfo } = makeDebugHandlers(makeCtx() as any);
    const conn: any = { from: 'a', to: 'b', points: [] };
    const result = formatWireDebugInfo(conn);
    expect(typeof result).toBe('string');
  });

  test('includes stats for conn with points', () => {
    const { formatWireDebugInfo } = makeDebugHandlers(makeCtx() as any);
    const conn: any = {
      from: 'a',
      to: 'b',
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 10, y: 20 },
      ],
    };
    const result = formatWireDebugInfo(conn);
    expect(result).toContain('len=');
  });
});
//=============================================================================
describe('checkWireIssues', () => {
  test('returns true for conn with fewer than 2 points', () => {
    const { checkWireIssues } = makeDebugHandlers(makeCtx() as any);
    const conn: any = { from: 'a', to: 'b', points: [] };
    const logEl = document.createElement('pre');
    logEl.textContent = '';
    const result = checkWireIssues(conn, logEl);
    expect(result).toBe(true);
  });

  test('returns true for conn with diagonal segment', () => {
    const { checkWireIssues } = makeDebugHandlers(makeCtx() as any);
    const conn: any = {
      from: 'a',
      to: 'b',
      points: [
        { x: 0, y: 0 },
        { x: 5, y: 5 },
      ],
    };
    const result = checkWireIssues(conn);
    expect(result).toBe(true);
  });

  test('returns false for orthogonal conn with no issues', () => {
    const checkPortDirections = () => true;
    const segmentHitsAnyBlock = () => false;
    const segmentOverlapsOtherWire = () => false;
    const { checkWireIssues } = makeDebugHandlers(
      makeCtx({ checkPortDirections, segmentHitsAnyBlock, segmentOverlapsOtherWire }) as any
    );
    const conn: any = {
      from: 'a',
      to: 'b',
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 10, y: 20 },
      ],
    };
    const result = checkWireIssues(conn);
    expect(result).toBe(false);
  });
});
//=============================================================================
