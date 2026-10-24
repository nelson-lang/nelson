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
import { makeWireCornerHandlers } from './wire/renderer-wire-corner';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeCtx = (ext: any = {}) => ({
  state: { connections: [], autoRoute: false },
  wireCornerLayer: document.createElementNS(ns, 'g'),
  wireJunctionLayer: document.createElementNS(ns, 'g'),
  ...ext,
});
//=============================================================================
describe('clearWireCornerHandles', () => {
  test('removes all children from wireCornerLayer', () => {
    const ctx = makeCtx();
    ctx.wireCornerLayer.appendChild(document.createElementNS(ns, 'circle'));
    ctx.wireCornerLayer.appendChild(document.createElementNS(ns, 'circle'));
    const { clearWireCornerHandles } = makeWireCornerHandlers(ctx as any);
    clearWireCornerHandles();
    expect(ctx.wireCornerLayer.childNodes.length).toBe(0);
  });
});
//=============================================================================
describe('clearWireJunctionDots', () => {
  test('removes all children from wireJunctionLayer', () => {
    const ctx = makeCtx();
    ctx.wireJunctionLayer.appendChild(document.createElementNS(ns, 'circle'));
    const { clearWireJunctionDots } = makeWireCornerHandlers(ctx as any);
    clearWireJunctionDots();
    expect(ctx.wireJunctionLayer.childNodes.length).toBe(0);
  });
});
//=============================================================================
describe('renderSharedSourceJunctionDots', () => {
  test('no-ops cleanly when connections is empty', () => {
    const ctx = makeCtx({ state: { connections: [], autoRoute: false } });
    const { renderSharedSourceJunctionDots } = makeWireCornerHandlers(ctx as any);
    expect(() => renderSharedSourceJunctionDots()).not.toThrow();
  });
});
//=============================================================================
describe('moveWireCorner', () => {
  test('returns false when conn is null', () => {
    const ctx = makeCtx();
    const C = makeCtx({ dedupePoints: (p: any) => p });
    const { moveWireCorner } = makeWireCornerHandlers(C as any);
    const result = moveWireCorner(null, 1, { x: 5, y: 5 });
    expect(result).toBe(false);
  });

  test('returns false for invalid index (0 or last)', () => {
    const ctx = makeCtx({ dedupePoints: (p: any) => p });
    const { moveWireCorner } = makeWireCornerHandlers(ctx as any);
    const conn: any = {
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 10, y: 10 },
      ],
    };
    const atStart = moveWireCorner(conn, 0, { x: 5, y: 0 });
    const atEnd = moveWireCorner(conn, 2, { x: 5, y: 5 });
    expect(atStart).toBe(false);
    expect(atEnd).toBe(false);
  });

  test('returns false when adjacent segments have same orientation', () => {
    const ctx = makeCtx({
      dedupePoints: (p: any) => p,
      segmentOrientation: (a: any, b: any) => (a.x === b.x ? 'V' : 'H'),
    });
    const { moveWireCorner } = makeWireCornerHandlers(ctx as any);
    const conn: any = {
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 20, y: 0 },
      ],
    };

    const result = moveWireCorner(conn, 1, { x: 10, y: 10 });

    expect(result).toBe(false);
  });

  test('moves H-V corner and updates conn.points', () => {
    const ctx = makeCtx({
      dedupePoints: (p: any) => p,
      removeColinearPoints: (p: any) => p,
      segmentOrientation: (a: any, b: any) => {
        if (a.x === b.x) return 'V';
        if (a.y === b.y) return 'H';
        return null;
      },
    });
    const { moveWireCorner } = makeWireCornerHandlers(ctx as any);
    const conn: any = {
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 10, y: 20 },
      ],
    };

    const result = moveWireCorner(conn, 1, { x: 15, y: 0 });

    expect(result).toBe(true);
    expect(conn.points).toBeDefined();
  });
});
//=============================================================================
describe('updateWireCornerHandles', () => {
  test('does nothing when autoRoute is true', () => {
    const ctx = makeCtx({ state: { connections: [], autoRoute: true } });
    const { updateWireCornerHandles } = makeWireCornerHandlers(ctx as any);

    expect(() => updateWireCornerHandles()).not.toThrow();
    expect(ctx.wireCornerLayer.childNodes.length).toBe(0);
  });

  test('does nothing when no selectedConnection', () => {
    const ctx = makeCtx({ state: { connections: [], autoRoute: false, selectedConnection: null } });
    const { updateWireCornerHandles } = makeWireCornerHandlers(ctx as any);

    updateWireCornerHandles();

    expect(ctx.wireCornerLayer.childNodes.length).toBe(0);
  });
});
//=============================================================================
