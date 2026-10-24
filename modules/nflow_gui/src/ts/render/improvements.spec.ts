//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test } from 'vitest';
import { makeWireApplyHandlers } from './wire/renderer-wire-apply.js';
import { makeWireCheckHandlers } from './wire/renderer-wire-checks.js';
import { buildFallbackPath } from './wire/wire-paths.js';
import { simplifyOrthogonalPath } from '../router/router.js';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
function makePath() {
  return document.createElementNS('http://www.w3.org/2000/svg', 'path');
}
//=============================================================================
function baseState(blocks: Map<string, any> = new Map(), connections: any[] = []) {
  return { blocks, connections, fastRouting: false, autoRoute: false };
}
//=============================================================================
function baseCtx(state: any, overrides: any = {}) {
  return {
    state,
    isValidStub: () => true,
    simplifyOrthogonalPath: (p: any) => p,
    dedupePoints: (p: any) => p,
    tryShorterOrthogonalPath: (_c: any, p: any) => p,
    applyWireOffsets: (_c: any, p: any) => p,
    buildFallbackPathFromPorts: (_c: any) => [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    buildDragRenderPoints: (_c: any, p: any) => p,
    renderSharedSourceJunctionDots: () => {},
    ...overrides,
  };
}
//=============================================================================
describe('Item 1 - applyWirePaths single pass', () => {
  test('each connection is processed exactly once: fallback assigned once', () => {
    let callCount = 0;
    const conn: any = { from: 'a', to: 'b', points: undefined, path: makePath() };
    const state = baseState(new Map(), [conn]);
    const ctx = baseCtx(state, {
      buildFallbackPathFromPorts: (_c: any) => {
        callCount += 1;
        return [
          { x: 0, y: 0 },
          { x: 10, y: 0 },
        ];
      },
    });
    const h = makeWireApplyHandlers(ctx as any);
    h.applyWirePaths(null, {});
    expect(callCount).toBe(1);
  });

  test('debugRenderPoints matches the path rendered (segment map not stale)', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 40, y: 0 },
    ];
    const conn: any = { from: 'a', to: 'b', points: pts.slice(), path: makePath() };
    const state = baseState(new Map(), [conn]);
    const ctx = baseCtx(state);
    const h = makeWireApplyHandlers(ctx as any);
    h.applyWirePaths(null, {});
    expect(conn.debugRenderPoints).toEqual(pts);
    expect(conn.path.getAttribute('d')).toBeTruthy();
  });
});
//=============================================================================
describe('Item 2 - toIndex default in checkPortDirections / shareSamePort', () => {
  test('checkPortDirections accepts connection with implicit toIndex 0', () => {
    const fromBlock: any = {
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      ports: [{ type: 'out', index: 0, x: 20, y: 10 }],
    };
    const toBlock: any = {
      x: 100,
      y: 0,
      width: 20,
      height: 20,
      ports: [{ type: 'in', index: 0, x: 0, y: 10 }],
    };
    const blocks = new Map<string, any>([
      ['A', fromBlock],
      ['B', toBlock],
    ]);
    const conn: any = {
      from: 'A',
      to: 'B',
      fromIndex: 0,
      // toIndex omitted on purpose: defaults to 0
      points: [
        { x: 20, y: 10 },
        { x: 60, y: 10 },
        { x: 100, y: 10 },
      ],
    };
    const state = baseState(blocks, [conn]);
    const ctx: any = { state };
    const h = makeWireCheckHandlers(ctx);
    const issues: string[] = [];
    const ok = h.checkPortDirections(conn, conn.points, issues);
    // May fail for other reasons (stub direction); it must not fail on the toPort lookup.
    expect(issues.some((i) => i.includes('toPort'))).toBe(false);
  });

  test('shareSamePort treats toIndex undefined same as 0', () => {
    const blocks = new Map();
    const state = baseState(blocks, []);
    const ctx: any = { state };
    const h = makeWireCheckHandlers(ctx);
    const a: any = { from: 'X', to: 'Y', fromIndex: 0, toIndex: undefined };
    const b: any = { from: 'X', to: 'Y', fromIndex: 0, toIndex: 0 };
    expect(h.shareSamePort(a, b)).toBe(true);
  });

  test('shareSamePort does NOT equate toIndex 1 with undefined', () => {
    const blocks = new Map();
    const state = baseState(blocks, []);
    const ctx: any = { state };
    const h = makeWireCheckHandlers(ctx);
    // Different source blocks so the from-side check cannot short-circuit
    const a: any = { from: 'X1', to: 'Y', fromIndex: 0, toIndex: undefined };
    const b: any = { from: 'X2', to: 'Y', fromIndex: 0, toIndex: 1 };
    expect(h.shareSamePort(a, b)).toBe(false);
  });
});
//=============================================================================
describe('Item 3 - labelSource removes marker-end', () => {
  function makeBlocksWithTypes(fromType: string | undefined, toType: string | undefined) {
    const fromBlock: any = {
      type: fromType,
      x: 0,
      y: 0,
      width: 10,
      height: 10,
      ports: [{ type: 'out', index: 0, x: 10, y: 5 }],
    };
    const toBlock: any = {
      type: toType,
      x: 100,
      y: 0,
      width: 10,
      height: 10,
      ports: [{ type: 'in', index: 0, x: 0, y: 5 }],
    };
    return new Map<string, any>([
      ['F', fromBlock],
      ['T', toBlock],
    ]);
  }

  test('normal connection gets marker-end arrow', () => {
    const path = makePath();
    const conn: any = {
      from: 'F',
      to: 'T',
      fromIndex: 0,
      toIndex: 0,
      points: [
        { x: 10, y: 5 },
        { x: 100, y: 5 },
      ],
      path,
    };
    const state = baseState(makeBlocksWithTypes(undefined, undefined), [conn]);
    const ctx = baseCtx(state);
    makeWireApplyHandlers(ctx as any).applyWirePaths(null, {});
    expect(path.getAttribute('marker-end')).toBe('url(#wire-arrow)');
  });

  test('connection to labelSink removes marker-end', () => {
    const path = makePath();
    const conn: any = {
      from: 'F',
      to: 'T',
      fromIndex: 0,
      toIndex: 0,
      points: [
        { x: 10, y: 5 },
        { x: 100, y: 5 },
      ],
      path,
    };
    const state = baseState(makeBlocksWithTypes(undefined, FieldNames.kLabelSink), [conn]);
    const ctx = baseCtx(state);
    makeWireApplyHandlers(ctx as any).applyWirePaths(null, {});
    expect(path.getAttribute('marker-end')).toBeNull();
  });

  test('connection from labelSource removes marker-end', () => {
    const path = makePath();
    const conn: any = {
      from: 'F',
      to: 'T',
      fromIndex: 0,
      toIndex: 0,
      points: [
        { x: 10, y: 5 },
        { x: 100, y: 5 },
      ],
      path,
    };
    const state = baseState(makeBlocksWithTypes(FieldNames.kLabelSource, undefined), [conn]);
    const ctx = baseCtx(state);
    makeWireApplyHandlers(ctx as any).applyWirePaths(null, {});
    expect(path.getAttribute('marker-end')).toBeNull();
  });

  test('applyWirePathsFast from labelSource also removes marker-end', () => {
    const path = makePath();
    const conn: any = {
      from: 'F',
      to: 'T',
      fromIndex: 0,
      toIndex: 0,
      points: [
        { x: 10, y: 5 },
        { x: 100, y: 5 },
      ],
      path,
    };
    const blocks = makeBlocksWithTypes(FieldNames.kLabelSource, undefined);
    const state = { ...baseState(blocks, [conn]), fastRouting: true };
    const ctx = baseCtx(state);
    const connections = new Map<string, any>([['c1', conn]]);
    makeWireApplyHandlers(ctx as any).applyWirePathsFast(connections as any);
    expect(path.getAttribute('marker-end')).toBeNull();
  });
});
//=============================================================================
describe('Item 4 - simplifyOrthogonalPath loop removal', () => {
  test('removes U-shaped detour (backwards loop)', () => {
    // (0,0)->(10,0)->(10,20)->(5,20)->(5,10)->(20,10): the section (10,0)...(5,10)
    // is a detour that can be cut to (10,10).
    const pts = [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 10, y: 20 },
      { x: 5, y: 20 },
      { x: 5, y: 10 },
      { x: 20, y: 10 },
    ];
    const result = simplifyOrthogonalPath(pts);
    expect(result.length).toBeLessThan(pts.length);
    expect(result[0]).toEqual({ x: 0, y: 0 });
    expect(result[result.length - 1]).toEqual({ x: 20, y: 10 });
    for (let i = 0; i < result.length - 1; i += 1) {
      expect(result[i].x === result[i + 1].x || result[i].y === result[i + 1].y).toBe(true);
    }
  });

  test('2-point diagonal gets L-shape corner inserted', () => {
    const result = simplifyOrthogonalPath([
      { x: 0, y: 0 },
      { x: 10, y: 5 },
    ]);
    expect(result.length).toBe(3);
    expect(result[0]).toEqual({ x: 0, y: 0 });
    expect(result[result.length - 1]).toEqual({ x: 10, y: 5 });
    for (let i = 0; i < result.length - 1; i += 1) {
      expect(result[i].x === result[i + 1].x || result[i].y === result[i + 1].y).toBe(true);
    }
  });

  test('already-simple orthogonal path is unchanged', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 50, y: 0 },
      { x: 50, y: 30 },
    ];
    expect(simplifyOrthogonalPath(pts)).toEqual(pts);
  });

  test('renderer simplifyOrthogonalPath delegates to shared logic and removes loop', async () => {
    const mod: any = await import('../../../dist/render/wire/renderer-wire-geometry.js');
    const C: any = {
      dedupePoints: (p: any) => p,
      removeColinearPoints: (p: any) => {
        if (p.length <= 2) return p;
        const out = [p[0]];
        for (let i = 1; i < p.length - 1; i++) {
          const prev = out[out.length - 1];
          const cur = p[i];
          const next = p[i + 1];
          if (!((prev.x === cur.x && cur.x === next.x) || (prev.y === cur.y && cur.y === next.y)))
            out.push(cur);
        }
        out.push(p[p.length - 1]);
        return out;
      },
      samePoint: (a: any, b: any) => a && b && a.x === b.x && a.y === b.y,
    };
    const mh = mod.makeWireGeometryHandlers(C);
    const pts = [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 10, y: 20 },
      { x: 5, y: 20 },
      { x: 5, y: 10 },
      { x: 20, y: 10 },
    ];
    const result = mh.simplifyOrthogonalPath(pts);
    expect(result.length).toBeLessThan(pts.length);
    expect(result[0]).toEqual({ x: 0, y: 0 });
    expect(result[result.length - 1]).toEqual({ x: 20, y: 10 });
  });
});
//=============================================================================
describe('Item 5 - buildFallbackPath tight-block stub clearance', () => {
  const STUB = 10;

  test('normal spacing: midX is clamped within clearance window', () => {
    // window [10,90], rawMid=50, so midX=50
    const path = buildFallbackPath({ x: 0, y: 0 }, { x: 100, y: 50 });
    expect(path.length).toBe(4);
    const midX = path[1].x;
    expect(midX).toBeGreaterThanOrEqual(STUB);
    expect(midX).toBeLessThanOrEqual(100 - STUB);
  });

  test('tight spacing (< 2×STUB): midX is pushed past source stub, not raw midpoint', () => {
    // window [10,5] is empty (tight), so midX = 0+10 = 10
    const path = buildFallbackPath({ x: 0, y: 0 }, { x: 15, y: 30 });
    expect(path.length).toBe(4);
    const midX = path[1].x;
    expect(midX).toBe(STUB);
  });

  test('tight spacing, source on the right: midX pushed left of source by one stub', () => {
    // tight: midX = 15-10 = 5
    const path = buildFallbackPath({ x: 15, y: 0 }, { x: 0, y: 30 });
    expect(path.length).toBe(4);
    const midX = path[1].x;
    expect(midX).toBe(15 - STUB);
  });

  test('vertical alignment returns straight 2-point path unchanged', () => {
    const path = buildFallbackPath({ x: 50, y: 0 }, { x: 50, y: 100 });
    expect(path).toEqual([
      { x: 50, y: 0 },
      { x: 50, y: 100 },
    ]);
  });

  test('all intermediate points share source Y then dest Y', () => {
    const from = { x: 0, y: 10 };
    const to = { x: 200, y: 80 };
    const path = buildFallbackPath(from, to);
    expect(path[1].y).toBe(from.y);
    expect(path[2].y).toBe(to.y);
  });
});
//=============================================================================
describe('Item 6 - enforcePortStubs orthogonal bridge points', () => {
  function makeApplyCtx(fromSide: string, toSide: string, points: any[]) {
    const GRID = 10;
    const portXOut = fromSide === 'right' ? 80 : 20;
    const portXIn = toSide === 'left' ? 20 : 80;
    const fromBlock: any = {
      x: fromSide === 'right' ? 0 : 0,
      y: 0,
      width: portXOut,
      height: 40,
      ports: [{ type: 'out', index: 0, x: portXOut, y: 20 }],
    };
    const toBlock: any = {
      x: 200,
      y: 30,
      width: 100,
      height: 40,
      ports: [{ type: 'in', index: 0, x: portXIn === 20 ? 0 : 100, y: 20 }],
    };
    const blocks = new Map<string, any>([
      ['F', fromBlock],
      ['T', toBlock],
    ]);
    const conn: any = { from: 'F', to: 'T', fromIndex: 0, toIndex: 0, points, path: makePath() };
    const state = baseState(blocks, [conn]);
    const ctx = baseCtx(state, {
      isValidStub: () => false, // force stub insertion for every port
      simplifyOrthogonalPath: (p: any) => p,
      dedupePoints: (p: any) => {
        const out: any[] = [];
        for (const pt of p) {
          const last = out[out.length - 1];
          if (!last || last.x !== pt.x || last.y !== pt.y) out.push(pt);
        }
        return out;
      },
    });
    return { h: makeWireApplyHandlers(ctx as any), conn, state };
  }

  test('all inserted points form orthogonal segments after stub enforcement', () => {
    const pts = [
      { x: 80, y: 20 },
      { x: 220, y: 50 },
    ];
    const { h, conn } = makeApplyCtx('right', 'left', pts);
    h.applyWirePaths(null, {});
    const result: any[] = conn.points || [];
    expect(result.length).toBeGreaterThanOrEqual(2);
    for (let i = 0; i < result.length - 1; i += 1) {
      const a = result[i];
      const b = result[i + 1];
      expect(a.x === b.x || a.y === b.y).toBe(true);
    }
  });

  test('no duplicate consecutive points after stub insertion', () => {
    const pts = [
      { x: 80, y: 20 },
      { x: 160, y: 20 },
      { x: 220, y: 50 },
    ];
    const { h, conn } = makeApplyCtx('right', 'left', pts);
    h.applyWirePaths(null, {});
    const result: any[] = conn.points || [];
    for (let i = 0; i < result.length - 1; i += 1) {
      expect(result[i].x === result[i + 1].x && result[i].y === result[i + 1].y).toBe(false);
    }
  });
});
//=============================================================================
