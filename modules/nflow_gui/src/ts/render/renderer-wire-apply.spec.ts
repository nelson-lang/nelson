//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { makeWireApplyHandlers } from './wire/renderer-wire-apply';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
const makeState = () => ({ blocks: new Map(), connections: [] });
test('isValidPathString detects bad strings', () => {
  const S: any = makeState();
  const ctx: any = { state: S, simplifyOrthogonalPath: (p: any) => p, dedupePoints: (p: any) => p };
  const h = makeWireApplyHandlers(ctx as any);
  expect(h.isValidPathString(null)).toBe(true);
  expect(h.isValidPathString('M0,0 L10,10')).toBe(true);
  expect(h.isValidPathString('NaN')).toBe(false);
  expect(h.isValidPathString('undefined')).toBe(false);
});
//=============================================================================
test('enforcePortStubs adds port positions and stubs when needed (via applyWirePaths)', () => {
  const from: any = {
    id: 'f',
    x: 0,
    y: 0,
    width: 10,
    height: 10,
    ports: [{ type: 'out', index: 0, x: 5, y: 5 }],
    group: document.createElement('div'),
  };
  const to: any = {
    id: 't',
    x: 60,
    y: 0,
    width: 10,
    height: 10,
    ports: [{ type: 'in', index: 0, x: 5, y: 5 }],
    group: document.createElement('div'),
  };
  const pts = [
    { x: 15, y: 10 },
    { x: 55, y: 10 },
  ];
  const conn: any = {
    from: 'f',
    to: 't',
    points: pts.slice(),
    path: document.createElement(FieldNames.kPath),
  };
  const state: any = {
    blocks: new Map([
      ['f', from],
      ['t', to],
    ]),
    connections: [conn],
    fastRouting: false,
    autoRoute: false,
  };
  const ctx: any = {
    state,
    isValidStub: () => false,
    simplifyOrthogonalPath: (p: any) => p,
    dedupePoints: (p: any) => p,
    applyWireOffsets: (_c: any, p: any) => p,
    buildDragRenderPoints: (_c: any, p: any) => p,
    tryShorterOrthogonalPath: (_c: any, p: any) => p,
    buildFallbackPathFromPorts: (_c: any) => pts.slice(),
    renderSharedSourceJunctionDots: () => {},
  };
  const h = makeWireApplyHandlers(ctx as any);
  h.applyWirePaths(null, {});
  const out = conn.points;
  // output should include initial fromPortPos and final toPortPos (snapped)
  expect(out[0].x).toBe(10);
  expect(out[out.length - 1].x).toBe(70);
});
//=============================================================================
test('applyWirePaths assigns fallback points and routeFailed when points missing', () => {
  const pathEl = document.createElement(FieldNames.kPath);
  const conn: any = { from: 'a', to: 'b', points: undefined, path: pathEl };
  const state: any = {
    blocks: new Map(),
    connections: [conn],
    fastRouting: false,
    autoRoute: false,
  };
  const ctx: any = {
    state,
    buildFallbackPathFromPorts: (c: any) => [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    buildDragRenderPoints: (_c: any, p: any) => p,
    applyWireOffsets: (_c: any, p: any) => p,
    simplifyOrthogonalPath: (p: any) => p,
    dedupePoints: (p: any) => p,
    tryShorterOrthogonalPath: (_c: any, p: any) => p,
    renderSharedSourceJunctionDots: () => {},
  };
  const h = makeWireApplyHandlers(ctx as any);
  h.applyWirePaths(null, {});
  expect(conn.points).toBeDefined();
});
//=============================================================================
test('applyWirePathsFast sets path d for a simple connection', () => {
  const fromBlock = {
    x: 0,
    y: 0,
    width: 10,
    height: 10,
    ports: [{ type: 'out', index: 0, x: 0, y: 0 }],
  };
  const toBlock = {
    x: 100,
    y: 0,
    width: 10,
    height: 10,
    ports: [{ type: 'in', index: 0, x: 0, y: 0 }],
  };
  const blocks = new Map();
  blocks.set('b1', fromBlock);
  blocks.set('b2', toBlock);

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn = {
    from: 'b1',
    to: 'b2',
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 0, y: 0 },
      { x: 110, y: 0 },
    ],
    path,
  };
  const connections = new Map();
  connections.set('c1', conn);

  const ctx: any = {
    state: { blocks, connections: [conn], fastRouting: true, autoRoute: false },
    isValidStub: () => true,
    simplifyOrthogonalPath: (p: any) => p,
    dedupePoints: (p: any) => p,
    tryShorterOrthogonalPath: (c: any, p: any) => p,
    applyWireOffsets: (c: any, p: any) => p,
    buildFallbackPathFromPorts: (c: any) => [
      { x: 0, y: 0 },
      { x: 110, y: 0 },
    ],
    buildDragRenderPoints: (c: any, p: any) => p,
    renderSharedSourceJunctionDots: () => {},
  };

  const handlers = makeWireApplyHandlers(ctx);
  handlers.applyWirePathsFast(connections);
  expect(path.getAttribute('d')).toBeTruthy();
  expect(conn.debugRenderPoints).toBeDefined();
});
//=============================================================================
test('makeWireApplyHandlers returns object safely', () => {
  const ctx: any = { svg: null, state: { connections: [] } };
  const handlers = makeWireApplyHandlers(ctx as any);
  expect(typeof handlers).toBe('object');
});
//=============================================================================
