//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../FieldNames.js';
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
test('updateConnections full-route branch assigns points when connections missing points', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod as any;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  const blockLayer = document.createElementNS(ns, 'g');
  const wireLayer = document.createElementNS(ns, 'g');
  const overlayLayer = document.createElementNS(ns, 'g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    routingDirty: true,
    fastRouting: false,
    autoRoute: true,
    deferRouting: false,
    deferRoutingIds: new Set(),
    routeEpoch: 0,
  };

  const b1: any = {
    id: 'b1',
    x: 10,
    y: 10,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'out', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  const b2: any = {
    id: 'b2',
    x: 120,
    y: 10,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'in', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  state.blocks.set('b1', b1);
  state.blocks.set('b2', b2);

  const conn: any = {
    from: 'b1',
    to: 'b2',
    fromIndex: 0,
    toIndex: 0,
    path: document.createElementNS(ns, 'path'),
    hitPath: document.createElementNS(ns, 'path'),
  };
  state.connections.push(conn);

  const renderer = createRenderer({
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  await renderer.forceFullRoute(50);

  expect(Array.isArray(conn.points)).toBe(true);
  expect(conn.points.length).toBeGreaterThanOrEqual(2);
  const d = conn.path.getAttribute('d');
  expect(typeof d).toBe('string');
  expect(d.length).toBeGreaterThan(0);
});
//=============================================================================
