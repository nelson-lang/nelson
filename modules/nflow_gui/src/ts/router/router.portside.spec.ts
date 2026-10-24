//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { routeAllConnections } from './router';
//=============================================================================
function makeBlock(id, x, y, width = 100, height = 100, portX = 50, portY = 50, portType = 'out') {
  return {
    id,
    x,
    y,
    width,
    height,
    rotation: 0,
    ports: [
      {
        type: portType,
        index: 0,
        x: portX,
        y: portY,
      },
    ],
  };
}
//=============================================================================
describe('port side -> direction mapping (via routeAllConnections)', () => {
  test('left port maps to `left` dir', () => {
    const state = {
      blocks: new Map([
        ['S', makeBlock('S', 0, 0, 100, 100, 10, 50, 'out')],
        ['T', makeBlock('T', 300, 0, 100, 100, 50, 50, 'in')],
      ]),
      connections: [{ from: 'S', to: 'T', fromIndex: 0, toIndex: 0 }],
    } as any;

    routeAllConnections(state, 400, 200, { x: 0, y: 0 }, 200);
    const nodes = (window as any).__routerLast?.nodes || [];
    const src = nodes.find((n) => n.id === 'S:out:0');
    expect(src).toBeDefined();
    expect(src.dir).toBe('left');
  });

  test('right port maps to `right` dir', () => {
    const state = {
      blocks: new Map([
        ['S', makeBlock('S', 0, 0, 100, 100, 90, 50, 'out')],
        ['T', makeBlock('T', 300, 0, 100, 100, 50, 50, 'in')],
      ]),
      connections: [{ from: 'S', to: 'T', fromIndex: 0, toIndex: 0 }],
    } as any;
    routeAllConnections(state, 400, 200, { x: 0, y: 0 }, 200);
    const nodes = (window as any).__routerLast?.nodes || [];
    const src = nodes.find((n) => n.id === 'S:out:0');
    expect(src).toBeDefined();
    expect(src.dir).toBe('right');
  });

  test('top port maps to `up` dir', () => {
    const state = {
      blocks: new Map([
        ['S', makeBlock('S', 0, 0, 100, 100, 50, 10, 'out')],
        ['T', makeBlock('T', 300, 0, 100, 100, 50, 50, 'in')],
      ]),
      connections: [{ from: 'S', to: 'T', fromIndex: 0, toIndex: 0 }],
    } as any;
    routeAllConnections(state, 400, 200, { x: 0, y: 0 }, 200);
    const nodes = (window as any).__routerLast?.nodes || [];
    const src = nodes.find((n) => n.id === 'S:out:0');
    expect(src).toBeDefined();
    expect(src.dir).toBe('up');
  });

  test('bottom port maps to `down` dir', () => {
    const state = {
      blocks: new Map([
        ['S', makeBlock('S', 0, 0, 100, 100, 50, 90, 'out')],
        ['T', makeBlock('T', 300, 0, 100, 100, 50, 50, 'in')],
      ]),
      connections: [{ from: 'S', to: 'T', fromIndex: 0, toIndex: 0 }],
    } as any;
    routeAllConnections(state, 400, 200, { x: 0, y: 0 }, 200);
    const nodes = (window as any).__routerLast?.nodes || [];
    const src = nodes.find((n) => n.id === 'S:out:0');
    expect(src).toBeDefined();
    expect(src.dir).toBe('down');
  });
});
//=============================================================================
