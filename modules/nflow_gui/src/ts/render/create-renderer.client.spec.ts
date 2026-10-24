//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('create-renderer client helpers', () => {
  let createRendererModule: any;

  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
  });

  test('clientToSvg applies matrixTransform when getScreenCTM present', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
    };

    svg.createSVGPoint = () => {
      const p: any = { x: 0, y: 0 };
      p.matrixTransform = () => ({ x: p.x - 10, y: p.y - 5 });
      return p;
    };
    svg.getScreenCTM = () => ({ inverse: () => ({}) } as any);

    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });
    const pt = renderer.clientToSvg(100, 50);
    expect(pt).toEqual({ x: 90, y: 45 });
  });

  test('findNearestConnection returns nearest when within threshold', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const conn: any = {
      points: [
        { x: 0, y: 0 },
        { x: 100, y: 0 },
      ],
    };
    const state: any = {
      blocks: new Map(),
      connections: [conn],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
    };

    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });
    const near = renderer.findNearestConnection({ x: 50, y: 5 });
    expect(near).toBe(conn);
    const far = renderer.findNearestConnection({ x: 500, y: 500 });
    expect(far).toBeNull();
  });
});
//=============================================================================
