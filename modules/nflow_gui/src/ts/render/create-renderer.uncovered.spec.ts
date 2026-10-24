//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
import { createRequire } from 'module';
import { fileURLToPath } from 'url';
import path from 'path';
//=============================================================================
beforeAll(async () => {
  // Ensure block templates are populated like other tests
  const libLoader = await import('../../../dist/app/library-loader.js');
  const discoveredManifests = libLoader.getDiscoveredManifests();
  const BUILTIN_DIRS = ['source', 'math', 'continuous', 'discrete', 'nonlinear', 'sink', 'utility'];
  const requireFunc = createRequire(import.meta.url);
  for (const dir of BUILTIN_DIRS) {
    const manifestUrl = path.join(
      process.cwd(),
      'modules',
      'nflow_blocks',
      'libraries',
      dir,
      'library.json'
    );
    const manifestPath = manifestUrl;
    // eslint-disable-next-line @typescript-eslint/no-var-requires
    const manifest = requireFunc(manifestPath);
    manifest._dir = dir;
    discoveredManifests.set(`builtin.${dir}`, manifest);
  }
  await Promise.all(BUILTIN_DIRS.map((d) => libLoader.loadLibrary(`builtin.${d}`)));
  const { refreshBlockTemplates } = await import('../../../dist/render/block/templates.js');
  refreshBlockTemplates();
});
//=============================================================================
test('exercise uncovered create-renderer flows (scope/comment/userFunc/resize/forceFullRoute)', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod as any;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  svg.dataset.worldWidth = '400';
  svg.dataset.worldHeight = '300';
  Object.defineProperty(svg, 'clientWidth', { get: () => 400, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 300, configurable: true });
  (svg as any).createSVGPoint = function () {
    return { x: 0, y: 0, matrixTransform: (m: any) => ({ x: 0, y: 0 }) };
  };
  (svg as any).getScreenCTM = () => ({ inverse: () => ({}) } as any);

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
    routingDirty: false,
    fastRouting: false,
    autoRoute: false,
    routeEpoch: 0,
  };

  document.body.appendChild(svg);

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

  const scope = renderer.createBlock('scope', 20, 20, {
    id: 's1',
    params: { width: 200, height: 160 },
  });
  expect(scope).toBeTruthy();
  expect(() => renderer.resizeBlock(scope, 180, 140)).not.toThrow();

  const comment = renderer.createBlock('comment', 10, 10, {
    id: 'c1',
    params: { commentText: 'hi' },
  });
  expect(comment).toBeTruthy();
  expect(() => renderer.resizeBlock(comment, 120, 80)).not.toThrow();

  const uf = renderer.createBlock('expression', 50, 50, { id: 'u1', params: { expr: 'x' } });
  if (uf) {
    expect(() => renderer.updateBlockLabel(uf)).not.toThrow();
  }

  const b1 = renderer.createBlock('constant', 0, 0, { id: 'b1', params: { value: 1 } });
  const b2 = renderer.createBlock('constant', 120, 0, { id: 'b2', params: { value: 2 } });
  const conn = renderer.createConnection(b1.id, b2.id, 0, 0);
  expect(conn).toBeTruthy();
  expect(() => renderer.findNearestConnection({ x: 10, y: 10 })).not.toThrow();
  renderer.selectConnection(conn);
  renderer.deleteConnection(conn);

  expect(() => renderer.clientToSvg(5, 5)).not.toThrow();

  await renderer.forceFullRoute(10);

  renderer.clearWorkspace();
});
//=============================================================================
export {};
//=============================================================================
