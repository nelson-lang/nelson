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
import path from 'path';
import { createRequire } from 'module';
import { fileURLToPath } from 'url';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
beforeAll(async () => {
  const libLoader: any = await import('../../../dist/app/library-loader.js');
  const discoveredManifests = libLoader.getDiscoveredManifests();
  const BUILTIN_DIRS = ['source', 'math', 'continuous', 'discrete', 'nonlinear', 'sink', 'utility'];
  const require = createRequire(import.meta.url);
  for (const dir of BUILTIN_DIRS) {
    const manifestPath = path.join(
      process.cwd(),
      'modules',
      'nflow_blocks',
      'libraries',
      dir,
      'library.json'
    );
    // eslint-disable-next-line import/no-dynamic-require, global-require
    // @ts-ignore
    const manifest = require(manifestPath);
    manifest._dir = dir;
    discoveredManifests.set(`builtin.${dir}`, manifest);
  }
  await Promise.all(BUILTIN_DIRS.map((d) => libLoader.loadLibrary(`builtin.${d}`)));
  const { refreshBlockTemplates } = await import('../../../dist/render/block/templates.js');
  refreshBlockTemplates();
});
//=============================================================================
test('exhaustive: call every public renderer API and simulate interactions', async () => {
  const mod: any = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  Object.defineProperty(svg, 'clientWidth', { get: () => 600, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 400, configurable: true });
  svg.createSVGPoint = function () {
    return { x: 0, y: 0, matrixTransform: (m: any) => ({ x: 0, y: 0 }) };
  } as any;
  svg.getScreenCTM = () => ({ inverse: () => ({}) } as any);

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
    selectedConnection: null,
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

  const a = renderer.createBlock('constant', 10, 10, { id: 'A', params: { value: 1 } });
  const b = renderer.createBlock(FieldNames.kGain, 150, 10, { id: 'B', params: { gain: 2 } });
  const c = renderer.createBlock('comment', 300, 10, { id: 'C', params: { commentText: 'hello' } });
  expect(a).toBeTruthy();
  expect(b).toBeTruthy();
  expect(c).toBeTruthy();

  const conn = renderer.createConnection(a.id, b.id, 0, 0);
  expect(conn).toBeTruthy();
  renderer.updateConnections();
  renderer.renderCurrentWirePaths(true);

  renderer.updateSelectionBox();
  renderer.selectBlock(a.id);
  renderer.setMultiSelection(new Set([a.id, b.id]), new Set());
  renderer.startMarqueeSelection({
    button: 0,
    clientX: 0,
    clientY: 0,
    preventDefault: () => {},
    stopPropagation: () => {},
  } as any);

  expect(() => renderer.clientToSvg(1, 2)).not.toThrow();
  expect(renderer.getLastConnectionError()).toBeNull();

  await renderer.forceFullRoute(20);

  renderer.updateBlockTransform(a);
  renderer.updateBlockLabel(a);
  renderer.resizeBlock(a, 100, 50);

  renderer.selectConnection(conn);
  renderer.deleteConnection(conn);
  renderer.deleteBlock(a.id);

  renderer.clearWorkspace();
  expect(renderer.findNearestConnection({ x: 10, y: 10 })).toBeNull();

  renderer.setLoopHighlight([b.id], [conn]);

  renderer.clearWorkspace();
});
//=============================================================================
