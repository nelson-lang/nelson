//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import path from 'path';
import { fileURLToPath } from 'url';
import { createRequire } from 'module';
//=============================================================================
const require = createRequire(import.meta.url);
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
test('call all exported renderer methods to exercise internal functions', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  svg.dataset.worldWidth = '800';
  svg.dataset.worldHeight = '600';
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
    autoRoute: true,
    deferRouting: false,
    deferRoutingIds: new Set(),
    routeEpoch: 0,
    selectedConnection: null,
  };

  const templates = await import('../../../dist/render/block/templates.js');
  const { refreshBlockTemplates } = templates;
  const libLoader = await import('../../../dist/app/library-loader.js');
  const discoveredManifests = libLoader.getDiscoveredManifests();
  const BUILTIN_DIRS = ['source', 'math', 'continuous', 'discrete', 'nonlinear', 'sink', 'utility'];
  for (const dir of BUILTIN_DIRS) {
    const manifestPath = path.join(
      process.cwd(),
      'modules',
      'nflow_blocks',
      'libraries',
      dir,
      'library.json'
    );
    // eslint-disable-next-line global-require, import/no-dynamic-require
    // @ts-ignore require
    const manifest = require(manifestPath);
    manifest._dir = dir;
    discoveredManifests.set(`builtin.${dir}`, manifest);
  }
  await Promise.all(BUILTIN_DIRS.map((d) => libLoader.loadLibrary(`builtin.${d}`)));
  refreshBlockTemplates();

  Object.defineProperty(svg, 'clientWidth', { get: () => 800, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 600, configurable: true });
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
    onConnectionError: (e: any) => {
      state._lastErr = e;
    },
  });

  const bA = renderer.createBlock('constant', 10, 10, { id: 'bA', params: { value: 1 } });
  const bB = renderer.createBlock('constant', 140, 10, { id: 'bB', params: { value: 2 } });
  expect(bA).not.toBeNull();
  expect(bB).not.toBeNull();

  const conn = renderer.createConnection(bA.id, bB.id, 0, 0);
  expect(conn).not.toBeNull();

  renderer.updateSelectionBox();
  renderer.updateBlockTransform(bA);
  renderer.updateBlockLabel(bA);
  renderer.setLoopHighlight([bA.id], [conn]);
  expect(renderer.getLastConnectionError()).toBeNull();

  svg.createSVGPoint = function () {
    return {
      x: 0,
      y: 0,
      matrixTransform(m: any) {
        return { x: this.x, y: this.y };
      },
    } as any;
  };
  svg.getScreenCTM = () => ({ inverse: () => ({}) } as any);
  expect(() => renderer.clientToSvg(10, 20)).not.toThrow();

  renderer.selectBlock(bA.id);
  renderer.setMultiSelection(new Set([bA.id]), new Set());

  renderer.startMarqueeSelection({
    button: 0,
    clientX: 0,
    clientY: 0,
    preventDefault: () => {},
    stopPropagation: () => {},
  } as any);

  renderer.renderCurrentWirePaths(false);
  await renderer.forceFullRoute(20);

  renderer.deleteConnection(conn);
  renderer.deleteBlock(bA.id);

  renderer.clearWorkspace();

  expect(renderer.findNearestConnection({ x: 0, y: 0 })).toBeNull();
  expect(true).toBe(true);
});
//=============================================================================
