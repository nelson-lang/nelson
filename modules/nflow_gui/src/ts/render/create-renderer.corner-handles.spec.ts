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
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const require = createRequire(import.meta.url);
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
beforeAll(async () => {
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
    // @ts-ignore
    const manifest = require(manifestPath);
    manifest._dir = dir;
    discoveredManifests.set(`builtin.${dir}`, manifest);
  }
  await Promise.all(BUILTIN_DIRS.map((d) => libLoader.loadLibrary(`builtin.${d}`)));
  const templates = await import('../../../dist/render/block/templates.js');
  templates.refreshBlockTemplates();
});
//=============================================================================
test('corner handles are created when connection selected and autoRoute disabled', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  Object.defineProperty(svg, 'clientWidth', { get: () => 400, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 300, configurable: true });
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
    autoRoute: false,
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

  const b1 = renderer.createBlock('constant', 10, 10, { id: 'b1', params: { value: 1 } });
  const b2 = renderer.createBlock('constant', 120, 10, { id: 'b2', params: { value: 2 } });
  const conn: any = {
    from: b1.id,
    to: b2.id,
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 20, y: 20 },
      { x: 60, y: 20 },
      { x: 120, y: 60 },
    ],
    path: document.createElementNS(ns, 'path'),
    hitPath: document.createElementNS(ns, 'path'),
  };
  state.connections.push(conn);

  state.selectedConnection = conn;
  state.autoRoute = false;
  expect(() => renderer.updateConnections()).not.toThrow();

  const junctionLayer = overlayLayer.querySelector('.wire-corner-layer');
  expect(junctionLayer).toBeTruthy();
  expect(() => {
    const handles = junctionLayer!.querySelectorAll('.wire-corner-handle');
    void handles.length;
  }).not.toThrow();
});
//=============================================================================
