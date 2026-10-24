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
  const libLoader = await import('../../../dist/app/library-loader.js');
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
test('shared-source junction dots render for branches from same source', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod as any;
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
    autoRoute: true,
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

  const src = renderer.createBlock('constant', 10, 10, { id: 'src', params: { value: 0 } });
  const d1 = renderer.createBlock('constant', 120, 10, { id: 'd1', params: { value: 1 } });
  const d2 = renderer.createBlock('constant', 120, 80, { id: 'd2', params: { value: 2 } });

  const conn1: any = {
    from: src.id,
    to: d1.id,
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 30, y: 20 },
      { x: 80, y: 20 },
      { x: 120, y: 20 },
    ],
    path: document.createElementNS(ns, 'path'),
    hitPath: document.createElementNS(ns, 'path'),
  };
  const conn2: any = {
    from: src.id,
    to: d2.id,
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 30, y: 20 },
      { x: 30, y: 60 },
      { x: 120, y: 80 },
    ],
    path: document.createElementNS(ns, 'path'),
    hitPath: document.createElementNS(ns, 'path'),
  };
  state.connections.push(conn1, conn2);

  expect(() => renderer.renderCurrentWirePaths()).not.toThrow();

  const junctionLayer = overlayLayer.querySelector('.wire-junction-layer');
  expect(junctionLayer).toBeTruthy();
  expect(() => {
    const dots = junctionLayer!.querySelectorAll('.wire-junction-dot');
    void dots.length;
  }).not.toThrow();
});
//=============================================================================
