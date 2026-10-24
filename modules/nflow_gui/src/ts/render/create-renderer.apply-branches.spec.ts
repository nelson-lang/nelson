//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from 'vitest';
import path from 'path';
import { createRequire } from 'module';
import { fileURLToPath } from 'url';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
test('applyWirePaths preservePathShape and overlap handling', async () => {
  const createMod: any = await import('../../../dist/render/create-renderer.js');
  const templatesMod: any = await import('../../../dist/render/block/templates.js');
  const libLoader: any = await import('../../../dist/app/library-loader.js');
  const require = createRequire(import.meta.url);
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
    // eslint-disable-next-line import/no-dynamic-require, global-require
    // @ts-ignore
    const manifest = require(manifestPath);
    manifest._dir = dir;
    discoveredManifests.set(`builtin.${dir}`, manifest);
  }
  await Promise.all(BUILTIN_DIRS.map((d) => libLoader.loadLibrary(`builtin.${d}`)));
  templatesMod.refreshBlockTemplates();

  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  svg.dataset.worldWidth = '400';
  svg.dataset.worldHeight = '300';
  Object.defineProperty(svg, 'clientWidth', { get: () => 400, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 300, configurable: true });
  const blockLayer = document.createElementNS(ns, 'g');
  const wireLayer = document.createElementNS(ns, 'g');
  const overlayLayer = document.createElementNS(ns, 'g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);
  document.body.appendChild(svg);

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
  };

  const labelSink: any = {
    id: 'LS',
    type: FieldNames.kLabelSink,
    x: 120,
    y: 10,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'in', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  const src: any = {
    id: 'S',
    type: 'constant',
    x: 10,
    y: 10,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'out', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  state.blocks.set('LS', labelSink);
  state.blocks.set('S', src);

  const conn: any = {
    from: 'S',
    to: 'LS',
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 20, y: 20 },
      { x: 100, y: 20 },
    ],
    path: document.createElementNS(ns, 'path'),
    hitPath: document.createElementNS(ns, 'path'),
    debugIssues: ['overlaps another wire'],
  };
  state.connections.push(conn);

  const renderer = createMod.createRenderer({
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

  renderer.renderCurrentWirePaths(true);

  expect(state.debugOverlapCount >= 0).toBeTruthy();
  expect(conn.path.getAttribute('marker-end')).toBeNull();
  const d = conn.path.getAttribute('d');
  expect(typeof d === 'string').toBeTruthy();
  expect(d!.length).toBeGreaterThan(0);
});
//=============================================================================
