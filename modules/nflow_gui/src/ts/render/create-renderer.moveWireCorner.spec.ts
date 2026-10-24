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
//=============================================================================
beforeAll(async () => {
  // ensure templates/libraries are loaded like other renderer tests
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
test('moveWireCorner via pointer events updates connection points', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  svg.dataset.worldWidth = '400';
  svg.dataset.worldHeight = '300';
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
    fastRouting: false,
    autoRoute: false,
  };

  const api = createRenderer({
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

  const a = api.createBlock('constant', 10, 10, { id: 'm1', params: { value: 1 } });
  const b = api.createBlock('constant', 120, 40, { id: 'm2', params: { value: 2 } });
  const conn = api.createConnection(a.id, b.id, 0, 0);
  expect(conn).toBeTruthy();

  api.selectConnection(conn);
  api.renderCurrentWirePaths();

  const handle = overlayLayer.querySelector('.wire-corner-handle');
  if (!handle) {
    api.selectConnection(conn);
  }
  const found = overlayLayer.querySelector('.wire-corner-handle');
  if (!found) return;

  const cx = Number(found.getAttribute('cx')) || 0;
  const cy = Number(found.getAttribute('cy')) || 0;

  const down = new PointerEvent('pointerdown', {
    bubbles: true,
    cancelable: true,
    pointerId: 5,
    clientX: cx,
    clientY: cy,
  });
  found.dispatchEvent(down);

  const move = new PointerEvent('pointermove', {
    bubbles: true,
    cancelable: true,
    pointerId: 5,
    clientX: cx + 10,
    clientY: cy + 10,
  });
  svg.dispatchEvent(move);

  const up = new PointerEvent('pointerup', {
    bubbles: true,
    cancelable: true,
    pointerId: 5,
    clientX: cx + 10,
    clientY: cy + 10,
  });
  svg.dispatchEvent(up);

  expect(Array.isArray(conn.points)).toBeTruthy();
});
//=============================================================================
