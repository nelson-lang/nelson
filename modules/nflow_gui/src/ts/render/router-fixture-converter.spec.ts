//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// .nflow → router-fixture converter.
//
// `.nflow` files carry no block geometry (width/height/ports); it lives in the
// block catalog. This spec builds the real editor (createRenderer + the real
// block libraries) in jsdom, loads real examples and emits routing snapshots for
// the parity corpus in modules/nflow_engine/tests/cpp/fixtures.
//
// NFLOW_FIXTURES_REGEN=1 rewrites the fixtures. Without it the spec is a drift
// guard: it fails when block geometry changes. Then regenerate the fixtures AND
// the goldens (NFLOW_ROUTER_REGEN=1, see the fixtures README).
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { beforeAll, describe, it, expect } from 'vitest';
import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'module';
//=============================================================================
const EXAMPLES: Array<{ file: string; fixture: string }> = [
  { file: 'causal/PID_Demo.nflow', fixture: 'example_pid.json' },
  { file: 'causal/Logic_Gates_Demo.nflow', fixture: 'example_logic_gates.json' },
  { file: 'causal/complementary_filter.nflow', fixture: 'example_complementary_filter.json' },
];
//=============================================================================
const examplesDir = path.join(process.cwd(), 'modules', 'nflow_blocks', 'examples');
const fixturesDir = path.join(process.cwd(), 'modules', 'nflow_engine', 'tests', 'cpp', 'fixtures');
const REGEN = process.env['NFLOW_FIXTURES_REGEN'] === '1';
beforeAll(async () => {
  const libLoader = await import('../../../dist/app/library-loader.js');
  const discoveredManifests = libLoader.getDiscoveredManifests();
  const BUILTIN_DIRS = [
    'source',
    'math',
    'continuous',
    'discrete',
    'logic',
    'nonlinear',
    'sink',
    'utility',
  ];
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
function makeRendererHarness() {
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg') as any;
  Object.defineProperty(svg, 'clientWidth', { get: () => 1600, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 1000, configurable: true });
  svg.createSVGPoint = function () {
    return { x: 0, y: 0, matrixTransform: () => ({ x: 0, y: 0 }) };
  };
  svg.getScreenCTM = () => ({ inverse: () => ({}) });
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
    autoRoute: true,
  };
  return { svg, blockLayer, wireLayer, overlayLayer, state };
}
//=============================================================================
async function convertExample(file: string): Promise<any> {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod as any;
  const harness = makeRendererHarness();
  const renderer = createRenderer({
    ...harness,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  const doc = JSON.parse(fs.readFileSync(path.join(examplesDir, file), 'utf-8'));
  for (const b of doc.blocks || []) {
    const created = renderer.createBlock(String(b.type), Number(b.x) || 0, Number(b.y) || 0, {
      id: b.id,
      rotation: Number(b.rotation) || 0,
      params: b.params || {},
    });
    expect(created, `block ${b.id} (${b.type}) failed to instantiate`).toBeTruthy();
  }

  const blocks = [...harness.state.blocks.values()]
    .sort((a: any, b: any) => (String(a.id) < String(b.id) ? -1 : 1))
    .map((block: any) => ({
      id: block.id,
      x: Number(block.x) || 0,
      y: Number(block.y) || 0,
      width: Number(block.width) || 0,
      height: Number(block.height) || 0,
      rotation: Number(block.rotation) || 0,
      ports: (block.ports || []).map((port: any) => ({
        x: Number(port.x) || 0,
        y: Number(port.y) || 0,
        type: port.type,
        index: Number(port.index) || 0,
      })),
    }));
  expect(blocks.length).toBe((doc.blocks || []).length);
  for (const b of blocks) {
    expect(b.width, `block ${b.id} has no width; catalog lookup failed`).toBeGreaterThan(0);
    expect(b.ports.length, `block ${b.id} has no ports`).toBeGreaterThan(0);
  }

  const connections = (doc.connections || []).map((c: any) => ({
    from: c.from,
    to: c.to,
    fromIndex: Number(c.fromIndex ?? 0),
    toIndex: Number(c.toIndex ?? 0),
  }));
  expect(connections.length).toBeGreaterThan(0);

  let maxX = 0;
  let maxY = 0;
  for (const b of blocks) {
    maxX = Math.max(maxX, b.x + b.width);
    maxY = Math.max(maxY, b.y + b.height);
  }
  return {
    blocks,
    connections,
    width: Math.ceil((maxX + 200) / 100) * 100,
    height: Math.ceil((maxY + 200) / 100) * 100,
  };
}
//=============================================================================
describe('.nflow → router fixture converter', () => {
  for (const { file, fixture } of EXAMPLES) {
    it(`${REGEN ? 'regenerates' : 'matches'} ${fixture} from ${file}`, async () => {
      const snapshot = await convertExample(file);
      const fixturePath = path.join(fixturesDir, fixture);
      if (REGEN) {
        fs.writeFileSync(fixturePath, `${JSON.stringify(snapshot, null, 1)}\n`, 'utf-8');
        return;
      }
      expect(
        fs.existsSync(fixturePath),
        `missing ${fixture}; run with NFLOW_FIXTURES_REGEN=1`
      ).toBe(true);
      const committed = JSON.parse(fs.readFileSync(fixturePath, 'utf-8'));
      // Drift guard: block catalog geometry changed → regenerate fixtures,
      // then the native goldens (NFLOW_ROUTER_REGEN=1).
      expect(snapshot).toEqual(committed);
    });
  }
});
//=============================================================================
