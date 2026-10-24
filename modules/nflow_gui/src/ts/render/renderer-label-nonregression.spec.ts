//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { fileURLToPath } from 'url';
import path from 'path';
import { createRequire } from 'module';
//=============================================================================
const require = createRequire(import.meta.url);
let render: any;
beforeAll(async () => {
  (global as any).queueMathJaxTypeset = (global as any).queueMathJaxTypeset || (() => {});
  const createMod = await import('../../../dist/render/create-renderer.js');
  const templatesMod = await import('../../../dist/render/block/templates.js');
  render = { ...createMod, ...templatesMod };

  const libLoader = await import('../../../dist/app/library-loader.js');
  const discoveredManifests = libLoader.getDiscoveredManifests();
  const BUILTIN_DIRS = ['source', 'math', 'continuous', 'discrete', 'nonlinear', 'sink', 'utility'];

  for (const dir of BUILTIN_DIRS) {
    const manifestPath = path.resolve(
      path.join(process.cwd(), 'modules', 'nflow_blocks', 'libraries', dir, 'library.json')
    );
    const manifest = require(manifestPath);
    manifest._dir = dir;
    discoveredManifests.set(`builtin.${dir}`, manifest);
  }

  await Promise.all(BUILTIN_DIRS.map((dir) => libLoader.loadLibrary(`builtin.${dir}`)));
  render.refreshBlockTemplates();
});
//=============================================================================
function makeRendererSetup(stateOverride: any = {}) {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.dataset.worldWidth = '800';
  svg.dataset.worldHeight = '600';
  Object.defineProperty(svg, 'clientWidth', { get: () => 800, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 600, configurable: true });

  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);
  document.body.appendChild(svg);

  const state = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyBlocks: new Set(),
    dirtyConnections: new Set(),
    selectedIds: new Set(),
    selectedConnections: new Set(),
    pendingPort: null,
    routingDirty: false,
    autoRoute: false,
    ...stateOverride,
  } as any;

  const renderer = render.createRenderer({
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

  return { svg, blockLayer, wireLayer, overlayLayer, state, renderer };
}
//=============================================================================
afterEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
describe('labelSource non-regression', () => {
  test('labelSource default param name is "x" and DOM rendered', () => {
    const { renderer } = makeRendererSetup();
    const block = renderer.createBlock('labelSource');
    expect(block).toBeTruthy();
    expect(block.params).toBeTruthy();
    expect(block.params.GotoTag).toBe('x');

    // Label math group may be created with either module or manifest class.
    const mathGroup =
      block.group.querySelector('.label-math') || block.group.querySelector('.label-source-math');
    expect(mathGroup).not.toBeNull();

    // Check KaTeX target span holds the expected TeX string (when katex not available, dataset.tex is set)
    const span = mathGroup?.querySelector('.katex-target') as HTMLElement | null;
    expect(span).not.toBeNull();
    expect(span?.dataset.tex).toBe('x');

    const nodeLine = block.group.querySelector('line.label-node');
    const nodeCircle = block.group.querySelector('circle.label-node');
    expect(nodeLine || nodeCircle).toBeTruthy();
  });
});
//=============================================================================
export {};
//=============================================================================
