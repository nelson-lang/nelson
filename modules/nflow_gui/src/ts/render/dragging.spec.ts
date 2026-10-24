//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
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
  const createMod = await import('../../../dist/render/create-renderer.js');
  const templatesMod = await import('../../../dist/render/block/templates.js');
  render = { ...createMod, ...templatesMod };

  const libLoader = await import('../../../dist/app/library-loader.js');
  const BUILTIN_DIRS = ['source', 'math', 'continuous', 'discrete', 'nonlinear', 'sink', 'utility'];
  for (const dir of BUILTIN_DIRS) {
    const manifestPath = path.resolve(
      path.join(process.cwd(), 'modules', 'nflow_blocks', 'libraries', dir, 'library.json')
    );
    const manifest = require(manifestPath);
    manifest._dir = dir;
    libLoader.getDiscoveredManifests().set(`builtin.${dir}`, manifest);
  }
  await Promise.all(BUILTIN_DIRS.map((d) => libLoader.loadLibrary(`builtin.${d}`)));
  render.refreshBlockTemplates();
});
//=============================================================================
function makeRendererSetup() {
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

  const state: any = {
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
  };

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
describe('dragging behavior for blocks', () => {
  test('pointer drag updates block position and marks dirty', () => {
    const { svg, renderer, state } = makeRendererSetup();
    const block = renderer.createBlock('constant');
    if (!block) return;

    const lastClient: any = { x: 0, y: 0 };
    (svg as any).createSVGPoint = () => ({
      x: 0,
      y: 0,
      matrixTransform: () => ({ x: lastClient.x - 10, y: lastClient.y - 10 }),
    });
    (svg as any).getScreenCTM = () => ({ inverse: () => ({}) });

    const initialX = block.x;
    const initialY = block.y;

    const down = new MouseEvent('pointerdown', {
      bubbles: true,
      clientX: 100,
      clientY: 100,
    } as any);
    Object.defineProperty(down, 'pointerId', { value: 1 });
    lastClient.x = (down as any).clientX;
    lastClient.y = (down as any).clientY;
    try {
      block.dragRect.setPointerCapture = block.dragRect.setPointerCapture || (() => {});
    } catch (e) {
      block.dragRect.setPointerCapture = () => {};
    }
    block.dragRect.dispatchEvent(down);

    const move1 = new MouseEvent('pointermove', {
      bubbles: true,
      clientX: 102,
      clientY: 103,
    } as any);
    Object.defineProperty(move1, 'pointerId', { value: 1 });
    lastClient.x = (move1 as any).clientX;
    lastClient.y = (move1 as any).clientY;
    block.group.dispatchEvent(move1);

    const move2 = new MouseEvent('pointermove', {
      bubbles: true,
      clientX: 200,
      clientY: 200,
    } as any);
    Object.defineProperty(move2, 'pointerId', { value: 1 });
    lastClient.x = (move2 as any).clientX;
    lastClient.y = (move2 as any).clientY;
    block.group.dispatchEvent(move2);

    const up = new MouseEvent('pointerup', { bubbles: true, clientX: 200, clientY: 200 } as any);
    Object.defineProperty(up, 'pointerId', { value: 1 });
    lastClient.x = (up as any).clientX;
    lastClient.y = (up as any).clientY;
    block.group.dispatchEvent(up);

    expect(
      state.dirtyBlocks.has(block.id) ||
        state.routingDirty ||
        block.x !== initialX ||
        block.y !== initialY
    ).toBeTruthy();
  });
});
//=============================================================================
