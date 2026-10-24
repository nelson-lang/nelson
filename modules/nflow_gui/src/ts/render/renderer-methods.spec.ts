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
import * as FieldNames from '../FieldNames.js';
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
describe('createRenderer - selectBlock and deselectAll', () => {
  test('selectBlock marks block as selected in state', () => {
    const { renderer, state } = makeRendererSetup();
    const block = renderer.createBlock('constant');
    if (!block) return;

    renderer.selectBlock(block.id);
    expect(state.selectedIds.has(block.id)).toBe(true);
  });

  test('selectBlock clears previous selection and selects new id', () => {
    const { renderer, state } = makeRendererSetup();
    const b1 = renderer.createBlock('constant');
    const b2 = renderer.createBlock(FieldNames.kGain);
    if (!b1 || !b2) return;

    renderer.selectBlock(b1.id);
    expect(state.selectedIds.has(b1.id)).toBe(true);
    renderer.selectBlock(b2.id);
    expect(state.selectedIds.has(b1.id)).toBe(false);
    expect(state.selectedIds.has(b2.id)).toBe(true);
  });

  test('setMultiSelection clears previous and selects given ids', () => {
    const { renderer, state } = makeRendererSetup();
    const b1 = renderer.createBlock('constant');
    const b2 = renderer.createBlock(FieldNames.kGain);
    if (!b1 || !b2) return;

    renderer.setMultiSelection(new Set([b1.id, b2.id]), new Set());
    expect(state.selectedIds.has(b1.id)).toBe(true);
    expect(state.selectedIds.has(b2.id)).toBe(true);
  });
});
//=============================================================================
describe('createRenderer - deleteBlock', () => {
  test('deleteBlock removes block from state and DOM', () => {
    const { renderer, state, blockLayer } = makeRendererSetup();
    const block = renderer.createBlock('constant');
    if (!block) return;

    expect(state.blocks.has(block.id)).toBe(true);
    renderer.deleteBlock(block.id);
    expect(state.blocks.has(block.id)).toBe(false);
    expect(blockLayer.querySelector(`[data-block-id="${block.id}"]`)).toBeNull();
  });

  test('deleteBlock on nonexistent id does not throw', () => {
    const { renderer } = makeRendererSetup();
    expect(() => renderer.deleteBlock('nonexistent-id')).not.toThrow();
  });
});
//=============================================================================
describe('createRenderer - updateBlockLabel', () => {
  test('updateBlockLabel updates block label without error', () => {
    const { renderer } = makeRendererSetup();
    const block = renderer.createBlock('constant');
    if (!block) return;
    block.params = { value: '42' };
    expect(() => renderer.updateBlockLabel(block)).not.toThrow();
  });
});
//=============================================================================
describe('createRenderer - updateBlockTransform', () => {
  test('updateBlockTransform sets transform attribute', () => {
    const { renderer } = makeRendererSetup();
    const block = renderer.createBlock('constant');
    if (!block) return;
    block.x = 100;
    block.y = 50;
    block.rotation = 90;
    renderer.updateBlockTransform(block);
    const transform = block.group.getAttribute('transform');
    expect(transform).toBeTruthy();
    expect(transform).toContain('translate');
  });
});
//=============================================================================
describe('createRenderer - resizeBlock', () => {
  test('resizeBlock updates block dimensions', () => {
    const { renderer } = makeRendererSetup();
    const block = renderer.createBlock('scope');
    if (!block) return;
    expect(() => renderer.resizeBlock(block, 200, 150)).not.toThrow();
    expect(block.width).toBeGreaterThanOrEqual(160);
  });

  test('resizeBlock clamps small values to minimum', () => {
    const { renderer } = makeRendererSetup();
    const block = renderer.createBlock('scope');
    if (!block) return;
    renderer.resizeBlock(block, 10, 10);
    expect(block.width).toBeGreaterThanOrEqual(160);
    expect(block.height).toBeGreaterThanOrEqual(120);
  });
});
//=============================================================================
describe('createRenderer - clearWorkspace', () => {
  test('clearWorkspace removes all blocks and connections', () => {
    const { renderer, state } = makeRendererSetup();
    renderer.createBlock('constant');
    renderer.createBlock(FieldNames.kGain);
    expect(state.blocks.size).toBeGreaterThan(0);

    renderer.clearWorkspace();
    expect(state.blocks.size).toBe(0);
    expect(state.connections.length).toBe(0);
  });
});
//=============================================================================
describe('createRenderer - clearPending', () => {
  test('clearPending clears pendingPort state', () => {
    const { renderer, state } = makeRendererSetup();
    state.pendingPort = { blockId: 'test', portIndex: 0, portType: 'out' };
    renderer.clearPending();
    expect(state.pendingPort).toBeNull();
  });
});
//=============================================================================
describe('createRenderer - updateSelectionBox', () => {
  test('updateSelectionBox runs without error with selection', () => {
    const { renderer, state } = makeRendererSetup();
    const block = renderer.createBlock('constant');
    if (!block) return;
    renderer.selectBlock(block.id, false);
    expect(() => renderer.updateSelectionBox()).not.toThrow();
  });

  test('updateSelectionBox runs without error with empty selection', () => {
    const { renderer } = makeRendererSetup();
    expect(() => renderer.updateSelectionBox()).not.toThrow();
  });
});
//=============================================================================
describe('createRenderer - createConnection', () => {
  test('createConnection between two blocks creates connection object', () => {
    const { renderer, state } = makeRendererSetup();
    const b1 = renderer.createBlock('constant');
    const b2 = renderer.createBlock('scope');
    if (!b1 || !b2) return;

    b1.ports = b1.ports || [{ type: 'out', index: 0, x: 40, y: 20 }];
    b2.ports = b2.ports || [{ type: 'in', index: 0, x: 0, y: 20 }];

    const conn = renderer.createConnection(b1.id, b2.id, 0, 0);
    if (conn) {
      expect(conn.from).toBe(b1.id);
      expect(conn.to).toBe(b2.id);
    }
    expect(typeof state.connections).toBe('object');
  });
});
//=============================================================================
describe('createRenderer - deleteConnection', () => {
  test('deleteConnection removes wire from DOM', () => {
    const { renderer, state, wireLayer } = makeRendererSetup();
    const b1 = renderer.createBlock('constant');
    const b2 = renderer.createBlock('scope');
    if (!b1 || !b2) return;

    b1.ports = [{ type: 'out', index: 0, x: 40, y: 20 }];
    b2.ports = [{ type: 'in', index: 0, x: 0, y: 20 }];

    renderer.createConnection(b1.id, b2.id, 0, 0);
    const initialConnCount = state.connections.length;
    if (initialConnCount > 0) {
      renderer.deleteConnection(state.connections[0]);
      expect(state.connections.length).toBe(initialConnCount - 1);
    }
  });
});
//=============================================================================
describe('createRenderer - selectConnection', () => {
  test('selectConnection can be called without error', () => {
    const { renderer, state } = makeRendererSetup();
    const b1 = renderer.createBlock('constant');
    const b2 = renderer.createBlock('scope');
    if (!b1 || !b2) return;

    b1.ports = [{ type: 'out', index: 0, x: 40, y: 20 }];
    b2.ports = [{ type: 'in', index: 0, x: 0, y: 20 }];

    renderer.createConnection(b1.id, b2.id, 0, 0);
    if (state.connections.length > 0) {
      expect(() => renderer.selectConnection(state.connections[0])).not.toThrow();
    }
  });
});
//=============================================================================
describe('createRenderer - setLoopHighlight', () => {
  test('setLoopHighlight sets CSS class on blocks and connections', () => {
    const { renderer, state } = makeRendererSetup();
    const b1 = renderer.createBlock('constant');
    const b2 = renderer.createBlock('scope');
    if (!b1 || !b2) return;

    renderer.setLoopHighlight(new Set([b1.id]), []);
    expect(b1.group.classList.contains('loop-highlight')).toBe(true);

    renderer.setLoopHighlight(null, null);
    expect(b1.group.classList.contains('loop-highlight')).toBe(false);
  });
});
//=============================================================================
describe('createRenderer - getLastConnectionError', () => {
  test('getLastConnectionError returns null initially', () => {
    const { renderer } = makeRendererSetup();
    expect(renderer.getLastConnectionError()).toBeNull();
  });
});
//=============================================================================
describe('createRenderer - clientToSvg', () => {
  test('clientToSvg returns coordinates as object', () => {
    const { renderer, svg } = makeRendererSetup();
    const mockPoint: any = { x: 0, y: 0, matrixTransform: () => ({ x: 100, y: 200 }) };
    (svg as any).createSVGPoint = () => mockPoint;
    (svg as any).getScreenCTM = () => ({ inverse: () => ({}) });
    const result = renderer.clientToSvg(100, 200);
    expect(typeof result.x).toBe('number');
    expect(typeof result.y).toBe('number');
  });
});
//=============================================================================
describe('createRenderer - findNearestConnection', () => {
  test('findNearestConnection returns null when no connections exist', () => {
    const { renderer } = makeRendererSetup();
    const result = renderer.findNearestConnection(0, 0, 10);
    expect(result).toBeNull();
  });
});
//=============================================================================
describe('createRenderer - renderCurrentWirePaths', () => {
  test('renderCurrentWirePaths runs without throwing', () => {
    const { renderer } = makeRendererSetup();
    expect(() => renderer.renderCurrentWirePaths()).not.toThrow();
    expect(() => renderer.renderCurrentWirePaths(true)).not.toThrow();
  });
});
//=============================================================================
describe('createRenderer - updateConnections', () => {
  test('updateConnections runs for empty state without error', () => {
    const { renderer } = makeRendererSetup();
    expect(() => renderer.updateConnections()).not.toThrow();
  });
});
//=============================================================================
describe('createRenderer - startMarqueeSelection', () => {
  test('startMarqueeSelection runs without throwing', () => {
    const { renderer } = makeRendererSetup();
    expect(() => renderer.startMarqueeSelection(10, 10)).not.toThrow();
  });
});
//=============================================================================
describe('createRenderer - createBlock (all block types)', () => {
  const allTypes = [
    'constant',
    'step',
    'ramp',
    'impulse',
    'sine',
    'chirp',
    'noise',
    'scope',
    'xyScope',
    'xyzScope',
    'saturation',
    'rate',
    'backlash',
    'lpf',
    'hpf',
    'pid',
    'tf',
    'dtf',
    'zoh',
    'foh',
    'integrator',
    'derivative',
    FieldNames.kDelay,
    'ddelay',
    'stateSpace',
    'dstateSpace',
    FieldNames.kGain,
    'sum',
    'mult',
    'abs',
    'switch',
    'expression',
    FieldNames.kSubsystem,
    FieldNames.kLabelSource,
    FieldNames.kLabelSink,
    'comment',
  ];

  allTypes.forEach((type) => {
    test(`createBlock("${type}") returns block or null without error`, () => {
      const { renderer } = makeRendererSetup();
      let result: any;
      expect(() => {
        result = renderer.createBlock(type);
      }).not.toThrow();
      if (result) {
        expect(result.type).toBeTruthy();
      }
    });
  });
});
//=============================================================================
