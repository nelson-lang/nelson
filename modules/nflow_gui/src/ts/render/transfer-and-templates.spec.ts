//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../FieldNames.js';
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let render: any;
beforeAll(async () => {
  const transferFn = await import('../../../dist/render/transfer-fn.js');
  const createMod = await import('../../../dist/render/create-renderer.js');
  render = { ...transferFn, ...createMod };
});
//=============================================================================
test('buildPolyTeX and buildTransferTeX produce expected LaTeX strings', () => {
  const simple = render.buildPolyTeX([1, -2, 3], 's');
  expect(simple).toContain('s^{2}');
  expect(simple).toContain('-2');

  const withSym = render.buildPolyTeX(['a', '-b', '0'], 'z');
  expect(withSym).toContain('a');
  expect(withSym).toContain('-b');

  const empty = render.buildPolyTeX([], 's');
  expect(empty).toBe('0');

  const transfer = render.buildTransferTeX([1, 0], [1, 2], 's');
  expect(transfer).toMatch(/\\frac\{.*\}\{.*\}/);
});
//=============================================================================
test('buildPolyMathML and buildTransferMathML produce MathML structure', () => {
  const p = render.buildPolyMathML([1, 'x', 0]);
  expect(p).toMatch(/<m(row|math)[^>]*>/);
  const t = render.buildTransferMathML([1], [1, -1]);
  expect(t).toContain('<mfrac>');
});
//=============================================================================
test('createBlock constructs additional built-in templates', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.dataset.worldWidth = '400';
  svg.dataset.worldHeight = '300';
  Object.defineProperty(svg, 'clientWidth', { get: () => 400, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 300, configurable: true });

  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyBlocks: new Set(),
    dirtyConnections: new Set(),
    selectedIds: new Set(),
    selectedConnection: null,
    pendingPort: null,
    routingDirty: false,
    autoRoute: false,
  };

  const renderer = render.createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });

  const extraTypes = [
    'pid',
    'integrator',
    'derivative',
    FieldNames.kDelay,
    'zoh',
    'foh',
    'stateSpace',
    'dstateSpace',
    'file',
  ];
  extraTypes.forEach((t) => {
    const block = renderer.createBlock(t);
    if (block) {
      expect(block.type).toBeTruthy();
      expect(block.group).toBeTruthy();
    }
  });
});
//=============================================================================
describe('type contracts', () => {
  test('buildTransferMathML/buildPolyMathML and TeX variants return strings even for empty/mixed inputs', () => {
    expect(typeof render.buildPolyMathML([1, 'x', 0])).toBe('string');
    expect(typeof render.buildTransferMathML([], [])).toBe('string');
    expect(typeof render.buildPolyTeX([1, 'a'], 's')).toBe('string');
    expect(typeof render.buildTransferTeX([1], [0, 'b'], 's')).toBe('string');
  });
});
//=============================================================================
