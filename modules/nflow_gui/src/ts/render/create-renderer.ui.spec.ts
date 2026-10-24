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
describe('create-renderer UI interactions', () => {
  let createRendererModule: any;

  let templatesModule: any;
  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
    templatesModule = await import('../../../dist/render/block/templates.js');
  });

  test('port click sets pending and clearPending removes it', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
    };
    const renderer = createRendererModule.createRenderer({
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

    const templates = templatesModule;
    templates.blockTemplates.testui = {
      width: 40,
      height: 20,
      inputs: [{ x: 0, y: 10 }],
      outputs: [{ x: 40, y: 10 }],
      defaultParams: {},
      render: (block: any) => {
        const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        rect.classList.add('nf-block');
        block.group.appendChild(rect);
      },
    };

    const b1 = renderer.createBlock('testui', 0, 0, { id: 'b1' });
    const b2 = renderer.createBlock('testui', 100, 0, { id: 'b2' });
    const out = b1.group.querySelector('.port');
    expect(out).not.toBeNull();

    out.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    expect(state.pendingPort).not.toBeNull();

    renderer.clearPending();
    expect(state.pendingPort).toBeNull();

    delete templates.blockTemplates.testui;
  });

  test('startMarqueeSelection and finishMarqueeSelection toggle marquee and multi-selection', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
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
      selectedConnections: new Set(),
    };
    const renderer = createRendererModule.createRenderer({
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

    const block = {
      id: 'b1',
      x: 10,
      y: 10,
      width: 20,
      height: 20,
      ports: [],
      group: document.createElementNS('http://www.w3.org/2000/svg', 'g'),
    };
    state.blocks.set('b1', block);

    svg.createSVGPoint = () => ({ x: 0, y: 0, matrixTransform: () => ({ x: 0, y: 0 }) } as any);
    svg.getScreenCTM = () => null;
    renderer.startMarqueeSelection({
      button: 0,
      clientX: 0,
      clientY: 0,
      preventDefault: () => {},
      stopPropagation: () => {},
    });
    svg.dispatchEvent(new MouseEvent('pointermove', { clientX: 50, clientY: 50, bubbles: true }));
    svg.dispatchEvent(new MouseEvent('pointerup', { clientX: 50, clientY: 50, bubbles: true }));
    expect(state.suppressNextCanvasClick).toBe(true);
  });

  test('setLoopHighlight toggles classes on blocks and connections', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = { blocks: new Map(), connections: [], nextId: 1 };
    const renderer = createRendererModule.createRenderer({
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

    const b = { id: 'b1', group: document.createElementNS('http://www.w3.org/2000/svg', 'g') };
    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = { path };
    state.blocks.set('b1', b);
    state.connections.push(conn);

    renderer.setLoopHighlight(['b1'], [conn]);
    expect(b.group.classList.contains('loop-highlight')).toBe(true);
    expect(conn.path.classList.contains('loop-highlight')).toBe(true);

    renderer.setLoopHighlight([], []);
    expect(b.group.classList.contains('loop-highlight')).toBe(false);
    expect(conn.path.classList.contains('loop-highlight')).toBe(false);
  });

  test('clicking output then input creates a connection (non-regression)', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
    };
    const renderer = createRendererModule.createRenderer({
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

    const templates = templatesModule;
    templates.blockTemplates.testui = {
      width: 40,
      height: 20,
      inputs: [{ x: 0, y: 10 }],
      outputs: [{ x: 40, y: 10 }],
      defaultParams: {},
      render: (block: any) => {
        const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        rect.classList.add('nf-block');
        block.group.appendChild(rect);
      },
    };

    const b1 = renderer.createBlock('testui', 0, 0, { id: 'b1' });
    const b2 = renderer.createBlock('testui', 100, 0, { id: 'b2' });
    const out = b1.group.querySelector('.port.out');
    const inp = b2.group.querySelector('.port.in');
    expect(out).not.toBeNull();
    expect(inp).not.toBeNull();

    out.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    expect(state.pendingPort).not.toBeNull();

    inp.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    expect(state.pendingPort).toBeNull();
    expect(state.connections.length).toBe(1);
    const conn = state.connections[0];
    expect(conn.from).toBe('b1');
    expect(conn.to).toBe('b2');

    delete templates.blockTemplates.testui;
  });
});
//=============================================================================
