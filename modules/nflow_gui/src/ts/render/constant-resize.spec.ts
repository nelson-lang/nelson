//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { makeInteractionHandlers } from './interaction/renderer-interaction';
import { makeBlockCreation } from './block/renderer-block-create';
import { blockTemplates } from './block/templates';
import { state } from '../app/state';
const ns = 'http://www.w3.org/2000/svg';
test('resizing a constant persists its size and repositions its output', () => {
  state.locked = false;
  const body = document.createElementNS(ns, 'rect');
  body.classList.add('nf-block');
  const group = document.createElementNS(ns, 'g');
  group.appendChild(body);
  const handle = document.createElementNS(ns, 'rect');
  const port = { type: 'out', x: 80, y: 40 };
  const block: any = {
    id: 'c',
    type: 'constant',
    width: 80,
    height: 80,
    params: {},
    group,
    resizeHandle: handle,
    ports: [port],
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    state: { dirtyBlocks: new Set() },
    updatePortElement: vi.fn(),
    updateBlockLabel: vi.fn(),
    updateConnections: vi.fn(),
  };
  makeInteractionHandlers(ctx).enableResize(block, handle);
  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 }));
  handle.dispatchEvent(
    new PointerEvent('pointermove', { clientX: 120, clientY: -40, pointerId: 1 })
  );
  handle.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1 }));
  expect(block.params).toMatchObject({ width: 200, height: 40 });
  expect(port).toMatchObject({ x: 200, y: 20 });
  expect(body.getAttribute('width')).toBe('200');
  expect(ctx.updatePortElement).toHaveBeenCalledWith(port);
  expect(ctx.updateConnections).toHaveBeenCalledWith(true);
});
test('loading a constant restores its size and output location', () => {
  const previous = (blockTemplates as any).constant;
  (blockTemplates as any).constant = {
    width: 80,
    height: 80,
    inputs: [],
    outputs: [{ x: 80, y: 40, side: 'right' }],
    defaultParams: {},
    render: () => {},
  };
  try {
    const ctx: any = {
      state: { nextId: 1, blocks: new Map(), routingDirty: false },
      blockLayer: document.createElementNS(ns, 'g'),
      createPortCircle: () => document.createElementNS(ns, 'circle'),
      updateBlockTransform: vi.fn(),
      enableDrag: vi.fn(),
      enableSelection: vi.fn(),
      enableResize: vi.fn(),
      updateConnections: vi.fn(),
    };
    const block: any = makeBlockCreation(ctx).createBlock('constant', 0, 0, {
      params: { width: 200, height: 40 },
    });
    expect(block.width).toBe(200);
    expect(block.height).toBe(40);
    expect(block.ports.find((port: any) => port.type === 'out')).toMatchObject({ x: 200, y: 20 });
  } finally {
    (blockTemplates as any).constant = previous;
  }
});

test('resizing an operator keeps its ports and symbols aligned', () => {
  state.locked = false;
  const group = document.createElementNS(ns, 'g');
  const body = document.createElementNS(ns, 'rect');
  body.classList.add('nf-block');
  group.appendChild(body);
  for (let i = 0; i < 3; i++) {
    const text = document.createElementNS(ns, 'text');
    text.classList.add('sum-sign');
    group.appendChild(text);
  }
  const handle = document.createElementNS(ns, 'rect');
  const ports = [
    { type: 'in', index: 0 },
    { type: 'in', index: 1 },
    { type: 'in', index: 2 },
    { type: 'out', index: 0 },
  ];
  const block: any = {
    id: 'sum',
    type: 'sum',
    width: 30,
    height: 60,
    params: {},
    group,
    resizeHandle: handle,
    ports,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    state: { dirtyBlocks: new Set() },
    updatePortElement: vi.fn(),
    updateBlockLabel: vi.fn(),
    updateConnections: vi.fn(),
  };
  makeInteractionHandlers(ctx).enableResize(block, handle);
  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 20, clientY: 60, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1 }));
  expect(block.params).toMatchObject({ width: 50, height: 120 });
  expect(ports.map((p: any) => [p.x, p.y])).toEqual([
    [0, 20],
    [0, 60],
    [0, 100],
    [50, 60],
  ]);
  expect(Array.from(group.querySelectorAll('.sum-sign')).map((el) => el.getAttribute('y'))).toEqual(
    ['20', '60', '100']
  );
});

test('resizing a two-output function keeps its symbol and ports aligned', () => {
  state.locked = false;
  const group = document.createElementNS(ns, 'g');
  const body = document.createElementNS(ns, 'rect');
  body.classList.add('nf-block');
  group.appendChild(body);
  const symbol = document.createElementNS(ns, 'text');
  symbol.classList.add('nf-function-symbol');
  group.appendChild(symbol);
  const handle = document.createElementNS(ns, 'rect');
  const ports = [
    { type: 'in', index: 0 },
    { type: 'out', index: 0 },
    { type: 'out', index: 1 },
  ];
  const block: any = {
    id: 'trig',
    type: 'trigFunction',
    width: 80,
    height: 80,
    params: { Function: 'sincos' },
    group,
    resizeHandle: handle,
    ports,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    state: { dirtyBlocks: new Set() },
    updatePortElement: vi.fn(),
    updateBlockLabel: vi.fn(),
    updateConnections: vi.fn(),
  };
  makeInteractionHandlers(ctx).enableResize(block, handle);
  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 40, clientY: 20, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1 }));
  expect(block.params).toMatchObject({ width: 120, height: 100 });
  expect(ports.map((p: any) => [p.x, p.y])).toEqual([
    [0, 50],
    [120, 40],
    [120, 60],
  ]);
  expect(symbol.getAttribute('x')).toBe('60');
  expect(symbol.getAttribute('y')).toBe('50');
});

test('resizing a gain updates its triangle and connected ports', () => {
  state.locked = false;
  const group = document.createElementNS(ns, 'g');
  const triangle = document.createElementNS(ns, 'polygon');
  triangle.classList.add('gain-triangle');
  group.appendChild(triangle);
  const handle = document.createElementNS(ns, 'rect');
  const ports = [
    { type: 'in', index: 0 },
    { type: 'out', index: 0 },
  ];
  const block: any = {
    id: 'g',
    type: 'gain',
    width: 72,
    height: 72,
    params: {},
    group,
    resizeHandle: handle,
    ports,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    state: { dirtyBlocks: new Set() },
    updatePortElement: vi.fn(),
    updateBlockLabel: vi.fn(),
    updateConnections: vi.fn(),
  };
  makeInteractionHandlers(ctx).enableResize(block, handle);
  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 72, clientY: 0, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1 }));
  expect(block.params).toMatchObject({ width: 144, height: 72 });
  expect(ports.map((p: any) => [p.x, p.y])).toEqual([
    [0, 36],
    [144, 36],
  ]);
  expect(triangle.getAttribute('points')).toBe('12,66 12,6 132,36');
  expect(ctx.updateConnections).toHaveBeenCalledWith(true);
});

test('loading a gain restores its size and port locations', () => {
  const previous = (blockTemplates as any).gain;
  (blockTemplates as any).gain = {
    width: 72,
    height: 72,
    inputs: [{ x: 0, y: 36, side: 'left' }],
    outputs: [{ x: 72, y: 36, side: 'right' }],
    defaultParams: {},
    render: () => {},
  };
  try {
    const ctx: any = {
      state: { nextId: 1, blocks: new Map(), routingDirty: false },
      blockLayer: document.createElementNS(ns, 'g'),
      createPortCircle: () => document.createElementNS(ns, 'circle'),
      updateBlockTransform: vi.fn(),
      enableDrag: vi.fn(),
      enableSelection: vi.fn(),
      enableResize: vi.fn(),
      updateConnections: vi.fn(),
    };
    const block: any = makeBlockCreation(ctx).createBlock('gain', 0, 0, {
      params: { width: 144, height: 72 },
    });
    expect([block.width, block.height]).toEqual([144, 72]);
    expect(block.ports.map((p: any) => [p.x, p.y])).toEqual([
      [0, 36],
      [144, 36],
    ]);
    expect(block.resizeHandle).toBeTruthy();
  } finally {
    (blockTemplates as any).gain = previous;
  }
});

test('resizing a bus creator moves its ports and label', () => {
  state.locked = false;
  const group = document.createElementNS(ns, 'g');
  const body = document.createElementNS(ns, 'rect');
  body.classList.add('nf-block');
  group.appendChild(body);
  const formula = document.createElementNS(ns, 'g');
  formula.classList.add('nf-block-formula');
  const label = document.createElementNS(ns, 'text');
  for (const dy of ['0', '14']) {
    const line = document.createElementNS(ns, 'tspan');
    line.setAttribute('dy', dy);
    label.appendChild(line);
  }
  formula.appendChild(label);
  group.appendChild(formula);
  const handle = document.createElementNS(ns, 'rect');
  const ports = [
    { type: 'in', index: 0 },
    { type: 'in', index: 1 },
    { type: 'out', index: 0 },
  ];
  const block: any = {
    id: 'bus',
    type: 'busCreator',
    width: 80,
    height: 70,
    params: {},
    group,
    resizeHandle: handle,
    ports,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    state: { dirtyBlocks: new Set() },
    updatePortElement: vi.fn(),
    updateBlockLabel: vi.fn(),
    updateConnections: vi.fn(),
  };
  makeInteractionHandlers(ctx).enableResize(block, handle);
  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 70, clientY: 50, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1 }));
  expect(block.params).toMatchObject({ width: 150, height: 120 });
  expect(ports.map((p: any) => [p.x, p.y])).toEqual([
    [0, 30],
    [0, 90],
    [150, 60],
  ]);
  expect(label.getAttribute('x')).toBe('75');
  expect(label.getAttribute('y')).toBe('53');
});

test('generic block resizing moves ordinary and physical ports with the artwork', () => {
  state.locked = false;
  const group = document.createElementNS(ns, 'g');
  const artwork = document.createElementNS(ns, 'rect');
  group.appendChild(artwork);
  const input = { type: 'in', index: 0, x: 0, y: 20, wireX: 0, wireY: 20 };
  const output = { type: 'out', index: 0, x: 80, y: 20, wireX: 80, wireY: 20 };
  const pin = {
    type: 'phys',
    index: 0,
    x: 40,
    y: 40,
    wireX: 40,
    wireY: 40,
    el: document.createElementNS(ns, 'circle'),
  };
  const handle = document.createElementNS(ns, 'rect');
  const block: any = {
    id: 'other',
    type: 'custom',
    width: 80,
    height: 40,
    params: {},
    group,
    resizeHandle: handle,
    ports: [input, output, pin],
    physicalPins: [{ x: 40, y: 40 }],
    resizeVisualWidth: 80,
    resizeVisualHeight: 40,
    resizeVisuals: [{ el: artwork, transform: '' }],
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    state: { dirtyBlocks: new Set() },
    updatePortElement: vi.fn(),
    updateBlockTransform: vi.fn(),
    updateConnections: vi.fn(),
  };
  makeInteractionHandlers(ctx).enableResize(block, handle);
  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 80, clientY: 40, pointerId: 1 }));
  handle.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1 }));
  expect(block.params).toMatchObject({ width: 160, height: 80 });
  expect(input).toMatchObject({ x: 0, y: 40 });
  expect(output).toMatchObject({ x: 160, y: 40 });
  expect(pin).toMatchObject({ x: 80, y: 80 });
  expect(pin.el.getAttribute('cx')).toBe('80');
  expect(artwork.getAttribute('transform')).toBe('scale(2, 2)');
  expect(ctx.updateConnections).toHaveBeenCalledWith(true);
  const refreshed = document.createElementNS(ns, 'rect');
  artwork.replaceWith(refreshed);
  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 2 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 40, clientY: 20, pointerId: 2 }));
  handle.dispatchEvent(new PointerEvent('pointerup', { pointerId: 2 }));
  expect(refreshed.getAttribute('transform')).toBe('scale(1.25, 1.25)');
  expect(output).toMatchObject({ x: 200, y: 50 });
});

test('generic block restores size and scaled port positions', () => {
  const previous = (blockTemplates as any).resizableExample;
  (blockTemplates as any).resizableExample = {
    width: 80,
    height: 40,
    inputs: [{ x: 0, y: 20, side: 'left' }],
    outputs: [{ x: 80, y: 20, side: 'right' }],
    defaultParams: {},
    render: () => {},
  };
  try {
    const ctx: any = {
      state: { nextId: 1, blocks: new Map(), routingDirty: false },
      blockLayer: document.createElementNS(ns, 'g'),
      createPortCircle: () => document.createElementNS(ns, 'circle'),
      updateBlockTransform: vi.fn(),
      enableDrag: vi.fn(),
      enableSelection: vi.fn(),
      enableResize: vi.fn(),
      updateConnections: vi.fn(),
    };
    const block: any = makeBlockCreation(ctx).createBlock('resizableExample', 0, 0, {
      params: { width: 160, height: 80 },
    });
    expect([block.width, block.height]).toEqual([160, 80]);
    expect(block.ports.map((p: any) => [p.x, p.y])).toEqual([
      [0, 40],
      [160, 40],
    ]);
    expect(block.resizeHandle).toBeTruthy();
  } finally {
    (blockTemplates as any).resizableExample = previous;
  }
});
