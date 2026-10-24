//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/interaction/renderer-drag.js');
  document.body.innerHTML = '';
});
//=============================================================================
test('does not start drag before threshold', async () => {
  const block: any = {
    id: 'b1',
    x: 0,
    y: 0,
    width: 100,
    height: 50,
    group: document.createElement('div'),
  };
  const handle = document.createElement('div');
  const updateBlockTransform = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const state: any = {
    blocks: new Map([['b1', block]]),
    dirtyBlocks: new Set(),
    isPinching: false,
    deleteMode: false,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    updateBlockTransform,
    state,
  };
  const handlers = mod.makeDragHandlers(ctx);
  handlers.enableDrag(block, handle);

  const down = new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 });
  handle.dispatchEvent(down);
  // small move under threshold
  const move = new PointerEvent('pointermove', { clientX: 3, clientY: 4, pointerId: 1 });
  handle.dispatchEvent(move);
  await new Promise((r) => setTimeout(r, 30));
  expect(updateBlockTransform.calls.length).toBe(0);
  expect(state.isDragging).not.toBe(true);
});
//=============================================================================
test('starts drag after threshold, updates position and finishes with commit', async () => {
  const block: any = {
    id: 'b2',
    x: 0,
    y: 0,
    width: 100,
    height: 50,
    group: document.createElement('div'),
  };
  const handle = document.createElement('div');
  const updateBlockTransform = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const updateConnections = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const signalDiagramChanged = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const state: any = {
    blocks: new Map([['b2', block]]),
    dirtyBlocks: new Set(),
    isPinching: false,
    deleteMode: false,
    selectedIds: new Set(),
    isDragging: false,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    updateBlockTransform,
    updateConnections,
    detectBlockOverlap: () => false,
    signalDiagramChanged,
    state,
  };
  const handlers = mod.makeDragHandlers(ctx);
  handlers.enableDrag(block, handle);

  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 2 }));
  // move past threshold
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 20, clientY: 0, pointerId: 2 }));
  // wait for RAF callback
  await new Promise((r) => setTimeout(r, 50));
  expect(updateBlockTransform.calls.length).toBeGreaterThan(0);
  expect(state.isDragging).toBe(true);
  expect(state.fastRouting).toBe(true);
  expect(state.routingDirty).toBe(true);

  handle.dispatchEvent(new PointerEvent('pointerup', { clientX: 20, clientY: 0, pointerId: 2 }));
  await new Promise((r) => setTimeout(r, 20));
  expect(state.isDragging).toBe(false);
  expect(updateConnections.calls.length).toBeGreaterThan(0);
  expect(signalDiagramChanged.calls.length).toBeGreaterThan(0);
});
//=============================================================================
test('does not start drag when state.locked', async () => {
  const block: any = {
    id: 'b4',
    x: 0,
    y: 0,
    width: 100,
    height: 50,
    group: document.createElement('div'),
  };
  const handle = document.createElement('div');
  const updateBlockTransform = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const state: any = {
    blocks: new Map([['b4', block]]),
    dirtyBlocks: new Set(),
    isPinching: false,
    deleteMode: false,
    isDragging: false,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    updateBlockTransform,
    state,
  };
  const handlers = mod.makeDragHandlers(ctx);
  handlers.enableDrag(block, handle);

  // lock global state
  const sMod = await import('../../../dist/app/state.js');
  sMod.state.locked = true;

  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 4 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 20, clientY: 0, pointerId: 4 }));
  await new Promise((r) => setTimeout(r, 50));
  expect(updateBlockTransform.calls.length).toBe(0);
  expect(state.isDragging).toBe(false);

  sMod.state.locked = false;
});
//=============================================================================
test('rollback on collision calls updateBlockTransform with animated true', async () => {
  const block: any = {
    id: 'b3',
    x: 0,
    y: 0,
    width: 100,
    height: 50,
    group: document.createElement('div'),
  };
  const handle = document.createElement('div');
  const calls: any[] = [];
  const updateBlockTransform = (b: any, animated?: boolean) => {
    calls.push({ id: b.id, animated: !!animated });
  };
  const state: any = {
    blocks: new Map([['b3', block]]),
    dirtyBlocks: new Set(),
    isPinching: false,
    deleteMode: false,
    isDragging: false,
  };
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    updateBlockTransform,
    detectBlockOverlap: () => true,
    state,
  };
  const handlers = mod.makeDragHandlers(ctx);
  handlers.enableDrag(block, handle);

  handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 3 }));
  handle.dispatchEvent(new PointerEvent('pointermove', { clientX: 20, clientY: 0, pointerId: 3 }));
  await new Promise((r) => setTimeout(r, 50));
  // now pointerup triggers rollback
  handle.dispatchEvent(new PointerEvent('pointerup', { clientX: 20, clientY: 0, pointerId: 3 }));
  await new Promise((r) => setTimeout(r, 30));
  // collision branch should clear routingDirty and call updateBlockTransform at least once
  expect(state.routingDirty).toBe(false);
  expect(calls.length).toBeGreaterThan(0);
});
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test } from 'vitest';
import { makeDragHandlers } from './interaction/renderer-drag.js';
//=============================================================================
describe('renderer-drag basic', () => {
  test('makeDragHandlers returns enableDrag function', () => {
    const ctx: any = {
      state: { blocks: new Map() },
      clientToSvg: (x: number, y: number) => ({ x, y }),
    };
    const handlers = makeDragHandlers(ctx as any);
    expect(typeof handlers.enableDrag).toBe('function');
    const block: any = { id: 'b1', group: document.createElement('div') };
    const handle = document.createElement('div');
    handlers.enableDrag(block, handle);
  });

  test('drag can move a block left of the world origin', async () => {
    const block: any = {
      id: 'signed-drag',
      x: 0,
      y: 0,
      width: 100,
      height: 50,
      group: document.createElement('div'),
    };
    const handle = document.createElement('div');
    handle.setPointerCapture = handle.setPointerCapture || (() => {});
    const state: any = {
      blocks: new Map([[block.id, block]]),
      dirtyBlocks: new Set(),
      isPinching: false,
      deleteMode: false,
      selectedIds: new Set(),
    };
    const ctx: any = {
      clientToSvg: (x: number, y: number) => ({ x, y }),
      updateBlockTransform: () => {},
      updateConnections: () => {},
      state,
    };
    const handlers = makeDragHandlers(ctx as any);
    handlers.enableDrag(block, handle);

    handle.dispatchEvent(
      new PointerEvent('pointerdown', { clientX: 20, clientY: 20, pointerId: 7 })
    );
    handle.dispatchEvent(
      new PointerEvent('pointermove', { clientX: -20, clientY: 20, pointerId: 7 })
    );
    await new Promise((r) => setTimeout(r, 50));

    expect(block.x).toBe(-40);
    expect(block.y).toBe(0);
    expect(state.dirtyBlocks.has(block.id)).toBe(true);
  });

  test('dashboard drag is disabled only while a simulation is active', async () => {
    const block: any = {
      id: 'dashboard-control',
      type: 'dashboardSlider',
      x: 0,
      y: 0,
      width: 100,
      height: 50,
      group: document.createElement('div'),
    };
    const handle = document.createElement('div');
    const control = document.createElement('button');
    block.group.append(handle, control);
    handle.setPointerCapture = handle.setPointerCapture || (() => {});
    const state: any = {
      blocks: new Map([[block.id, block]]),
      dirtyBlocks: new Set(),
      isPinching: false,
      deleteMode: false,
      selectedIds: new Set(),
      simulationActive: true,
    };
    const ctx: any = {
      clientToSvg: (x: number, y: number) => ({ x, y }),
      updateBlockTransform: () => {},
      updateConnections: () => {},
      state,
    };
    makeDragHandlers(ctx as any).enableDrag(block, handle);

    handle.dispatchEvent(new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 8 }));
    handle.dispatchEvent(
      new PointerEvent('pointermove', { clientX: 40, clientY: 0, pointerId: 8 })
    );
    await new Promise((resolve) => setTimeout(resolve, 30));
    expect(block.x).toBe(0);
    expect(state.isDragging).not.toBe(true);

    state.simulationActive = false;
    control.dispatchEvent(
      new PointerEvent('pointerdown', {
        bubbles: true,
        clientX: 0,
        clientY: 0,
        pointerId: 9,
      })
    );
    handle.dispatchEvent(
      new PointerEvent('pointermove', { clientX: 40, clientY: 0, pointerId: 9 })
    );
    await new Promise((resolve) => setTimeout(resolve, 30));
    expect(block.x).toBe(0);

    handle.dispatchEvent(
      new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 10 })
    );
    handle.dispatchEvent(
      new PointerEvent('pointermove', { clientX: 40, clientY: 0, pointerId: 10 })
    );
    await new Promise((resolve) => setTimeout(resolve, 30));
    expect(block.x).toBe(40);
    expect(state.isDragging).toBe(true);
  });

  test('finishes a dashboard drag when pointerup occurs outside its SVG handle', async () => {
    const block: any = {
      id: 'dashboard-combo',
      type: 'dashboardComboBox',
      x: 0,
      y: 0,
      width: 160,
      height: 25,
      group: document.createElement('div'),
    };
    const handle = document.createElement('div');
    handle.setPointerCapture = handle.setPointerCapture || (() => {});
    const state: any = {
      blocks: new Map([[block.id, block]]),
      dirtyBlocks: new Set(),
      isPinching: false,
      deleteMode: false,
      selectedIds: new Set(),
      simulationActive: false,
    };
    const ctx: any = {
      clientToSvg: (x: number, y: number) => ({ x, y }),
      updateBlockTransform: () => {},
      updateConnections: () => {},
      detectBlockOverlap: () => false,
      signalDiagramChanged: vi.fn(),
      state,
    };
    makeDragHandlers(ctx as any).enableDrag(block, handle);

    handle.dispatchEvent(
      new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 12 })
    );
    handle.dispatchEvent(
      new PointerEvent('pointermove', { clientX: 40, clientY: 0, pointerId: 12 })
    );
    await new Promise((resolve) => setTimeout(resolve, 30));
    expect(state.isDragging).toBe(true);

    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 12 }));
    expect(state.isDragging).toBe(false);
    expect(ctx.signalDiagramChanged).toHaveBeenCalledOnce();
  });
});
//=============================================================================
import { fileURLToPath } from 'url';
import path from 'path';
import { createRequire } from 'module';
//=============================================================================
const require = createRequire(import.meta.url);
let makeDrag: any;
beforeAll(async () => {
  makeDrag = (await import('../../../dist/render/interaction/renderer-drag.js')).makeDragHandlers;
});
//=============================================================================
function makeCtx() {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
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

  const ctx: any = {
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    clientToSvg: (x: number, y: number) => ({ x: x - 10, y: y - 10 }),
    updateBlockTransform: () => {},
    updateConnections: () => {},
    detectBlockOverlap: () => false,
    updateSelectionBox: () => {},
    signalDiagramChanged: () => {},
  };

  return { ctx, state };
}
//=============================================================================
afterEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
describe('makeDragHandlers (renderer-drag)', () => {
  test('basic drag updates block and marks dirty', async () => {
    const { ctx, state } = makeCtx();

    const block: any = { id: 'b1', x: 10, y: 10, width: 120, height: 60 };
    const dragRect = document.createElement('div');
    const group = document.createElement('div');
    block.group = group;
    block.dragRect = dragRect;

    state.blocks.set(block.id, block);

    const handlers = makeDrag(ctx);
    handlers.enableDrag(block, dragRect);

    const down = new MouseEvent('pointerdown', {
      bubbles: true,
      clientX: 100,
      clientY: 100,
    } as any);
    Object.defineProperty(down, 'pointerId', { value: 1 });
    try {
      dragRect.setPointerCapture = dragRect.setPointerCapture || (() => {});
    } catch (e) {
      dragRect.setPointerCapture = () => {};
    }
    dragRect.dispatchEvent(down);

    // small move (below threshold) should not start drag
    const smallMove = new MouseEvent('pointermove', {
      bubbles: true,
      clientX: 102,
      clientY: 103,
    } as any);
    Object.defineProperty(smallMove, 'pointerId', { value: 1 });
    dragRect.dispatchEvent(smallMove);

    // big move to start the drag
    const bigMove = new MouseEvent('pointermove', {
      bubbles: true,
      clientX: 300,
      clientY: 300,
    } as any);
    Object.defineProperty(bigMove, 'pointerId', { value: 1 });
    // emulate SVG point transformation by overriding createSVGPoint used by clientToSvg in other tests
    (ctx.svg as any).createSVGPoint = () => ({
      x: 0,
      y: 0,
      matrixTransform: () => ({ x: bigMove.clientX - 10, y: bigMove.clientY - 10 }),
    });
    dragRect.dispatchEvent(bigMove);

    const up = new MouseEvent('pointerup', { bubbles: true, clientX: 300, clientY: 300 } as any);
    Object.defineProperty(up, 'pointerId', { value: 1 });
    dragRect.dispatchEvent(up);

    await new Promise<void>((resolve) => {
      setTimeout(() => {
        resolve();
      }, 20);
    });
    // wait a tick for RAF-handled updates
    {
      expect(
        state.dirtyBlocks.has('b1') || state.routingDirty || state.blocks.get('b1').x !== 10
      ).toBeTruthy();
    }
  });
});
//=============================================================================
