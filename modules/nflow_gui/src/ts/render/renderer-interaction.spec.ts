//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for renderer-interaction
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/interaction/renderer-interaction.js');
  document.body.innerHTML = '';
});
//=============================================================================
test('enableSelection calls selectBlock on click when ctrlKey not pressed', () => {
  const select: any = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const ctx: any = { selectBlock: select };
  const handlers = mod.makeInteractionHandlers(ctx as any);
  const block: any = { id: 'b1' };
  const handle = document.createElement('div');
  handlers.enableSelection(block, handle);
  const ev = new MouseEvent('click');
  handle.dispatchEvent(ev);
  expect(select.calls.length).toBe(1);
  expect(select.calls[0][0]).toBe('b1');
});
//=============================================================================
test('enableResize updates block size on pointer events and triggers updateConnections', () => {
  const updateConnections: any = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const clientToSvg = (x: number, y: number) => ({ x, y });
  const state: any = { fastRouting: false, routingDirty: false, dirtyBlocks: new Set() };
  const ctx: any = {
    clientToSvg,
    updateCommentLayout: (() => {
      const s: any = (...a: any[]) => s.calls.push(a);
      s.calls = [];
      return s;
    })(),
    updateScopeLayout: (() => {
      const s: any = (...a: any[]) => s.calls.push(a);
      s.calls = [];
      return s;
    })(),
    updateConnections,
    state,
  };
  const handlers = mod.makeInteractionHandlers(ctx as any);
  const block: any = { id: 'b1', width: 10, height: 10, type: 'comment' };
  const handle = document.createElement('div');
  handlers.enableResize(block, handle);
  const down = new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 });
  handle.dispatchEvent(down);
  const move = new PointerEvent('pointermove', { clientX: 5, clientY: 5, pointerId: 1 });
  handle.dispatchEvent(move);
  expect(block.width).toBeGreaterThan(10);
  expect(state.fastRouting).toBe(true);
  const up = new PointerEvent('pointerup', { pointerId: 1 });
  handle.dispatchEvent(up);
  expect(state.fastRouting).toBe(false);
  expect(updateConnections.calls.length).toBeGreaterThan(0);
});
//=============================================================================
test('enableResize does nothing when state.locked', async () => {
  const updateConnections: any = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const clientToSvg = (x: number, y: number) => ({ x, y });
  const state: any = { fastRouting: false, routingDirty: false, dirtyBlocks: new Set() };
  const ctx: any = {
    clientToSvg,
    updateCommentLayout: () => {},
    updateScopeLayout: () => {},
    updateConnections,
    state,
  };
  const handlers = mod.makeInteractionHandlers(ctx as any);
  const block: any = { id: 'b1', width: 10, height: 10, type: 'comment' };
  const handle = document.createElement('div');
  handlers.enableResize(block, handle);
  const sMod = await import('../../../dist/app/state.js');
  sMod.state.locked = true;

  const down = new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 5 });
  handle.dispatchEvent(down);
  const move = new PointerEvent('pointermove', { clientX: 5, clientY: 5, pointerId: 5 });
  handle.dispatchEvent(move);
  expect(block.width).toBe(10);
  expect(state.fastRouting).toBe(false);

  sMod.state.locked = false;
});
//=============================================================================
test('enableSubsystemOpen triggers on long press and dblclick', async () => {
  const onOpen: any = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const state: any = {};
  const ctx: any = { onOpenSubsystem: onOpen, state };
  const handlers = mod.makeInteractionHandlers(ctx as any);
  const block: any = { type: FieldNames.kSubsystem, id: 's1' };
  const handle = document.createElement('div');
  handlers.enableSubsystemOpen(block, handle);
  const dbl = new MouseEvent('dblclick', { bubbles: true, cancelable: true });
  handle.dispatchEvent(dbl);
  expect(onOpen.calls.length).toBe(1);
  // long press (wait slightly more than LONG_PRESS_MS)
  // attach the element to the document so events behave consistently
  document.body.appendChild(handle);
  const down: any = new Event('pointerdown', { bubbles: true, cancelable: true });
  down.clientX = 0;
  down.clientY = 0;
  down.pointerId = 2;
  down.button = 0;
  handle.dispatchEvent(down);
  await new Promise((r) => setTimeout(r, 600));
  expect(onOpen.calls.length).toBe(2);
});
//=============================================================================
