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
import * as FieldNames from '../FieldNames.js';
//=============================================================================
beforeEach(() => {
  // @ts-ignore
  global.requestAnimationFrame = (cb) => {
    cb();
    return 0;
  };
});
//=============================================================================
test('enableResize updates block size and calls updates', () => {
  const group = document.createElement('g');
  const block: any = { id: 'b1', x: 0, y: 0, width: 10, height: 10, group };
  const handle = document.createElement('div');
  const state: any = { fastRouting: false, routingDirty: false, dirtyBlocks: new Set() };
  const ctx: any = {
    state,
    clientToSvg: (x: number, y: number) => ({ x, y }),
    updateScopeLayout: vi.fn(),
    updateConnections: vi.fn(),
  };
  const h = makeInteractionHandlers(ctx);
  h.enableResize(block, handle);

  const down = new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 1 }) as any;
  handle.dispatchEvent(down);
  const move = new PointerEvent('pointermove', { clientX: 5, clientY: 6, pointerId: 1 }) as any;
  handle.dispatchEvent(move);
  expect(block.width).toBeGreaterThan(10);
  expect(block.height).toBeGreaterThan(10);
  expect(ctx.updateScopeLayout).toHaveBeenCalled();
  expect(ctx.updateConnections).toHaveBeenCalled();

  const up = new PointerEvent('pointerup', { pointerId: 1 }) as any;
  handle.dispatchEvent(up);
  expect(state.fastRouting).toBe(false);
});
//=============================================================================
test('enableSelection calls selectBlock when clicked without ctrl', () => {
  const group = document.createElement('g');
  const block: any = { id: 'b2', group };
  const handle = document.createElement('div');
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    selectBlock: vi.fn(),
    state: {},
  };
  const h = makeInteractionHandlers(ctx);
  h.enableSelection(block, handle);
  const click = new MouseEvent('click', { bubbles: true }) as any;
  handle.dispatchEvent(click);
  expect(ctx.selectBlock).toHaveBeenCalledWith('b2');
  const ctrlClick = new MouseEvent('click', { bubbles: true, ctrlKey: true }) as any;
  handle.dispatchEvent(ctrlClick);
  // should not call again for ctrlKey
  expect(ctx.selectBlock).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('enableSubsystemOpen dblclick and long press trigger onOpenSubsystem', (done) => {
  const group = document.createElement('g');
  const block: any = { id: 's1', type: FieldNames.kSubsystem, group };
  const handle = document.createElement('div');
  const ctx: any = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    onOpenSubsystem: vi.fn(),
    state: {},
  };
  const h = makeInteractionHandlers(ctx);
  h.enableSubsystemOpen(block, handle);

  const dbl = new MouseEvent('dblclick', { bubbles: true }) as any;
  handle.dispatchEvent(dbl);
  expect(ctx.onOpenSubsystem).toHaveBeenCalled();

  const down = new PointerEvent('pointerdown', { clientX: 0, clientY: 0, pointerId: 5 }) as any;
  handle.dispatchEvent(down);
  // wait longer than LONG_PRESS_MS
  setTimeout(() => {
    expect(ctx.onOpenSubsystem).toHaveBeenCalled();
    const up = new PointerEvent('pointerup', { pointerId: 5 }) as any;
    handle.dispatchEvent(up);
    done();
  }, 600);
});
//=============================================================================
test('enableScopeHover sets scopeHoverX and calls renderScope via import', () => {
  const group = document.createElement('g');
  const block: any = { id: 'sc', type: 'scope', x: 0, group };
  const ctx: any = { clientToSvg: (x: number, y: number) => ({ x, y }), state: {} };
  const h = makeInteractionHandlers(ctx);
  h.enableScopeHover(block);
  const move = new PointerEvent('pointermove', { clientX: 10, clientY: 0 }) as any;
  block.group.dispatchEvent(move);
  expect(block.scopeHoverX).not.toBeNull();
  const leave = new PointerEvent('pointerleave', {}) as any;
  block.group.dispatchEvent(leave);
  expect(block.scopeHoverX).toBeNull();
});
//=============================================================================
