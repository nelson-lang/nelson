//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { setupSvgListeners } from './interaction/renderer-events';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('pointermove with wireCornerDrag active calls moveWireCorner and applies paths', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const applied = { called: false };
  const updated = { called: false };
  const ctx: any = {
    svg,
    wireCornerDrag: { active: true, pointerId: 5, conn: { id: 'c' }, index: 1 },
    clientToSvg: (x: number, y: number) => ({ x, y }),
    moveWireCorner: (_conn: any, _index: number, _p: any) => true,
    applyWirePaths: (_m: Map<any, any>) => {
      applied.called = true;
    },
    updateSelectionBox: () => {
      updated.called = true;
    },
  };
  setupSvgListeners(ctx);
  const ev = new PointerEvent('pointermove', { pointerId: 5, clientX: 1, clientY: 2 });
  svg.dispatchEvent(ev);
  expect(applied.called).toBe(true);
  expect(updated.called).toBe(true);
});
//=============================================================================
test('pointermove with marquee active updates marquee rect', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const updated: any = { called: false, args: null };
  const ctx: any = {
    svg,
    marqueeState: { active: true, pointerId: 7, start: { x: 0, y: 0 } },
    clientToSvg: (x: number, y: number) => ({ x, y }),
    updateMarqueeRect: (a: any, b: any) => {
      updated.called = true;
      updated.args = { a, b };
    },
  };
  setupSvgListeners(ctx);
  const ev = new PointerEvent('pointermove', { pointerId: 7, clientX: 10, clientY: 20 });
  svg.dispatchEvent(ev);
  expect(updated.called).toBe(true);
  expect(updated.args.b).toEqual({ x: 10, y: 20 });
});
//=============================================================================
test('pointerup ends wireCornerDrag and dispatches diagramChanged', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const updated = { called: false };
  const ctx: any = {
    svg,
    wireCornerDrag: { active: true, pointerId: 9, conn: { id: 'z' }, index: 2 },
    updateWireCornerHandles: () => {
      updated.called = true;
    },
  };
  let diag = false;
  window.addEventListener('diagramChanged', () => {
    diag = true;
  });
  setupSvgListeners(ctx);
  const ev = new PointerEvent('pointerup', { pointerId: 9 });
  svg.dispatchEvent(ev);
  expect(ctx.wireCornerDrag.active).toBe(false);
  expect(ctx.wireCornerDrag.conn).toBeNull();
  expect(ctx.wireCornerDrag.index).toBe(-1);
  expect(ctx.wireCornerDrag.pointerId).toBeNull();
  expect(updated.called).toBe(true);
  expect(diag).toBe(true);
});
//=============================================================================
test('pointercancel hides marquee and resets state', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const marqueeRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const ctx: any = {
    svg,
    marqueeState: { active: true, pointerId: 11, start: {} },
    marqueeRect,
  };
  setupSvgListeners(ctx);
  const ev = new PointerEvent('pointercancel', { pointerId: 11 });
  marqueeRect.setAttribute('display', 'block');
  svg.dispatchEvent(ev);
  expect(marqueeRect.getAttribute('display')).toBe('none');
  expect(ctx.marqueeState.active).toBe(false);
  expect(ctx.marqueeState.pointerId).toBeNull();
  expect(ctx.marqueeState.start).toBeNull();
});
//=============================================================================
