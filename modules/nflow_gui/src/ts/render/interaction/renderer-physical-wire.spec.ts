//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Acausal physical wiring: resolving a pin, arming a drag, and the undirected
// connect / domain / same-block rules on drop.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { makePhysicalWireHandlers } from './renderer-physical-wire.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
//=============================================================================
function makeBlock(id: string, pins: Array<{ name: string; domain: string }>) {
  const group = document.createElementNS(ns, 'g');
  group.setAttribute('data-block-id', id);
  const ports: any[] = [];
  const pinEls: Element[] = [];
  pins.forEach((p, i) => {
    const el = document.createElementNS(ns, 'circle');
    el.setAttribute('class', 'physical-pin');
    el.setAttribute('data-pin', p.name);
    el.setAttribute('data-domain', p.domain);
    group.appendChild(el);
    ports.push({ name: p.name, type: 'phys', index: i, domain: p.domain, el });
    pinEls.push(el);
  });
  return { id, group, ports, pinEls } as any;
}
//=============================================================================
function makeCtx(blocks: any[]) {
  const svg = document.createElementNS(ns, 'svg');
  document.body.appendChild(svg);
  const overlayLayer = document.createElementNS(ns, 'g');
  svg.appendChild(overlayLayer);
  const map = new Map<string, any>();
  blocks.forEach((b) => {
    map.set(b.id, b);
    svg.appendChild(b.group);
  });
  const calls: any = { created: [] as any[], errors: [] as string[] };
  const ctx: any = {
    svg,
    overlayLayer,
    state: { blocks: map, locked: false },
    clientToSvg: (x: number, y: number) => ({ x, y }),
    onConnectionError: (m: string) => calls.errors.push(m),
    createConnection: (from: string, to: string, toIndex = 0, fromIndex = 0) => {
      calls.created.push({ from, to, toIndex, fromIndex });
      return {};
    },
  };
  return { ctx, svg, calls };
}
//=============================================================================
function pointer(target: Element, x: number, y: number, pointerId = 1, button = 0): any {
  return { pointerId, button, clientX: x, clientY: y, target };
}
//=============================================================================
describe('makePhysicalWireHandlers', () => {
  afterEach(() => {
    document.body.innerHTML = '';
    delete (document as any).elementFromPoint;
  });
  const stubDropTarget = (el: Element | null) => {
    (document as any).elementFromPoint = () => el;
  };

  it('resolves a physical pin to its block, index and domain', () => {
    const b = makeBlock('R1', [{ name: 'p', domain: 'electrical' }]);
    const { ctx } = makeCtx([b]);
    const h = makePhysicalWireHandlers(ctx);
    expect(h.resolvePhysicalPin(b.pinEls[0])).toMatchObject({
      blockId: 'R1',
      index: 0,
      domain: 'electrical',
    });
  });

  it('arms only on a physical pin', () => {
    const b = makeBlock('R1', [{ name: 'p', domain: 'electrical' }]);
    const { ctx } = makeCtx([b]);
    const h = makePhysicalWireHandlers(ctx);
    const notPin = document.createElementNS(ns, 'rect');
    expect(h.armPhysicalWireDrag(pointer(notPin, 0, 0))).toBe(false);
    expect(h.armPhysicalWireDrag(pointer(b.pinEls[0], 0, 0))).toBe(true);
    h.cancelPhysicalWireDrag();
  });

  it('connects two same-domain pins on different components on drop', () => {
    const b1 = makeBlock('R1', [{ name: 'a', domain: 'electrical' }]);
    const b2 = makeBlock('C1', [{ name: 'b', domain: 'electrical' }]);
    const { ctx, calls } = makeCtx([b1, b2]);
    const h = makePhysicalWireHandlers(ctx);

    stubDropTarget(b2.pinEls[0]);
    expect(h.armPhysicalWireDrag(pointer(b1.pinEls[0], 0, 0))).toBe(true);
    window.dispatchEvent(new PointerEvent('pointermove', { pointerId: 1, clientX: 40, clientY: 0 }));
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1, clientX: 200, clientY: 40 }));
    expect(calls.created).toContainEqual({ from: 'R1', to: 'C1', toIndex: 0, fromIndex: 0 });
  });

  it('refuses to connect pins of different domains', () => {
    const b1 = makeBlock('R1', [{ name: 'a', domain: 'electrical' }]);
    const b2 = makeBlock('M1', [{ name: 'b', domain: 'mechanical' }]);
    const { ctx, calls } = makeCtx([b1, b2]);
    const h = makePhysicalWireHandlers(ctx);

    stubDropTarget(b2.pinEls[0]);
    h.armPhysicalWireDrag(pointer(b1.pinEls[0], 0, 0));
    window.dispatchEvent(new PointerEvent('pointermove', { pointerId: 1, clientX: 40, clientY: 0 }));
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1, clientX: 200, clientY: 40 }));
    expect(calls.created.length).toBe(0);
    expect(calls.errors.join(' ')).toMatch(/same physical domain/i);
  });

  it('refuses to connect two pins of the same component', () => {
    const b1 = makeBlock('R1', [
      { name: 'a', domain: 'electrical' },
      { name: 'b', domain: 'electrical' },
    ]);
    const { ctx, calls } = makeCtx([b1]);
    const h = makePhysicalWireHandlers(ctx);

    stubDropTarget(b1.pinEls[1]);
    h.armPhysicalWireDrag(pointer(b1.pinEls[0], 0, 0));
    window.dispatchEvent(new PointerEvent('pointermove', { pointerId: 1, clientX: 40, clientY: 0 }));
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1, clientX: 200, clientY: 40 }));
    expect(calls.created.length).toBe(0);
    expect(calls.errors.join(' ')).toMatch(/different components/i);
  });

  it('does nothing without travel (a plain click)', () => {
    const b = makeBlock('R1', [{ name: 'p', domain: 'electrical' }]);
    const { ctx, calls } = makeCtx([b]);
    const h = makePhysicalWireHandlers(ctx);
    expect(h.armPhysicalWireDrag(pointer(b.pinEls[0], 0, 0))).toBe(true);
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1, clientX: 0, clientY: 0 }));
    expect(calls.created.length).toBe(0);
  });
});
//=============================================================================
