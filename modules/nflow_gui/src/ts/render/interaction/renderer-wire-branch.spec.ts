//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Wire fan-out (branching): Ctrl/Cmd+drag from a wire onto an input creates a
// second connection from that wire's source; the arm/drag/cancel lifecycle and
// the modifier/self guards.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { makeWireBranchHandlers } from './renderer-wire-branch.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
type P = { x: number; y: number };
//=============================================================================
function makeConn(from: string, to: string, points: P[], extra: any = {}) {
  return {
    from,
    to,
    fromIndex: 0,
    toIndex: 0,
    points: points.map((p) => ({ ...p })),
    path: document.createElementNS(ns, 'path'),
    ...extra,
  } as any;
}
//=============================================================================
function portGroup(blockId: string, type: 'in' | 'out', index: number) {
  const g = document.createElementNS(ns, 'g');
  g.setAttribute('class', 'port-group');
  g.setAttribute('data-block-id', blockId);
  g.setAttribute('data-port-type', type);
  g.setAttribute('data-port-index', String(index));
  return g;
}
//=============================================================================
function makeCtx(connections: any[]) {
  const svg = document.createElementNS(ns, 'svg');
  document.body.appendChild(svg);
  const calls: any = {
    beginPending: 0,
    clearPending: 0,
    created: [] as any[],
    errors: [] as string[],
  };
  const ctx: any = {
    svg,
    state: { connections, locked: false },
    clientToSvg: (x: number, y: number) => ({ x, y }),
    beginPending: () => {
      calls.beginPending += 1;
    },
    clearPending: () => {
      calls.clearPending += 1;
    },
    onConnectionError: (m: string) => calls.errors.push(m),
    // Mirrors the real createConnection outcome needed here: outputs fan out, so
    // the only rejection is an already-occupied input.
    createConnection: (from: string, to: string, toIndex = 0, fromIndex = 0) => {
      const occupied = connections.some(
        (c) => c.to === to && Number(c.toIndex ?? 0) === Number(toIndex)
      );
      if (occupied) {
        calls.errors.push('occupied');
        return null;
      }
      const conn = makeConn(from, to, [], { toIndex, fromIndex });
      connections.push(conn);
      calls.created.push({ from, to, toIndex, fromIndex });
      return conn;
    },
    findNearestConnection: (_point: P) => connections[0] ?? null,
  };
  return { ctx, svg, calls };
}
//=============================================================================
function pointer(
  x: number,
  y: number,
  opts: { pointerId?: number; onWire?: boolean; button?: number; ctrl?: boolean } = {}
): any {
  const { pointerId = 1, onWire = true, button = 0, ctrl = true } = opts;
  return {
    pointerId,
    button,
    ctrlKey: ctrl,
    metaKey: false,
    clientX: x,
    clientY: y,
    target: { classList: { contains: () => onWire } },
  };
}
//=============================================================================
const route: P[] = [
  { x: 0, y: 0 },
  { x: 100, y: 0 },
];
//=============================================================================
describe('branch validation', () => {
  it('adds a connection from the wire source to the dropped input', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    const h = makeWireBranchHandlers(ctx);
    expect(h.branchWireFromSource(conn, 'c', 1)).toBe(true);
    expect(calls.created).toContainEqual({ from: 'a', to: 'c', toIndex: 1, fromIndex: 0 });
  });

  it('fans out from an output that already drives another input', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    const h = makeWireBranchHandlers(ctx);
    // a.out0 already feeds b.in0; branching the same output to a free input passes.
    expect(h.branchWireFromSource(conn, 'c', 0)).toBe(true);
    expect(calls.created).toContainEqual({ from: 'a', to: 'c', toIndex: 0, fromIndex: 0 });
  });

  it('rejects branching back onto the source block', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    const h = makeWireBranchHandlers(ctx);
    expect(h.branchWireFromSource(conn, 'a', 0)).toBe(false);
    expect(calls.created.length).toBe(0);
    expect(calls.errors.join(' ')).toMatch(/output port to an input port/);
  });

  it('is refused while the diagram is locked', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    ctx.state.locked = true;
    const h = makeWireBranchHandlers(ctx);
    expect(h.branchWireFromSource(conn, 'c', 0)).toBe(false);
    expect(calls.errors.join(' ')).toMatch(/locked/);
  });

  it('surfaces createConnection rejection on an occupied input', () => {
    const conn = makeConn('a', 'b', route);
    const other = makeConn('x', 'c', route);
    const { ctx } = makeCtx([conn, other]);
    const h = makeWireBranchHandlers(ctx);
    expect(h.branchWireFromSource(conn, 'c', 0)).toBe(false);
  });
});
//=============================================================================
describe('arm / drag / cancel lifecycle', () => {
  afterEach(() => {
    document.body.innerHTML = '';
    delete (document as any).elementFromPoint;
  });

  const stubDropTarget = (el: Element | null) => {
    (document as any).elementFromPoint = () => el;
  };

  it('arms only with the Ctrl/Cmd modifier on a wire', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx } = makeCtx([conn]);
    const h = makeWireBranchHandlers(ctx);
    expect(h.armWireBranchDrag(pointer(50, 0, { ctrl: false }))).toBe(false); // no modifier
    expect(h.armWireBranchDrag(pointer(50, 0, { onWire: false }))).toBe(false); // not on wire
    expect(h.armWireBranchDrag(pointer(50, 0))).toBe(true); // Ctrl on wire
    h.cancelWireBranchDrag();
  });

  it('shows the ghost past the threshold and branches on an input drop', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, svg, calls } = makeCtx([conn]);
    svg.appendChild(portGroup('a', 'out', 0)); // source anchor for beginPending
    const targetGroup = portGroup('c', 'in', 0);
    svg.appendChild(targetGroup);
    const h = makeWireBranchHandlers(ctx);

    stubDropTarget(targetGroup);
    expect(h.armWireBranchDrag(pointer(50, 0))).toBe(true);
    window.dispatchEvent(
      new PointerEvent('pointermove', { pointerId: 1, clientX: 90, clientY: 0 })
    );
    expect(calls.beginPending).toBe(1);
    window.dispatchEvent(
      new PointerEvent('pointerup', { pointerId: 1, clientX: 200, clientY: 40 })
    );
    expect(calls.created).toContainEqual({ from: 'a', to: 'c', toIndex: 0, fromIndex: 0 });
    expect(calls.clearPending).toBe(1);
  });

  it('rejects a drop on an output port', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, svg, calls } = makeCtx([conn]);
    svg.appendChild(portGroup('a', 'out', 0));
    const wrong = portGroup('c', 'out', 0);
    svg.appendChild(wrong);
    const h = makeWireBranchHandlers(ctx);

    stubDropTarget(wrong);
    h.armWireBranchDrag(pointer(50, 0));
    window.dispatchEvent(
      new PointerEvent('pointermove', { pointerId: 1, clientX: 90, clientY: 0 })
    );
    window.dispatchEvent(
      new PointerEvent('pointerup', { pointerId: 1, clientX: 200, clientY: 40 })
    );
    expect(calls.created.length).toBe(0);
    expect(calls.errors.join(' ')).toMatch(/output port to an input port/);
  });

  it('a press with no travel leaves the wire untouched (click selects it)', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    const h = makeWireBranchHandlers(ctx);
    expect(h.armWireBranchDrag(pointer(50, 0))).toBe(true);
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1, clientX: 50, clientY: 0 }));
    expect(calls.beginPending).toBe(0);
    expect(calls.clearPending).toBe(0);
    expect(calls.created.length).toBe(0);
  });

  it('Escape during a drag clears the pending ghost', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, svg, calls } = makeCtx([conn]);
    svg.appendChild(portGroup('a', 'out', 0));
    const h = makeWireBranchHandlers(ctx);
    h.armWireBranchDrag(pointer(50, 0));
    window.dispatchEvent(
      new PointerEvent('pointermove', { pointerId: 1, clientX: 90, clientY: 0 })
    );
    expect(h.cancelWireBranchDrag()).toBe(true);
    expect(calls.clearPending).toBe(1);
    expect(calls.created.length).toBe(0);
  });
});
//=============================================================================
