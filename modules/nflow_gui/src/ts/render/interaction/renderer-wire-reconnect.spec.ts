//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Wire-endpoint re-targeting: endpoint resolution, validation rules mirroring
// createConnection, and the arm/drag/cancel lifecycle.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { makeWireReconnectHandlers } from './renderer-wire-reconnect.js';
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
function makeCtx(connections: any[], blocks: Map<string, any> = new Map()) {
  const svg = document.createElementNS(ns, 'svg');
  document.body.appendChild(svg);
  const calls: any = { beginPending: 0, clearPending: 0, errors: [] as string[] };
  const ctx: any = {
    svg,
    state: { connections, blocks, locked: false, autoRoute: true, routingDirty: false },
    clientToSvg: (x: number, y: number) => ({ x, y }),
    beginPending: () => {
      calls.beginPending += 1;
    },
    clearPending: () => {
      calls.clearPending += 1;
    },
    onConnectionError: (m: string) => calls.errors.push(m),
    buildFastDragPathFromPorts: () => [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    applyWirePathsFast: () => {},
    queueRouteAfterPreview: () => {},
    applyWirePaths: () => {},
    updateWireCornerHandles: () => {},
  };
  return { ctx, svg, calls };
}
//=============================================================================
function pointer(x: number, y: number, pointerId = 1, onWire = true, button = 0): any {
  return {
    pointerId,
    button,
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
describe('wire endpoint resolution', () => {
  it('matches the nearest tip within the grab radius, nothing beyond', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx } = makeCtx([conn]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.resolveWireEndpointAt({ x: 4, y: 0 })).toMatchObject({ end: 'from' });
    expect(h.resolveWireEndpointAt({ x: 96, y: 0 })).toMatchObject({ end: 'to' });
    // Just past the port's 12px hit circle but inside the band: still an endpoint.
    expect(h.resolveWireEndpointAt({ x: 18, y: 0 })).toMatchObject({ end: 'from' });
    // Mid-wire, far from both tips: no endpoint.
    expect(h.resolveWireEndpointAt({ x: 50, y: 0 })).toBeNull();
  });
});
//=============================================================================
describe('reconnect validation (mirrors createConnection)', () => {
  it('moves the input end to a free input and reroutes', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx } = makeCtx([conn]);
    const h = makeWireReconnectHandlers(ctx);
    const committed: string[] = [];
    const l = () => committed.push('ok');
    window.addEventListener('diagramCommitted', l);
    try {
      expect(h.reconnectWireEndpoint(conn, 'to', 'c', 1)).toBe(true);
    } finally {
      window.removeEventListener('diagramCommitted', l);
    }
    expect(conn.to).toBe('c');
    expect(conn.toIndex).toBe(1);
    expect(conn.from).toBe('a');
    expect(ctx.state.routingDirty).toBe(true);
    expect(committed).toContain('ok');
  });

  it('moves the output end; outputs fan out so no occupancy check applies', () => {
    const conn = makeConn('a', 'b', route);
    // c.out2 already feeds another input: reusing it as a source must still pass.
    const other = makeConn('c', 'z', route, { fromIndex: 2 });
    const { ctx } = makeCtx([conn, other]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.reconnectWireEndpoint(conn, 'from', 'c', 2)).toBe(true);
    expect(conn.from).toBe('c');
    expect(conn.fromIndex).toBe(2);
  });

  it('rejects an already-occupied input', () => {
    const conn = makeConn('a', 'b', route);
    const other = makeConn('x', 'c', route, { toIndex: 0 });
    const { ctx, calls } = makeCtx([conn, other]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.reconnectWireEndpoint(conn, 'to', 'c', 0)).toBe(false);
    expect(conn.to).toBe('b'); // unchanged
    expect(calls.errors.join(' ')).toMatch(/already has a connection/);
  });

  it('rejects a self-connection', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.reconnectWireEndpoint(conn, 'to', 'a', 0)).toBe(false);
    expect(calls.errors.length).toBe(1);
  });

  it('rejects a duplicate connection', () => {
    const conn = makeConn('a', 'b', route);
    const dup = makeConn('a', 'c', route);
    const { ctx, calls } = makeCtx([conn, dup]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.reconnectWireEndpoint(conn, 'to', 'c', 0)).toBe(false);
    expect(calls.errors.join(' ')).toMatch(/already exists/);
  });

  it('returns false for a no-op drop on the same port', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.reconnectWireEndpoint(conn, 'to', 'b', 0)).toBe(false);
    expect(calls.errors.length).toBe(0);
  });

  it('is refused while the diagram is locked', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    ctx.state.locked = true;
    const h = makeWireReconnectHandlers(ctx);
    expect(h.reconnectWireEndpoint(conn, 'to', 'c', 0)).toBe(false);
    expect(calls.errors.join(' ')).toMatch(/locked/);
  });

  it('allows phys fan-in onto an occupied acausal node', () => {
    const conn = makeConn('a', 'b', route);
    const other = makeConn('x', 'c', route);
    const blocks = new Map<string, any>([
      ['a', { ports: [{ type: 'phys', index: 0 }] }],
      ['c', { ports: [{ type: 'phys', index: 0 }] }],
    ]);
    const { ctx } = makeCtx([conn, other], blocks);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.reconnectWireEndpoint(conn, 'to', 'c', 0)).toBe(true);
    expect(conn.to).toBe('c');
  });
});
//=============================================================================
describe('arm / drag / cancel lifecycle', () => {
  afterEach(() => {
    document.body.innerHTML = '';
    delete (document as any).elementFromPoint;
  });

  // jsdom has no layout, so stub the drop-target hit test the release relies on.
  const stubDropTarget = (el: Element | null) => {
    (document as any).elementFromPoint = () => el;
  };

  it('arms only on a wire near an endpoint', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx } = makeCtx([conn]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.armWireEndpointDrag(pointer(50, 0))).toBe(false); // mid-wire
    expect(h.armWireEndpointDrag(pointer(4, 0, 1, false))).toBe(false); // not on wire
    expect(h.armWireEndpointDrag(pointer(4, 0))).toBe(true); // near from-tip, on wire
    h.cancelWireEndpointDrag();
  });

  it('shows the ghost past the threshold and reconnects on a compatible drop', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, svg, calls } = makeCtx([conn]);
    svg.appendChild(portGroup('a', 'out', 0)); // anchor for beginPending
    const targetGroup = portGroup('c', 'in', 1);
    svg.appendChild(targetGroup);
    const h = makeWireReconnectHandlers(ctx);

    stubDropTarget(targetGroup);
    expect(h.armWireEndpointDrag(pointer(96, 0))).toBe(true); // grab the input tip
    window.dispatchEvent(
      new PointerEvent('pointermove', { pointerId: 1, clientX: 140, clientY: 0 })
    );
    expect(calls.beginPending).toBe(1);
    window.dispatchEvent(
      new PointerEvent('pointerup', { pointerId: 1, clientX: 200, clientY: 40 })
    );
    expect(conn.to).toBe('c');
    expect(conn.toIndex).toBe(1);
    expect(calls.clearPending).toBe(1);
  });

  it('rejects a drop on the wrong port side', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, svg, calls } = makeCtx([conn]);
    svg.appendChild(portGroup('a', 'out', 0));
    const wrong = portGroup('c', 'out', 0); // moving the input tip onto an output
    svg.appendChild(wrong);
    const h = makeWireReconnectHandlers(ctx);

    stubDropTarget(wrong);
    h.armWireEndpointDrag(pointer(96, 0));
    window.dispatchEvent(
      new PointerEvent('pointermove', { pointerId: 1, clientX: 140, clientY: 0 })
    );
    window.dispatchEvent(
      new PointerEvent('pointerup', { pointerId: 1, clientX: 200, clientY: 40 })
    );
    expect(conn.to).toBe('b'); // unchanged
    expect(calls.errors.join(' ')).toMatch(/output port to an input port/);
  });

  it('a press with no travel leaves the wire untouched (click selects it)', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, calls } = makeCtx([conn]);
    const h = makeWireReconnectHandlers(ctx);
    expect(h.armWireEndpointDrag(pointer(96, 0))).toBe(true);
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 1, clientX: 96, clientY: 0 }));
    expect(calls.beginPending).toBe(0);
    expect(calls.clearPending).toBe(0);
    expect(conn.to).toBe('b');
  });

  it('Escape during a drag clears the pending ghost', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, svg, calls } = makeCtx([conn]);
    svg.appendChild(portGroup('a', 'out', 0));
    const h = makeWireReconnectHandlers(ctx);
    h.armWireEndpointDrag(pointer(96, 0));
    window.dispatchEvent(
      new PointerEvent('pointermove', { pointerId: 1, clientX: 140, clientY: 0 })
    );
    expect(h.cancelWireEndpointDrag()).toBe(true);
    expect(calls.clearPending).toBe(1);
    expect(conn.to).toBe('b');
  });
});
//=============================================================================
describe('endpoint hover feedback', () => {
  it('sets crosshair near a tip, yields off-tip, claims during a drag', () => {
    const conn = makeConn('a', 'b', route);
    const { ctx, svg } = makeCtx([conn]);
    (svg as any).style = { cursor: '' };
    const h = makeWireReconnectHandlers(ctx);
    expect(h.handleWireEndpointHover(pointer(4, 0))).toBe(true);
    expect((svg as any).style.cursor).toBe('crosshair');
    // Mid-wire: not an endpoint, yields to segment hover and clears its cursor.
    expect(h.handleWireEndpointHover(pointer(50, 0))).toBe(false);
    expect((svg as any).style.cursor).toBe('');
    // Off-wire clears too.
    h.handleWireEndpointHover(pointer(4, 0));
    expect(h.handleWireEndpointHover(pointer(4, 0, 1, false))).toBe(false);
    expect((svg as any).style.cursor).toBe('');
    // While a drag is active, hover is claimed so segment hover stays suppressed.
    h.armWireEndpointDrag(pointer(96, 0));
    expect(h.handleWireEndpointHover(pointer(300, 300, 1, false))).toBe(true);
    h.cancelWireEndpointDrag();
  });
});
//=============================================================================
