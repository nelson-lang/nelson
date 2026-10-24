//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Wire-segment editing state machine: arm/threshold, perpendicular move with
// neighbor stretch, stub protection, pin commit, cancel, pin release.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { makeWireSegmentHandlers } from './renderer-wire-segment.js';
//=============================================================================
type P = { x: number; y: number; pin?: boolean };
function makeConn(points: P[]) {
  return {
    from: 'a',
    to: 'b',
    fromIndex: 0,
    toIndex: 0,
    points: points.map((p) => ({ ...p })),
  } as any;
}
//=============================================================================
function makeCtx(conn: any) {
  const applied: any[] = [];
  const ctx: any = {
    state: {
      connections: [conn],
      locked: false,
      autoRoute: true,
      routingDirty: false,
    },
    svg: null,
    clientToSvg: (x: number, y: number) => ({ x, y }),
    applyWirePathsFast: (s: any) => applied.push(s),
    updateConnections: () => {},
    updateSelectionBox: () => {},
  };
  return { ctx, applied };
}
//=============================================================================
function pointerEvent(x: number, y: number, pointerId = 1, onWire = true): any {
  return {
    pointerId,
    clientX: x,
    clientY: y,
    target: { classList: { contains: () => onWire } },
  };
}
//=============================================================================
// Route: (0,0) → stub → long H at y=0 → V at x=100 → H at y=80 → stub → (200,80)
const baseline: P[] = [
  { x: 0, y: 0 },
  { x: 10, y: 0 },
  { x: 100, y: 0 },
  { x: 100, y: 80 },
  { x: 190, y: 80 },
  { x: 200, y: 80 },
];
//=============================================================================
describe('wire segment editing', () => {
  it('resolves the interior segment under the pointer (stubs excluded)', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    const h = makeWireSegmentHandlers(ctx);
    const hit = h.resolveWireSegmentAt({ x: 100, y: 40 });
    expect(hit).not.toBeNull();
    expect(hit!.segIndex).toBe(2);
    expect(hit!.orient).toBe('V');
    // On the from-port stub (beyond the interior segment's threshold): no hit.
    expect(h.resolveWireSegmentAt({ x: 2, y: 0 })).toBeNull();
  });

  it('stays armed below the 4px threshold (click-through selection preserved)', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    const h = makeWireSegmentHandlers(ctx);
    expect(h.armWireSegmentDrag(pointerEvent(100, 40))).toBe(true);
    expect(h.wireSegmentDrag.phase).toBe('armed');
    h.updateWireSegmentDrag(pointerEvent(102, 40));
    expect(h.wireSegmentDrag.phase).toBe('armed');
    // Releasing without dragging is not consumed → click selection intact.
    expect(h.commitWireSegmentDrag(pointerEvent(102, 40))).toBe(false);
    expect(h.wireSegmentDrag.phase).toBe('idle');
    expect(conn.points.map((p: any) => [p.x, p.y])).toEqual(baseline.map((p) => [p.x, p.y]));
  });

  it('drags the segment perpendicular, stretching its neighbors', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    const h = makeWireSegmentHandlers(ctx);
    h.armWireSegmentDrag(pointerEvent(100, 40));
    h.updateWireSegmentDrag(pointerEvent(140, 40)); // V segment → moves in x
    expect(h.wireSegmentDrag.phase).toBe('dragging');
    const xs = conn.points.map((p: any) => p.x);
    expect(xs).toContain(140);
    expect(conn.points[0]).toMatchObject({ x: 0, y: 0 });
    expect(conn.points[conn.points.length - 1]).toMatchObject({ x: 200, y: 80 });
  });

  it('commit pins the moved segment endpoints and dispatches diagramCommitted', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    const h = makeWireSegmentHandlers(ctx);
    const events: string[] = [];
    const listener = () => events.push('committed');
    window.addEventListener('diagramCommitted', listener);
    try {
      h.armWireSegmentDrag(pointerEvent(100, 40));
      h.updateWireSegmentDrag(pointerEvent(140, 40));
      expect(h.commitWireSegmentDrag(pointerEvent(140, 40))).toBe(true);
    } finally {
      window.removeEventListener('diagramCommitted', listener);
    }
    const pinned = conn.points.filter((p: any) => p.pin);
    expect(pinned.length).toBe(2);
    expect(pinned.every((p: any) => p.x === 140)).toBe(true);
    expect(events).toContain('committed');
    expect(ctx.state.routingDirty).toBe(true);
  });

  it('cancel (Escape) restores the pre-drag shape', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    const h = makeWireSegmentHandlers(ctx);
    h.armWireSegmentDrag(pointerEvent(100, 40));
    h.updateWireSegmentDrag(pointerEvent(150, 40));
    expect(h.cancelWireSegmentDrag(null)).toBe(true);
    expect(conn.points.map((p: any) => [p.x, p.y])).toEqual(baseline.map((p) => [p.x, p.y]));
    expect(h.wireSegmentDrag.phase).toBe('idle');
  });

  it('protects the port stub when grabbing the stub-adjacent segment', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    const h = makeWireSegmentHandlers(ctx);
    h.armWireSegmentDrag(pointerEvent(50, 0));
    expect(h.wireSegmentDrag.segIndex).toBe(1);
    h.updateWireSegmentDrag(pointerEvent(50, 30));
    h.commitWireSegmentDrag(pointerEvent(50, 30));
    expect(conn.points[0]).toMatchObject({ x: 0, y: 0 });
    expect(conn.points[1].y).toBe(0);
    expect(conn.points[1].x).toBeGreaterThanOrEqual(10);
    expect(conn.points.some((p: any) => p.pin && p.y === 30)).toBe(true);
  });

  it('releaseWirePins clears pins and reroutes', () => {
    const conn = makeConn([
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 100, y: 0, pin: true },
      { x: 100, y: 80, pin: true },
      { x: 190, y: 80 },
      { x: 200, y: 80 },
    ]);
    const { ctx } = makeCtx(conn);
    const h = makeWireSegmentHandlers(ctx);
    expect(h.releaseWirePins(conn)).toBe(true);
    expect(conn.points.some((p: any) => p.pin)).toBe(false);
    expect(ctx.state.routingDirty).toBe(true);
    expect(h.releaseWirePins(conn)).toBe(false);
  });

  it('ignores wires when the diagram is locked or in manual mode', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    ctx.state.locked = true;
    const h = makeWireSegmentHandlers(ctx);
    expect(h.armWireSegmentDrag(pointerEvent(100, 40))).toBe(false);
    ctx.state.locked = false;
    ctx.state.autoRoute = false;
    expect(h.armWireSegmentDrag(pointerEvent(100, 40))).toBe(false);
  });
});
//=============================================================================
describe('hover cursor feedback', () => {
  function svgStub() {
    return { style: { cursor: '' } } as any;
  }

  it('sets ns/ew-resize over a draggable segment and clears elsewhere', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    ctx.svg = svgStub();
    const h = makeWireSegmentHandlers(ctx);
    h.handleWireSegmentHover(pointerEvent(100, 40));
    expect(ctx.svg.style.cursor).toBe('ew-resize');
    h.handleWireSegmentHover(pointerEvent(50, 0));
    expect(ctx.svg.style.cursor).toBe('ns-resize');
    h.handleWireSegmentHover(pointerEvent(50, 0, 1, false));
    expect(ctx.svg.style.cursor).toBe('');
  });

  it('does not fight an active drag and respects the diagram lock', () => {
    const conn = makeConn(baseline);
    const { ctx } = makeCtx(conn);
    ctx.svg = svgStub();
    const h = makeWireSegmentHandlers(ctx);
    h.armWireSegmentDrag(pointerEvent(100, 40));
    h.updateWireSegmentDrag(pointerEvent(140, 40)); // dragging
    const during = ctx.svg.style.cursor;
    h.handleWireSegmentHover(pointerEvent(50, 0));
    expect(ctx.svg.style.cursor).toBe(during); // untouched while dragging
    h.cancelWireSegmentDrag(null);

    ctx.state.locked = true;
    h.handleWireSegmentHover(pointerEvent(100, 40));
    expect(ctx.svg.style.cursor).toBe('');
  });
});
//=============================================================================
