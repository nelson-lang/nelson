//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Insert-on-wire splice: dropping a block on a wire cuts it and threads the
// block between the endpoints; the port-count/self/physical guards and the
// rollback when the second connection fails.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { makeInsertOnWireHandlers } from './renderer-insert-on-wire.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
//=============================================================================
function makeConn(from: string, to: string, extra: any = {}) {
  const path = document.createElementNS(ns, 'path');
  path.setAttribute('class', extra.physical ? 'wire physical-wire' : 'wire');
  return {
    from,
    to,
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 0, y: 40 },
      { x: 200, y: 40 },
    ],
    path,
    ...extra,
  } as any;
}
//=============================================================================
function makeBlock(id: string, inputs: number, outputs: number) {
  return { id, x: 0, y: 0, width: 80, height: 80, inputs, outputs, ports: [] } as any;
}
//=============================================================================
// ctx mirroring the real createConnection outcome: outputs fan out freely, an
// occupied input is rejected, and `rejectFrom` forces a chosen leg to fail.
function makeCtx(connections: any[], blocks: Record<string, any>) {
  let nearest: any = connections[0] ?? null;
  let rejectFrom: string | null = null;
  const created: any[] = [];
  const deleted: any[] = [];
  const ctx: any = {
    state: {
      connections,
      blocks: new Map(Object.entries(blocks)),
      locked: false,
      routingDirty: false,
    },
    findNearestConnection: () => nearest,
    createConnection: (from: string, to: string, toIndex = 0, fromIndex = 0) => {
      if (rejectFrom !== null && from === rejectFrom) return null;
      const occupied = connections.some(
        (c) => c.to === to && Number(c.toIndex ?? 0) === Number(toIndex)
      );
      if (occupied) return null;
      const conn = makeConn(from, to, { toIndex, fromIndex });
      connections.push(conn);
      created.push({ from, to, toIndex, fromIndex });
      return conn;
    },
    deleteConnection: (conn: any) => {
      const i = connections.indexOf(conn);
      if (i >= 0) connections.splice(i, 1);
      deleted.push(conn);
    },
    updateConnections: () => {},
  };
  return {
    ctx,
    created,
    deleted,
    setNearest: (c: any) => (nearest = c),
    setRejectFrom: (id: string | null) => (rejectFrom = id),
  };
}
//=============================================================================
describe('insert on wire', () => {
  it('splices a block onto the wire under the drop point', () => {
    const wire = makeConn('a', 'b');
    const { ctx, created } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(true);
    // Original wire gone, two new legs through the block.
    expect(ctx.state.connections).not.toContain(wire);
    expect(created).toContainEqual({ from: 'a', to: 'g', toIndex: 0, fromIndex: 0 });
    expect(created).toContainEqual({ from: 'g', to: 'b', toIndex: 0, fromIndex: 0 });
    expect(ctx.state.connections.length).toBe(2);
  });

  it('splices a moved block using its own center when no point is given', () => {
    const wire = makeConn('a', 'b');
    const { ctx, created } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g')).toBe(true);
    expect(created).toContainEqual({ from: 'a', to: 'g', toIndex: 0, fromIndex: 0 });
    expect(created).toContainEqual({ from: 'g', to: 'b', toIndex: 0, fromIndex: 0 });
  });

  it('preserves a non-zero source output index on the inbound leg', () => {
    const wire = makeConn('a', 'b', { fromIndex: 2 });
    const { ctx, created } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g')).toBe(true);
    expect(created).toContainEqual({ from: 'a', to: 'g', toIndex: 0, fromIndex: 2 });
  });

  it('targets the first free input of the dropped block', () => {
    const wire = makeConn('a', 'b');
    const occupy = makeConn('x', 'g', { toIndex: 0 });
    const { ctx, created } = makeCtx([wire, occupy], { g: makeBlock('g', 2, 1) });
    const h = makeInsertOnWireHandlers(ctx);
    h.insertBlockOnWire('g', { x: 40, y: 40 });
    expect(created).toContainEqual({ from: 'a', to: 'g', toIndex: 1, fromIndex: 0 });
  });

  it('refuses a block without an input', () => {
    const wire = makeConn('a', 'b');
    const { ctx, created } = makeCtx([wire], { g: makeBlock('g', 0, 1) });
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(false);
    expect(ctx.state.connections).toContain(wire);
    expect(created.length).toBe(0);
  });

  it('refuses a block without an output', () => {
    const wire = makeConn('a', 'b');
    const { ctx, created } = makeCtx([wire], { g: makeBlock('g', 1, 0) });
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(false);
    expect(ctx.state.connections).toContain(wire);
    expect(created.length).toBe(0);
  });

  it('ignores a physical/acausal wire', () => {
    const wire = makeConn('a', 'b', { physical: true });
    const { ctx } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(false);
    expect(ctx.state.connections).toContain(wire);
  });

  it('ignores a wire already connected to the dropped block', () => {
    const wire = makeConn('g', 'b');
    const { ctx } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(false);
    expect(ctx.state.connections).toContain(wire);
  });

  it('does nothing when no wire sits under the drop', () => {
    const wire = makeConn('a', 'b');
    const { ctx, setNearest } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    setNearest(null);
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(false);
    expect(ctx.state.connections).toContain(wire);
  });

  it('rolls back and restores the original wire when the outbound leg fails', () => {
    const wire = makeConn('a', 'b');
    const { ctx, created, setRejectFrom } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    setRejectFrom('g'); // the block->target leg will fail
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(false);
    // The diagram is back to a single a->b wire; no block legs remain.
    expect(ctx.state.connections.length).toBe(1);
    const restored = ctx.state.connections[0];
    expect({ from: restored.from, to: restored.to }).toEqual({ from: 'a', to: 'b' });
    expect(created).toContainEqual({ from: 'a', to: 'b', toIndex: 0, fromIndex: 0 });
    expect(ctx.state.connections.some((c: any) => c.from === 'g' || c.to === 'g')).toBe(false);
  });

  it('is refused while the diagram is locked', () => {
    const wire = makeConn('a', 'b');
    const { ctx } = makeCtx([wire], { g: makeBlock('g', 1, 1) });
    ctx.state.locked = true;
    const h = makeInsertOnWireHandlers(ctx);
    expect(h.insertBlockOnWire('g', { x: 40, y: 40 })).toBe(false);
    expect(ctx.state.connections).toContain(wire);
  });
});
//=============================================================================
