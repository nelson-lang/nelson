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
import { state } from './state.js';
//=============================================================================
function makeBlock(id: number, x = 0, y = 0): Record<string, unknown> {
  return { id, type: 'gain', x, y, width: 60, height: 40, params: {} };
}
//=============================================================================
function makeConn(from: number, to: number, fromIndex = 0, toIndex = 0): Record<string, unknown> {
  return { from, to, fromIndex, toIndex };
}
//=============================================================================
/** Variant of makeBlock that uses a string id (as loaded from a .nflow file). */
function makeStrBlock(id: string, x = 0, y = 0): Record<string, unknown> {
  return { id, type: 'gain', x, y, width: 60, height: 40, params: {} };
}
//=============================================================================
function makeStrConn(
  from: string | number,
  to: string | number,
  fromIndex = 0,
  toIndex = 0
): Record<string, unknown> {
  return { from, to, fromIndex, toIndex };
}
//=============================================================================
function resetState() {
  state.blocks.clear();
  (state.connections as unknown[]) = [];
  state.selectedId = null;
  state.selectedIds = new Set();
}
//=============================================================================
function buildMockRenderer() {
  let nextId = 1000;
  const created: Array<{ type: string; x: number; y: number; opts: unknown }> = [];
  const deleted: number[] = [];
  const connected: Array<{ from: number; to: number; toIndex: number; fromIndex: number }> = [];

  const createBlock = vi.fn((type: string, x: number, y: number, opts: unknown) => {
    const id = nextId++;
    created.push({ type, x, y, opts });
    return { id };
  });

  const deleteBlock = vi.fn((id: number) => {
    deleted.push(id);
    state.blocks.delete(id);
    (state.connections as unknown[]) = (state.connections as Array<Record<string, unknown>>).filter(
      (c) => c.from !== id && c.to !== id
    );
  });

  const createConnection = vi.fn((from: number, to: number, toIndex: number, fromIndex: number) => {
    connected.push({ from, to, toIndex, fromIndex });
    return {};
  });

  const renderer = {
    current: { createBlock, deleteBlock, createConnection, updateSelectionBox: vi.fn() },
  };

  return { renderer, created, deleted, connected };
}
//=============================================================================
describe('createSubsystemFromSelection', () => {
  let m: typeof import('./subsystem-handlers.js');
  let setStatusEl: (el: HTMLElement | null) => void;
  let setRendererRef: (ref: unknown) => void;

  beforeAll(async () => {
    m = await import('./subsystem-handlers.js');
    const dh = await import('./diagram-handlers.js');
    setStatusEl = dh.setStatusEl;
    setRendererRef = dh.setRendererRef as (ref: unknown) => void;
  });

  beforeEach(() => {
    resetState();
    setStatusEl(null);
    setRendererRef(null);
  });

  test('single block selected: no-op, shows status message', () => {
    const statusEl = document.createElement('div');
    setStatusEl(statusEl);

    const { renderer } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1));
    state.selectedId = 1;
    state.selectedIds = new Set([1]);

    (m as any).createSubsystemFromSelection();

    expect(renderer.current.createBlock).not.toHaveBeenCalled();
    expect(statusEl.textContent).toBeTruthy();
  });

  test('A→B both selected: subsystem with 0 external ports', () => {
    const { renderer, created, deleted, connected } = buildMockRenderer();
    setRendererRef(renderer);

    const a = makeBlock(1, 0, 50);
    const b = makeBlock(2, 200, 50);
    state.blocks.set(1, a);
    state.blocks.set(2, b);
    (state.connections as unknown[]) = [makeConn(1, 2)];

    state.selectedIds = new Set([1, 2]);

    (m as any).createSubsystemFromSelection();

    expect(created.length).toBe(1);
    expect(created[0].type).toBe('subsystem');

    const params = (created[0].opts as any).params;
    expect(params.externalInputs).toHaveLength(0);
    expect(params.externalOutputs).toHaveLength(0);

    expect(deleted).toContain(1);
    expect(deleted).toContain(2);

    expect(connected).toHaveLength(0);
  });

  test('A→B→C, select B+C: subsystem with 1 inport, 0 outports', () => {
    const { renderer, created, deleted, connected } = buildMockRenderer();
    setRendererRef(renderer);

    const a = makeBlock(1, 0, 50);
    const b = makeBlock(2, 200, 50);
    const c = makeBlock(3, 400, 50);
    state.blocks.set(1, a);
    state.blocks.set(2, b);
    state.blocks.set(3, c);
    (state.connections as unknown[]) = [makeConn(1, 2), makeConn(2, 3)];

    state.selectedIds = new Set([2, 3]);

    (m as any).createSubsystemFromSelection();

    expect(created.length).toBe(1);
    const params = (created[0].opts as any).params;

    expect(params.externalInputs).toHaveLength(1);
    expect(params.externalInputs[0].name).toBe('in1');

    expect(params.externalOutputs).toHaveLength(0);

    const innerBlocks: unknown[] = params.subsystem.blocks;
    expect(innerBlocks.length).toBe(3); // B + C + labelSource

    expect(deleted).not.toContain(1);
    expect(deleted).toContain(2);
    expect(deleted).toContain(3);

    expect(connected).toHaveLength(1);
    expect(connected[0].from).toBe(1);
    expect(connected[0].toIndex).toBe(0);
  });

  test('B and C selected with incoming/outgoing boundaries', () => {
    const { renderer, created, connected } = buildMockRenderer();
    setRendererRef(renderer);

    const a = makeBlock(1, 0, 0);
    const b = makeBlock(2, 100, 0);
    const c = makeBlock(3, 100, 100);
    const d = makeBlock(4, 200, 100);
    state.blocks.set(1, a);
    state.blocks.set(2, b);
    state.blocks.set(3, c);
    state.blocks.set(4, d);
    // A→B (incoming to B), C→D (outgoing from C)
    (state.connections as unknown[]) = [makeConn(1, 2), makeConn(3, 4)];

    state.selectedIds = new Set([2, 3]);

    (m as any).createSubsystemFromSelection();

    const params = (created[0].opts as any).params;
    expect(params.externalInputs).toHaveLength(1); // A→B
    expect(params.externalOutputs).toHaveLength(1); // C→D

    expect(connected).toHaveLength(2);
  });

  test('incoming boundary connections are sorted by target block y', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    // c is higher up (lower y), so its port should be in1
    const extA = makeBlock(1, 0, 200);
    const extB = makeBlock(2, 0, 100);
    const b = makeBlock(3, 200, 200);
    const c = makeBlock(4, 200, 100);
    state.blocks.set(1, extA);
    state.blocks.set(2, extB);
    state.blocks.set(3, b);
    state.blocks.set(4, c);
    (state.connections as unknown[]) = [makeConn(1, 3), makeConn(2, 4)];

    state.selectedIds = new Set([3, 4]);

    (m as any).createSubsystemFromSelection();

    const params = (created[0].opts as any).params;
    expect(params.externalInputs[0].name).toBe('in1');
    expect(params.externalInputs[1].name).toBe('in2');

    // Inner labelSource at index 0 should target block 4 (c, y=100)
    const innerConns: unknown[] = params.subsystem.connections;
    const firstLabelConn = innerConns.find((conn) => (conn as any).to === 4);
    expect(firstLabelConn).toBeDefined();
  });

  test('no renderer: returns without throwing', () => {
    setRendererRef(null);
    state.blocks.set(1, makeBlock(1));
    state.blocks.set(2, makeBlock(2));
    state.selectedIds = new Set([1, 2]);

    expect(() => (m as any).createSubsystemFromSelection()).not.toThrow();
  });

  test('zero blocks selected: no-op', () => {
    const statusEl = document.createElement('div');
    setStatusEl(statusEl);
    const { renderer } = buildMockRenderer();
    setRendererRef(renderer);

    state.selectedIds = new Set();
    state.selectedId = null;

    (m as any).createSubsystemFromSelection();

    expect(renderer.current.createBlock).not.toHaveBeenCalled();
    expect(statusEl.textContent).toBeTruthy();
  });

  // Boundary ports are per connection, not per block: A feeds B and C, so 2 inports.
  test('diamond topology: 2 incomings, 0 outgoing for B+C+D', () => {
    const { renderer, created, connected } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1, 0, 50)); // A (external)
    state.blocks.set(2, makeBlock(2, 150, 0)); // B
    state.blocks.set(3, makeBlock(3, 150, 100)); // C
    state.blocks.set(4, makeBlock(4, 300, 50)); // D
    (state.connections as unknown[]) = [
      makeConn(1, 2), // A→B  incoming boundary
      makeConn(1, 3), // A→C  incoming boundary
      makeConn(2, 4), // B→D  internal
      makeConn(3, 4), // C→D  internal
    ];

    state.selectedIds = new Set([2, 3, 4]);

    (m as any).createSubsystemFromSelection();

    const params = (created[0].opts as any).params;
    expect(params.externalInputs).toHaveLength(2); // A→B and A→C
    expect(params.externalOutputs).toHaveLength(0);
    expect(connected).toHaveLength(2); // reconnect A→subsystem port 0 and port 1
  });

  test('multiple outgoing boundaries produce multiple outports', () => {
    const { renderer, created, connected } = buildMockRenderer();
    setRendererRef(renderer);

    // Select B and E; both have outgoing connections.
    state.blocks.set(1, makeBlock(1, 0, 50)); // Ext-In
    state.blocks.set(2, makeBlock(2, 150, 0)); // B (selected)
    state.blocks.set(3, makeBlock(3, 150, 100)); // E (selected)
    state.blocks.set(4, makeBlock(4, 300, 0)); // Ext-Out1
    state.blocks.set(5, makeBlock(5, 300, 100)); // Ext-Out2
    (state.connections as unknown[]) = [
      makeConn(1, 2), // incoming to B
      makeConn(2, 4), // B → Ext-Out1  outgoing
      makeConn(3, 5), // E → Ext-Out2  outgoing
    ];

    state.selectedIds = new Set([2, 3]);

    (m as any).createSubsystemFromSelection();

    const params = (created[0].opts as any).params;
    expect(params.externalInputs).toHaveLength(1);
    expect(params.externalOutputs).toHaveLength(2);
    expect(connected).toHaveLength(3); // 1 in-reconnect + 2 out-reconnects
  });

  test('internal connections are preserved in inner diagram', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1, 0, 50));
    state.blocks.set(2, makeBlock(2, 150, 50));
    state.blocks.set(3, makeBlock(3, 300, 50));
    (state.connections as unknown[]) = [makeConn(1, 2), makeConn(2, 3)]; // both internal

    state.selectedIds = new Set([1, 2, 3]);

    (m as any).createSubsystemFromSelection();

    const innerConns: unknown[] = (created[0].opts as any).params.subsystem.connections;
    const hasConn = (from: number, to: number) =>
      innerConns.some((c) => (c as any).from === from && (c as any).to === to);
    expect(hasConn(1, 2)).toBe(true);
    expect(hasConn(2, 3)).toBe(true);
  });

  test('subsystem block is placed at bounding-box center of selected blocks', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    // Block at (0,0) 60×40 → right edge 60, bottom 40
    // Block at (200,100) 60×40 → right edge 260, bottom 140
    state.blocks.set(1, makeBlock(1, 0, 0));
    state.blocks.set(2, makeBlock(2, 200, 100));
    (state.connections as unknown[]) = [];

    state.selectedIds = new Set([1, 2]);

    (m as any).createSubsystemFromSelection();

    // minX=0, maxX=260, minY=0, maxY=140  → center (130, 70)
    expect(created[0].x).toBeCloseTo(130);
    expect(created[0].y).toBeCloseTo(70);
  });

  test('selection is cleared and state marked dirty after creation', () => {
    const { renderer } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1, 0, 50));
    state.blocks.set(2, makeBlock(2, 200, 50));
    (state.connections as unknown[]) = [];
    state.selectedIds = new Set([1, 2]);
    state.selectedId = 1 as unknown as number;

    (m as any).createSubsystemFromSelection();

    expect(state.selectedIds.size).toBe(0);
    expect(state.selectedId).toBeNull();
  });

  test('outgoing reconnection preserves original toIndex on external target', () => {
    const { renderer, created, connected } = buildMockRenderer();
    setRendererRef(renderer);

    // B → D with toIndex=2 (D has multiple inports)
    state.blocks.set(1, makeBlock(1, 100, 50)); // B (selected)
    state.blocks.set(2, makeBlock(2, 300, 50)); // D (external)
    (state.connections as unknown[]) = [makeConn(1, 2, 0, 2)];

    state.selectedIds = new Set([1]);
    // Only 1 block → no-op
    (m as any).createSubsystemFromSelection();
    expect(created.length).toBe(0);

    state.blocks.set(3, makeBlock(3, 100, 150)); // E (selected)
    (state.connections as unknown[]) = [makeConn(1, 2, 0, 2)]; // outgoing boundary
    state.selectedIds = new Set([1, 3]);

    (m as any).createSubsystemFromSelection();

    // Outer reconnection: subsystem port 0 → D, toIndex must be preserved (2)
    const outReconnect = connected.find((c) => c.to === 2);
    expect(outReconnect).toBeDefined();
    expect(outReconnect!.toIndex).toBe(2);
  });

  test('labelSource for incoming boundary is placed left of inner content', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1, 0, 50)); // A external
    state.blocks.set(2, makeBlock(2, 200, 50)); // B selected  (minX=200)
    state.blocks.set(3, makeBlock(3, 400, 50)); // C selected
    (state.connections as unknown[]) = [makeConn(1, 2), makeConn(2, 3)];
    state.selectedIds = new Set([2, 3]);

    (m as any).createSubsystemFromSelection();

    const innerBlocks: Array<Record<string, unknown>> = (created[0].opts as any).params.subsystem
      .blocks;
    const labelSrc = innerBlocks.find((b) => b.type === 'labelSource');
    expect(labelSrc).toBeDefined();
    // Position normalization: offsetX = minX(200) - INNER_PAD(100) = 100
    // labelSource x = (minX - 80) - offsetX = (200 - 80) - 100 = 20
    expect(Number(labelSrc!.x)).toBeCloseTo(20);
  });

  test('labelSink for outgoing boundary is placed right of inner content', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1, 0, 50)); // A selected  (maxX = 0+60=60)
    state.blocks.set(2, makeBlock(2, 200, 50)); // B selected  (maxX = 200+60=260)
    state.blocks.set(3, makeBlock(3, 400, 50)); // C external
    (state.connections as unknown[]) = [makeConn(1, 2), makeConn(2, 3)];
    state.selectedIds = new Set([1, 2]);

    (m as any).createSubsystemFromSelection();

    const innerBlocks: Array<Record<string, unknown>> = (created[0].opts as any).params.subsystem
      .blocks;
    const labelSink = innerBlocks.find((b) => b.type === 'labelSink');
    expect(labelSink).toBeDefined();
    // Position normalization: offsetX = minX(0) - INNER_PAD(100) = -100
    // labelSink x = (maxX(260) + 80) - offsetX(-100) = 440
    expect(Number(labelSink!.x)).toBeCloseTo(440);
  });

  // Regression: string block IDs from .nflow files produced NaN label ids.
  test('string block IDs: labelSource and labelSink IDs are non-NaN and unique', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    // Named IDs like those loaded from a .nflow file
    state.blocks.set('and_gate' as any, makeStrBlock('and_gate', 600, 240));
    state.blocks.set('or_gate' as any, makeStrBlock('or_gate', 600, 420));
    state.blocks.set('b2' as any, makeStrBlock('b2', 300, 220));
    state.blocks.set('and_scope' as any, makeStrBlock('and_scope', 900, 240));

    (state.connections as unknown[]) = [
      makeStrConn('b2', 'and_gate', 0, 0), // incoming boundary
      makeStrConn('b2', 'or_gate', 0, 0), // incoming boundary
      makeStrConn('and_gate', 'and_scope', 0, 0), // outgoing boundary
      makeStrConn('or_gate', 'and_scope', 0, 0), // outgoing boundary
    ];

    state.selectedIds = new Set(['and_gate', 'or_gate'] as any);

    (m as any).createSubsystemFromSelection();

    expect(created.length).toBe(1);
    const innerBlocks: Array<Record<string, unknown>> = (created[0].opts as any).params.subsystem
      .blocks;

    const labelIds = innerBlocks
      .filter((b) => b.type === 'labelSource' || b.type === 'labelSink')
      .map((b) => b.id);

    for (const id of labelIds) {
      expect(String(id)).not.toBe('NaN');
      if (typeof id === 'number') expect(id).not.toBeNaN();
    }
    const unique = new Set(labelIds.map(String));
    expect(unique.size).toBe(labelIds.length);
  });

  test('string block IDs: boundary ports are ordered by target block y position', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    // and_gate at y=240 → should become in1 (lower y → first port)
    // or_gate  at y=420 → should become in2
    state.blocks.set('and_gate' as any, makeStrBlock('and_gate', 600, 240));
    state.blocks.set('or_gate' as any, makeStrBlock('or_gate', 600, 420));
    state.blocks.set('ext_src1' as any, makeStrBlock('ext_src1', 300, 240));
    state.blocks.set('ext_src2' as any, makeStrBlock('ext_src2', 300, 420));

    (state.connections as unknown[]) = [
      makeStrConn('ext_src1', 'and_gate', 0, 0), // target y=240 → in1
      makeStrConn('ext_src2', 'or_gate', 0, 0), // target y=420 → in2
    ];

    state.selectedIds = new Set(['and_gate', 'or_gate'] as any);

    (m as any).createSubsystemFromSelection();

    expect(created.length).toBe(1);
    const params = (created[0].opts as any).params;
    expect(params.externalInputs).toHaveLength(2);
    expect(params.externalInputs[0].name).toBe('in1');
    expect(params.externalInputs[1].name).toBe('in2');

    const innerConns: unknown[] = params.subsystem.connections;
    const first = innerConns.find((c) => (c as any).to === 'and_gate');
    expect(first).toBeDefined();
  });

  test('string block IDs: Logic-Gates scenario produces correct port counts', () => {
    const { renderer, created, connected } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set('b2' as any, makeStrBlock('b2', 300, 220));
    state.blocks.set('b3' as any, makeStrBlock('b3', 300, 260));
    state.blocks.set('b5' as any, makeStrBlock('b5', 300, 400));
    state.blocks.set('b4' as any, makeStrBlock('b4', 300, 440));
    state.blocks.set('and_scope' as any, makeStrBlock('and_scope', 900, 240));
    state.blocks.set('or_scope' as any, makeStrBlock('or_scope', 900, 420));
    state.blocks.set('and_gate' as any, makeStrBlock('and_gate', 600, 240));
    state.blocks.set('or_gate' as any, makeStrBlock('or_gate', 600, 420));

    (state.connections as unknown[]) = [
      makeStrConn('b2', 'and_gate', 0, 0), // incoming
      makeStrConn('b3', 'and_gate', 0, 1), // incoming
      makeStrConn('b5', 'or_gate', 0, 0), // incoming
      makeStrConn('b4', 'or_gate', 0, 1), // incoming
      makeStrConn('and_gate', 'and_scope', 0, 0), // outgoing
      makeStrConn('or_gate', 'or_scope', 0, 0), // outgoing
    ];

    state.selectedIds = new Set(['and_gate', 'or_gate'] as any);

    (m as any).createSubsystemFromSelection();

    expect(created.length).toBe(1);
    const params = (created[0].opts as any).params;
    expect(params.externalInputs).toHaveLength(4);
    expect(params.externalOutputs).toHaveLength(2);
    // 4 incoming reconnections + 2 outgoing = 6 total
    expect(connected).toHaveLength(6);

    const innerBlocks: Array<Record<string, unknown>> = params.subsystem.blocks;
    const labelIds = innerBlocks
      .filter((b) => b.type === 'labelSource' || b.type === 'labelSink')
      .map((b) => String(b.id));
    expect(labelIds.every((id) => id !== 'NaN')).toBe(true);
    expect(new Set(labelIds).size).toBe(labelIds.length);
  });

  test('string block IDs: internal connection and label wires reference correct string IDs', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set('src' as any, makeStrBlock('src', 0, 50)); // external
    state.blocks.set('gate' as any, makeStrBlock('gate', 200, 50)); // selected
    state.blocks.set('gate2' as any, makeStrBlock('gate2', 200, 150)); // selected
    state.blocks.set('sink' as any, makeStrBlock('sink', 400, 50)); // external

    (state.connections as unknown[]) = [
      makeStrConn('src', 'gate', 0, 0), // incoming boundary
      makeStrConn('gate', 'gate2', 0, 0), // internal
      makeStrConn('gate2', 'sink', 0, 0), // outgoing boundary
    ];

    state.selectedIds = new Set(['gate', 'gate2'] as any);

    (m as any).createSubsystemFromSelection();

    expect(created.length).toBe(1);
    const innerConns: Array<Record<string, unknown>> = (created[0].opts as any).params.subsystem
      .connections;

    const internalConn = innerConns.find((c) => c.from === 'gate' && c.to === 'gate2');
    expect(internalConn).toBeDefined();

    // labelSource→gate connection must target 'gate' (string), not NaN
    const labelToGate = innerConns.find((c) => c.to === 'gate');
    expect(labelToGate).toBeDefined();
    expect(String(labelToGate!.to)).toBe('gate');

    // gate2→labelSink connection must originate from 'gate2' (string), not NaN
    const gate2ToSink = innerConns.find((c) => c.from === 'gate2' && c.to !== 'gate2');
    expect(gate2ToSink).toBeDefined();
    expect(String(gate2ToSink!.from)).toBe('gate2');
  });

  test('incoming port is named from connected source block label', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    const srcBlock = {
      id: 1,
      type: 'constant',
      x: 0,
      y: 50,
      width: 60,
      height: 40,
      label: 'Temperature',
      params: {},
    };
    state.blocks.set(1, srcBlock as any);
    state.blocks.set(2, makeBlock(2, 200, 50));
    state.blocks.set(3, makeBlock(3, 400, 50));
    (state.connections as unknown[]) = [makeConn(1, 2), makeConn(2, 3)];
    state.selectedIds = new Set([2, 3]);

    (m as any).createSubsystemFromSelection();

    const params = (created[0].opts as any).params;
    expect(params.externalInputs[0].name).toBe('Temperature');
  });

  test('outgoing port is named from connected target block params.name', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1, 0, 50));
    state.blocks.set(2, makeBlock(2, 200, 50));
    const sinkBlock = {
      id: 3,
      type: 'scope',
      x: 400,
      y: 50,
      width: 60,
      height: 40,
      params: { name: 'Output Signal' },
    };
    state.blocks.set(3, sinkBlock as any);
    (state.connections as unknown[]) = [makeConn(1, 2), makeConn(2, 3)];
    state.selectedIds = new Set([1, 2]);

    (m as any).createSubsystemFromSelection();

    const params = (created[0].opts as any).params;
    expect(params.externalOutputs[0].name).toBe('Output Signal');
  });

  test('inner blocks are offset relative to selection bounding box', () => {
    const { renderer, created } = buildMockRenderer();
    setRendererRef(renderer);

    state.blocks.set(1, makeBlock(1, 500, 300));
    state.blocks.set(2, makeBlock(2, 700, 300));
    (state.connections as unknown[]) = [makeConn(1, 2)];
    state.selectedIds = new Set([1, 2]);

    (m as any).createSubsystemFromSelection();

    const innerBlocks: Array<Record<string, unknown>> = (created[0].opts as any).params.subsystem
      .blocks;
    // minX=500, minY=300, INNER_PAD=100
    // block 1: x = 500-500+100=100, y = 300-300+100=100
    // block 2: x = 700-500+100=300, y = 300-300+100=100
    const blk1 = innerBlocks.find((b) => b.id === 1);
    const blk2 = innerBlocks.find((b) => b.id === 2);
    expect(Number(blk1!.x)).toBeCloseTo(100);
    expect(Number(blk1!.y)).toBeCloseTo(100);
    expect(Number(blk2!.x)).toBeCloseTo(300);
    expect(Number(blk2!.y)).toBeCloseTo(100);
  });
});
//=============================================================================
