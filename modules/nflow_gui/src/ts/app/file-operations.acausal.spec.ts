//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// serializeDiagram lifts acausal physical-pin connections into the engine's
// `physicalWires` format. This must be a NO-OP for purely causal diagrams
// (byte-identical), and activate only when blocks carry acausal physical pins.
import { serializeDiagram } from './file-operations';
import { state } from './state';
//=============================================================================
function resetState() {
  state.blocks = new Map() as never;
  state.connections = [] as never;
}
//=============================================================================
test('serializeDiagram: a causal diagram gets no physicalWires field', () => {
  resetState();
  (state.blocks as Map<string, unknown>).set('g1', { id: 'g1', type: 'gain', x: 0, y: 0 });
  (state.blocks as Map<string, unknown>).set('s1', { id: 's1', type: 'scope', x: 0, y: 0 });
  (state.connections as unknown[]).push({ from: 'g1', to: 's1', fromIndex: 0, toIndex: 0 });
  const out = serializeDiagram(state as never);
  expect('physicalWires' in out).toBe(false);
  expect(out.connections).toEqual([{ from: 'g1', to: 's1', fromIndex: 0, toIndex: 0 }]);
});
//=============================================================================
test('serializeDiagram: acausal physical-pin connections lift into physicalWires', () => {
  resetState();
  // Two 2-terminal electrical blocks with physical pins p(0) / n(1).
  const twoTerminal = (id: string) => ({
    id,
    type: id.startsWith('R') ? 'Resistor' : 'Capacitor',
    x: 0,
    y: 0,
    acausal: true,
    physicalPins: [{ name: 'p' }, { name: 'n' }],
  });
  (state.blocks as Map<string, unknown>).set('R1', twoTerminal('R1'));
  (state.blocks as Map<string, unknown>).set('C1', twoTerminal('C1'));
  (state.connections as unknown[]).push(
    { from: 'R1', to: 'C1', fromIndex: 1, toIndex: 0 }, // R1.n - C1.p
    { from: 'C1', to: 'R1', fromIndex: 1, toIndex: 0 } // C1.n - R1.p
  );
  const out = serializeDiagram(state as never);
  expect(Array.isArray(out.physicalWires)).toBe(true);
  const wires = out.physicalWires as Array<{ pins: string[] }>;
  expect(wires).toHaveLength(2);
  const sets = wires.map((w) => new Set(w.pins));
  expect(sets.some((s) => s.has('R1.n') && s.has('C1.p'))).toBe(true);
  expect(sets.some((s) => s.has('C1.n') && s.has('R1.p'))).toBe(true);
  expect(out.connections).toHaveLength(0);
});
//=============================================================================
test('serializeDiagram: a sensor keeps its signal output while joining pins', () => {
  resetState();
  (state.blocks as Map<string, unknown>).set('S1', {
    id: 'S1',
    type: 'VoltageSensor',
    x: 0,
    y: 0,
    acausal: true,
    physicalPins: [{ name: 'a' }, { name: 'b' }], // indices 0,1; signal out at 2
  });
  (state.blocks as Map<string, unknown>).set('R1', {
    id: 'R1',
    type: 'Resistor',
    x: 0,
    y: 0,
    acausal: true,
    physicalPins: [{ name: 'p' }, { name: 'n' }],
  });
  (state.blocks as Map<string, unknown>).set('sc', { id: 'sc', type: 'scope', x: 0, y: 0 });
  (state.connections as unknown[]).push(
    { from: 'S1', to: 'R1', fromIndex: 0, toIndex: 0 }, // S1.a - R1.p (physical)
    { from: 'S1', to: 'sc', fromIndex: 2, toIndex: 0 } // signal out -> scope
  );
  const out = serializeDiagram(state as never);
  const wires = out.physicalWires as Array<{ pins: string[] }>;
  expect(wires).toHaveLength(1);
  expect(new Set(wires[0].pins)).toEqual(new Set(['S1.a', 'R1.p']));
  expect(out.connections).toEqual([{ from: 'S1', to: 'sc', fromIndex: 2, toIndex: 0 }]);
});
//=============================================================================
