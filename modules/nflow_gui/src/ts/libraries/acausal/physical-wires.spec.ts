//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  buildPhysicalWires,
  validateNetDomains,
  partitionConnections,
  pinKey,
  splitPinKey,
  type PhysicalConnection,
} from './physical-wires';
//=============================================================================
test('buildPhysicalWires: an RC loop collapses to three nodes', () => {
  // src.p - R.p ; R.n - C.p ; C.n - src.n  -> nodes {src.p,R.p}, {R.n,C.p}, {C.n,src.n}
  const conns: PhysicalConnection[] = [
    { a: 'src.p', b: 'R.p' },
    { a: 'R.n', b: 'C.p' },
    { a: 'C.n', b: 'src.n' },
  ];
  const wires = buildPhysicalWires(conns);
  expect(wires).toHaveLength(3);
  const asSets = wires.map((w) => new Set(w.pins));
  expect(asSets.some((s) => s.has('src.p') && s.has('R.p') && s.size === 2)).toBe(true);
  expect(asSets.some((s) => s.has('R.n') && s.has('C.p') && s.size === 2)).toBe(true);
  expect(asSets.some((s) => s.has('C.n') && s.has('src.n') && s.size === 2)).toBe(true);
});
//=============================================================================
test('buildPhysicalWires: transitive connections merge into one N-ary net', () => {
  const conns: PhysicalConnection[] = [
    { a: 'a.p', b: 'b.p' },
    { a: 'b.p', b: 'c.p' },
    { a: 'c.p', b: 'd.p' },
  ];
  const wires = buildPhysicalWires(conns);
  expect(wires).toHaveLength(1);
  expect(new Set(wires[0].pins)).toEqual(new Set(['a.p', 'b.p', 'c.p', 'd.p']));
});
//=============================================================================
test('buildPhysicalWires: ignores self-loops and dedupes endpoints', () => {
  const conns: PhysicalConnection[] = [
    { a: 'x.p', b: 'x.p' }, // self loop -> ignored
    { a: 'x.p', b: 'y.p' },
    { a: 'y.p', b: 'x.p' }, // redundant reverse -> same net
  ];
  const wires = buildPhysicalWires(conns);
  expect(wires).toHaveLength(1);
  expect(wires[0].pins.sort()).toEqual(['x.p', 'y.p']);
});
//=============================================================================
test('buildPhysicalWires: is deterministic in first-seen order', () => {
  const conns: PhysicalConnection[] = [
    { a: 'n1.a', b: 'n1.b' },
    { a: 'n2.a', b: 'n2.b' },
  ];
  const a = JSON.stringify(buildPhysicalWires(conns));
  const b = JSON.stringify(buildPhysicalWires(conns));
  expect(a).toBe(b);
  expect(buildPhysicalWires(conns)[0].pins[0]).toBe('n1.a');
});
//=============================================================================
test('validateNetDomains: accepts single-domain nets', () => {
  const wires = [{ pins: ['R.p', 'C.p'] }];
  const res = validateNetDomains(wires, () => 'electrical');
  expect(res.ok).toBe(true);
});
//=============================================================================
test('validateNetDomains: rejects a net that mixes two domains', () => {
  const wires = [{ pins: ['R.p', 'HeatCap.port'] }];
  const domainOf = (pin: string) => (pin.startsWith('R') ? 'electrical' : 'thermal');
  const res = validateNetDomains(wires, domainOf);
  expect(res.ok).toBe(false);
  expect(res.error).toContain('electrical');
  expect(res.error).toContain('thermal');
  expect(res.net?.pins).toEqual(['R.p', 'HeatCap.port']);
});
//=============================================================================
test('validateNetDomains: unknown pins do not force a domain', () => {
  const wires = [{ pins: ['R.p', 'label.free'] }];
  const domainOf = (pin: string) => (pin === 'R.p' ? 'electrical' : undefined);
  expect(validateNetDomains(wires, domainOf).ok).toBe(true);
});
//=============================================================================
test('partitionConnections: separates physical from signal edges', () => {
  const conns: PhysicalConnection[] = [
    { a: 'R.p', b: 'C.p' }, // physical <-> physical
    { a: 'Sensor.out', b: 'Scope.in' }, // signal <-> signal
    { a: 'R.p', b: 'Scope.in' }, // mixed -> signal bucket
  ];
  const physicalPins = new Set(['R.p', 'C.p']);
  const { physical, signal } = partitionConnections(conns, (p) => physicalPins.has(p));
  expect(physical).toEqual([{ a: 'R.p', b: 'C.p' }]);
  expect(signal).toHaveLength(2);
});
//=============================================================================
test('pinKey / splitPinKey round-trip, block ids may contain dots', () => {
  expect(pinKey('R1', 'p')).toBe('R1.p');
  expect(splitPinKey('R1.p')).toEqual({ blockId: 'R1', pinName: 'p' });
  expect(splitPinKey('sub.R1.p')).toEqual({ blockId: 'sub.R1', pinName: 'p' });
});
//=============================================================================
