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
  rewriteConnectionsToPhysicalWires,
  exportAcausalDocument,
  blockPinInfoFromManifest,
  pinInfoLookupFor,
  type EditorConnection,
  type BlockPinInfo,
} from './acausal-export';
//=============================================================================
const twoTerminal = (): BlockPinInfo => ({
  acausal: true,
  fromPinName: (i) => (i === 0 ? 'p' : i === 1 ? 'n' : null),
  toPinName: (i) => (i === 0 ? 'p' : i === 1 ? 'n' : null),
});
//=============================================================================
test('rewrite: an RC loop of directed edges becomes physicalWires nets', () => {
  // src(0=p,1=n), R(0=p,1=n), C(0=p,1=n). Wire src.p-R.p, R.n-C.p, C.n-src.n.
  const lookup = (id: string) => (['src', 'R', 'C'].includes(id) ? twoTerminal() : undefined);
  const conns: EditorConnection[] = [
    { from: 'src', to: 'R', fromIndex: 0, toIndex: 0 }, // src.p - R.p
    { from: 'R', to: 'C', fromIndex: 1, toIndex: 0 }, // R.n - C.p
    { from: 'C', to: 'src', fromIndex: 1, toIndex: 1 }, // C.n - src.n
  ];
  const res = rewriteConnectionsToPhysicalWires(conns, lookup);
  expect(res.connections).toHaveLength(0); // all physical
  expect(res.physicalWires).toHaveLength(3);
  const sets = res.physicalWires.map((w) => new Set(w.pins));
  expect(sets.some((s) => s.has('src.p') && s.has('R.p'))).toBe(true);
  expect(sets.some((s) => s.has('R.n') && s.has('C.p'))).toBe(true);
  expect(sets.some((s) => s.has('C.n') && s.has('src.n'))).toBe(true);
});
//=============================================================================
test('rewrite: a sensor keeps its signal output but joins its physical pins', () => {
  const sensor: BlockPinInfo = {
    acausal: true,
    fromPinName: (i) => (i === 0 ? 'a' : i === 1 ? 'b' : null), // index 2 = signal
    toPinName: (i) => (i === 0 ? 'a' : i === 1 ? 'b' : null),
  };
  const scope: BlockPinInfo = { acausal: false };
  const lookup = (id: string) =>
    id === 'S' ? sensor : id === 'R' ? twoTerminal() : id === 'sc' ? scope : undefined;
  const conns: EditorConnection[] = [
    { from: 'S', to: 'R', fromIndex: 0, toIndex: 0 }, // S.a - R.p (physical)
    { from: 'S', to: 'sc', fromIndex: 2, toIndex: 0 }, // signal out -> scope
  ];
  const res = rewriteConnectionsToPhysicalWires(conns, lookup);
  expect(res.physicalWires).toHaveLength(1);
  expect(new Set(res.physicalWires[0].pins)).toEqual(new Set(['S.a', 'R.p']));
  expect(res.connections).toEqual([{ from: 'S', to: 'sc', fromIndex: 2, toIndex: 0 }]);
});
//=============================================================================
test('rewrite: a purely causal diagram is unchanged', () => {
  const lookup = () => ({ acausal: false } as BlockPinInfo);
  const conns: EditorConnection[] = [
    { from: 'a', to: 'b', fromIndex: 0, toIndex: 0 },
    { from: 'b', to: 'c', fromIndex: 0, toIndex: 0 },
  ];
  const res = rewriteConnectionsToPhysicalWires(conns, lookup);
  expect(res.connections).toEqual(conns);
  expect(res.physicalWires).toHaveLength(0);
});
//=============================================================================
test('exportAcausalDocument omits physicalWires for a causal doc, adds it otherwise', () => {
  const causal = { blocks: [{ id: 'a' }], connections: [{ from: 'a', to: 'b' }] };
  const out1 = exportAcausalDocument(causal, () => ({ acausal: false }));
  expect('physicalWires' in out1).toBe(false);

  const acausal = {
    blocks: [{ id: 'R' }, { id: 'C' }],
    connections: [{ from: 'R', to: 'C', fromIndex: 1, toIndex: 0 }],
  };
  const out2 = exportAcausalDocument(acausal, (id) =>
    ['R', 'C'].includes(id) ? twoTerminal() : undefined
  );
  expect(out2.physicalWires).toHaveLength(1);
  expect(new Set(out2.physicalWires![0].pins)).toEqual(new Set(['R.n', 'C.p']));
  expect(out2.connections).toHaveLength(0);
  expect(out2.blocks).toBe(acausal.blocks); // blocks passed through
});
//=============================================================================
test('blockPinInfoFromManifest maps physical-pin indices, ignores signal ports', () => {
  const manifest = {
    acausal: true,
    physicalPins: [{ name: 'p' }, { name: 'n' }],
    inputs: [],
    outputs: [{}], // a signal output at index 2 (after the two physical pins)
  };
  const info = blockPinInfoFromManifest(manifest);
  expect(info.acausal).toBe(true);
  expect(info.fromPinName!(0)).toBe('p');
  expect(info.fromPinName!(1)).toBe('n');
  expect(info.fromPinName!(2)).toBeNull(); // signal port -> not a physical pin
  expect(info.toPinName!(0)).toBe('p');
  expect(blockPinInfoFromManifest({ acausal: false }).acausal).toBe(false);
  expect(blockPinInfoFromManifest(undefined).acausal).toBe(false);
});
//=============================================================================
test('pinInfoLookupFor + exportAcausalDocument: manifest-driven RC export', () => {
  const manifests: Record<string, { acausal: boolean; physicalPins: Array<{ name: string }> }> = {
    Resistor: { acausal: true, physicalPins: [{ name: 'p' }, { name: 'n' }] },
    Capacitor: { acausal: true, physicalPins: [{ name: 'p' }, { name: 'n' }] },
  };
  const doc = {
    blocks: [
      { id: 'R', type: 'Resistor' },
      { id: 'C', type: 'Capacitor' },
    ],
    connections: [
      { from: 'R', to: 'C', fromIndex: 1, toIndex: 0 }, // R.n - C.p
      { from: 'C', to: 'R', fromIndex: 1, toIndex: 0 }, // C.n - R.p
    ] as EditorConnection[],
  };
  const lookup = pinInfoLookupFor(doc.blocks, (t) => manifests[t]);
  const out = exportAcausalDocument(doc, lookup);
  expect(out.connections).toHaveLength(0);
  expect(out.physicalWires).toHaveLength(2);
  const sets = out.physicalWires!.map((w) => new Set(w.pins));
  expect(sets.some((s) => s.has('R.n') && s.has('C.p'))).toBe(true);
  expect(sets.some((s) => s.has('C.n') && s.has('R.p'))).toBe(true);
});
//=============================================================================
