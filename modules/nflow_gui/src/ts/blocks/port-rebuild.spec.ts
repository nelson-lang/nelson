//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { describe, test, expect } from 'vitest';
import {
  desiredPortCounts,
  actualPortCounts,
  portCountsChanged,
  survivingConnections,
} from './port-rebuild';
//=============================================================================
const ports = (inputs: number, outputs: number) => [
  ...Array.from({ length: inputs }, (_, index) => ({ type: 'in', index })),
  ...Array.from({ length: outputs }, (_, index) => ({ type: 'out', index })),
];
//=============================================================================
describe('desiredPortCounts', () => {
  test('mux: inputs follow the Inputs param, one output', () => {
    expect(desiredPortCounts({ type: 'mux', params: { Inputs: 4 } })).toEqual({
      inputs: 4,
      outputs: 1,
    });
  });
  test('demux: outputs follow the Outputs param, one input', () => {
    expect(desiredPortCounts({ type: 'demux', params: { Outputs: 3 } })).toEqual({
      inputs: 1,
      outputs: 3,
    });
  });
  test('sum: inputs are the sign count', () => {
    expect(desiredPortCounts({ type: 'sum', params: { Inputs: '+-+' } })).toEqual({
      inputs: 3,
      outputs: 1,
    });
    expect(desiredPortCounts({ type: 'sum', params: { Inputs: '++' } })?.inputs).toBe(2);
  });
  test('multiportSwitch: one control input plus the data inputs', () => {
    expect(desiredPortCounts({ type: 'multiportSwitch', params: { DataPortCount: 3 } })).toEqual({
      inputs: 4,
      outputs: 1,
    });
  });
  test('non-structural blocks return null', () => {
    expect(desiredPortCounts({ type: 'gain', params: { Gain: 2 } })).toBeNull();
    expect(desiredPortCounts({ type: 'scope', params: {} })).toBeNull();
  });
  test('clamps to sane minimums', () => {
    expect(desiredPortCounts({ type: 'mux', params: { Inputs: 0 } })?.inputs).toBe(1);
    expect(desiredPortCounts({ type: 'sum', params: { Inputs: '' } })?.inputs).toBe(1);
  });
});
//=============================================================================
describe('actualPortCounts', () => {
  test('counts rendered in/out ports', () => {
    expect(actualPortCounts({ ports: ports(3, 1) })).toEqual({ inputs: 3, outputs: 1 });
  });
  test('tolerates a missing ports array', () => {
    expect(actualPortCounts({})).toEqual({ inputs: 0, outputs: 0 });
  });
});
//=============================================================================
describe('portCountsChanged', () => {
  test('true when the param count differs from the rendered ports', () => {
    expect(portCountsChanged({ type: 'mux', params: { Inputs: 4 }, ports: ports(2, 1) })).toBe(
      true
    );
  });
  test('false when they already match', () => {
    expect(portCountsChanged({ type: 'mux', params: { Inputs: 2 }, ports: ports(2, 1) })).toBe(
      false
    );
  });
  test('false for non-structural blocks', () => {
    expect(portCountsChanged({ type: 'gain', params: { Gain: 9 }, ports: ports(1, 1) })).toBe(
      false
    );
  });
});
//=============================================================================
describe('survivingConnections', () => {
  const conns = [
    { from: 'src', to: 'mux', fromIndex: 0, toIndex: 0 },
    { from: 'src2', to: 'mux', fromIndex: 0, toIndex: 2 }, // lost when mux shrinks to 2 inputs
    { from: 'mux', to: 'sink', fromIndex: 0, toIndex: 0 },
  ];
  test('drops wires addressing a removed input port', () => {
    const kept = survivingConnections(conns, 'mux', { inputs: 2, outputs: 1 });
    expect(kept).toHaveLength(2);
    expect(kept.find((c) => c.toIndex === 2)).toBeUndefined();
  });
  test('keeps every wire when the ports still cover them', () => {
    const kept = survivingConnections(conns, 'mux', { inputs: 3, outputs: 1 });
    expect(kept).toHaveLength(3);
  });
  test('validates the output side too', () => {
    const out = [{ from: 'demux', to: 'sink', fromIndex: 2, toIndex: 0 }];
    expect(survivingConnections(out, 'demux', { inputs: 1, outputs: 2 })).toHaveLength(0);
    expect(survivingConnections(out, 'demux', { inputs: 1, outputs: 3 })).toHaveLength(1);
  });
});
//=============================================================================
