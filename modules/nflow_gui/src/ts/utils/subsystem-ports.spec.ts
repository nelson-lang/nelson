//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment node */
/* eslint-disable @typescript-eslint/no-explicit-any */
let ports: any;
beforeEach(async () => {
  ports = await import('../../../dist/utils/subsystem-ports.js');
});
//=============================================================================
test('collectExternalPorts filters by type and isExternalPort and sorts by y,x,id', () => {
  const blocks = [
    {
      id: 'b3',
      type: FieldNames.kLabelSource,
      params: { isExternalPort: true, name: 'C' },
      x: 5,
      y: 20,
    },
    {
      id: 'b1',
      type: FieldNames.kLabelSource,
      params: { isExternalPort: true, name: 'A' },
      x: 1,
      y: 10,
    },
    {
      id: 'b2',
      type: FieldNames.kLabelSource,
      params: { isExternalPort: true, name: 'B' },
      x: 2,
      y: 10,
    },
    { id: 'b4', type: FieldNames.kLabelSink, params: { isExternalPort: false }, x: 0, y: 0 },
  ];
  const res = ports.collectExternalPorts(blocks, FieldNames.kLabelSource);
  expect(Array.isArray(res)).toBe(true);
  expect(res.map((p: any) => p.id)).toEqual(['b1', 'b2', 'b3']);
});
//=============================================================================
test('stabilizeExternalPortOrder preserves previous order when ids match', () => {
  const prev = [
    { id: 'a', name: 'A' },
    { id: 'b', name: 'B' },
  ];
  const next = [
    { id: 'b', name: 'B' },
    { id: 'a', name: 'A' },
  ];
  const out = ports.stabilizeExternalPortOrder(next, prev);
  expect(out.map((p: any) => p.id)).toEqual(['a', 'b']);
});
//=============================================================================
test('stabilizeExternalPortOrder returns next when sets differ', () => {
  const prev = [{ id: 'a' }];
  const next = [{ id: 'b' }];
  const out = ports.stabilizeExternalPortOrder(next, prev);
  expect(out).toEqual([{ id: 'b', name: 'b' }]);
});
//=============================================================================
test('externalPortsChanged detects differences', () => {
  const a = [
    { id: 'x', name: 'X' },
    { id: 'y', name: 'Y' },
  ];
  const b = [
    { id: 'x', name: 'X' },
    { id: 'y', name: 'Y' },
  ];
  expect(ports.externalPortsChanged(a, b)).toBe(false);
  const c = [{ id: 'x', name: 'X' }];
  expect(ports.externalPortsChanged(a, c)).toBe(true);
  const d = [
    { id: 'x', name: 'Z' },
    { id: 'y', name: 'Y' },
  ];
  expect(ports.externalPortsChanged(a, d)).toBe(true);
});
//=============================================================================
describe('type contracts', () => {
  test('collectExternalPorts returns array of objects with id and name', () => {
    const out = ports.collectExternalPorts([], FieldNames.kLabelSource);
    expect(Array.isArray(out)).toBe(true);
    out.forEach((p: any) => {
      expect(typeof p).toBe('object');
      if (p != null) {
        expect(typeof p.id === 'string' || typeof p.id === 'number').toBe(true);
      }
    });
  });
  test('stabilizeExternalPortOrder preserves array shape and externalPortsChanged returns boolean', () => {
    expect(Array.isArray(ports.stabilizeExternalPortOrder([], []))).toBe(true);
    expect(typeof ports.externalPortsChanged([], [])).toBe('boolean');
  });
});
//=============================================================================
export {};
//=============================================================================
