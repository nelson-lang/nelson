//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for the post-build structural validator. The loaded-library registry
// is mocked (same as block-catalog.spec), so these run with NO engine. They pin
// that the validator flags the silent failures (a dangling signal input, a
// physical model off DAE) and stays quiet on a well-formed model, without nagging
// about acausal pin blocks or dashboards.
import { beforeEach, expect, test, vi } from 'vitest';
//=============================================================================
const { librariesRef } = vi.hoisted(() => ({
  librariesRef: { map: new Map<string, unknown>() },
}));
vi.mock('./library-loader.js', () => ({
  getLoadedLibraries: () => librariesRef.map,
}));
//=============================================================================
import { validateBuiltModel } from './assistant-validate.js';
//=============================================================================
beforeEach(() => {
  librariesRef.map = new Map<string, unknown>([
    [
      'builtin.math',
      {
        id: 'builtin.math',
        library: {
          blocks: [
            { type: 'constant', inputs: [], outputs: [{}] },
            { type: 'gain', inputs: [{}], outputs: [{}] },
            { type: 'scope', inputs: [{}], outputs: [] },
          ],
        },
      },
    ],
    [
      'builtin.acausal_electrical',
      {
        id: 'builtin.acausal_electrical',
        library: {
          blocks: [
            {
              type: 'Resistor',
              inputs: [],
              outputs: [],
              physicalPins: [{ name: 'p' }, { name: 'n' }],
            },
          ],
        },
      },
    ],
    [
      'builtin.dashboard',
      {
        id: 'builtin.dashboard',
        library: { blocks: [{ type: 'gauge', inputs: [], outputs: [] }] },
      },
    ],
  ]);
});
//=============================================================================
test('no issues for a fully wired signal model', () => {
  const doc = {
    blocks: [
      { id: 'Src', type: 'constant' },
      { id: 'K', type: 'gain' },
      { id: 'Scope', type: 'scope' },
    ],
    connections: [
      { from: 'Src', to: 'K', fromIndex: 0, toIndex: 0 },
      { from: 'K', to: 'Scope', fromIndex: 0, toIndex: 0 },
    ],
    solver: 'discrete',
  };
  expect(validateBuiltModel(doc)).toEqual([]);
});
//=============================================================================
test('flags a block whose signal input is left dangling', () => {
  const doc = {
    blocks: [
      { id: 'Src', type: 'constant' },
      { id: 'K', type: 'gain' },
      { id: 'Scope', type: 'scope' },
    ],
    // K feeds the scope, but nothing feeds K.
    connections: [{ from: 'K', to: 'Scope', fromIndex: 0, toIndex: 0 }],
    solver: 'discrete',
  };
  const issues = validateBuiltModel(doc);
  expect(issues).toHaveLength(1);
  expect(issues[0]).toContain('"K"');
  expect(issues[0]).toContain('gain');
});
//=============================================================================
test('flags a physical model left on a non-DAE solver', () => {
  const doc = {
    blocks: [{ id: 'R1', type: 'Resistor' }],
    connections: [],
    physicalWires: [{ pins: ['R1.p', 'R1.n'] }],
    solver: 'discrete',
  };
  const issues = validateBuiltModel(doc);
  expect(issues.some((i) => i.includes("Solver='dae'"))).toBe(true);
});
//=============================================================================
test('no DAE complaint when the physical model already uses dae', () => {
  const doc = {
    blocks: [{ id: 'R1', type: 'Resistor' }],
    connections: [],
    physicalWires: [{ pins: ['R1.p', 'R1.n'] }],
    solver: 'dae',
  };
  expect(validateBuiltModel(doc)).toEqual([]);
});
//=============================================================================
test('acausal pin blocks and dashboards are not flagged for missing inputs', () => {
  const doc = {
    blocks: [
      { id: 'R1', type: 'Resistor' },
      { id: 'G', type: 'gauge' },
    ],
    connections: [],
    physicalWires: [{ pins: ['R1.p', 'R1.n'] }],
    solver: 'dae',
  };
  expect(validateBuiltModel(doc)).toEqual([]);
});
//=============================================================================
test('stays quiet when no libraries are loaded (cannot resolve ports)', () => {
  librariesRef.map = new Map();
  const doc = { blocks: [{ id: 'K', type: 'gain' }], connections: [], solver: 'discrete' };
  expect(validateBuiltModel(doc)).toEqual([]);
});
//=============================================================================
test('empty or missing model yields no issues', () => {
  expect(validateBuiltModel(null)).toEqual([]);
  expect(validateBuiltModel({})).toEqual([]);
  expect(validateBuiltModel({ blocks: [] })).toEqual([]);
});
//=============================================================================
