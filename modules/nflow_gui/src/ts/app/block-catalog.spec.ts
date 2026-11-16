//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for the live block catalog. The loaded-library registry is mocked,
// so these run with NO engine (what CI has): they pin that each loaded block is
// listed once with its scripted path, parameter names, and signal ports or
// physical pins — the data the assistant needs instead of a hand-kept skill list.
import { beforeEach, expect, test, vi } from 'vitest';
//=============================================================================
const { librariesRef } = vi.hoisted(() => ({
  librariesRef: { map: new Map<string, unknown>() },
}));
vi.mock('./library-loader.js', () => ({
  getLoadedLibraries: () => librariesRef.map,
}));
//=============================================================================
import { buildBlockCatalog } from './block-catalog.js';
//=============================================================================
beforeEach(() => {
  librariesRef.map = new Map();
});
//=============================================================================
test('is empty when no library is loaded', () => {
  expect(buildBlockCatalog()).toBe('');
});
//=============================================================================
test('lists each block with its path, parameters and ports or pins', () => {
  librariesRef.map = new Map<string, unknown>([
    [
      'builtin.math',
      {
        id: 'builtin.math',
        library: {
          blocks: [{ type: 'gain', defaultParams: { Gain: 1 }, inputs: [{}], outputs: [{}] }],
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
              defaultParams: { R: 1000 },
              inputs: [],
              outputs: [],
              physicalPins: [{ name: 'p' }, { name: 'n' }],
            },
            {
              type: 'VoltageSensor',
              defaultParams: {},
              inputs: [],
              outputs: [{}],
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
        library: {
          blocks: [{ type: 'dashboardGauge', defaultParams: { Limits: [], LabelPosition: 'Top' } }],
        },
      },
    ],
  ]);

  const catalog = buildBlockCatalog();
  // Builtin prefix dropped; signal ports counted.
  expect(catalog).toContain('- nflow/math/gain {Gain} 1in/1out');
  // Physical pins listed by name; a sensor's signal output flagged.
  expect(catalog).toContain('- nflow/acausal_electrical/Resistor {R} pins: p,n');
  expect(catalog).toContain('- nflow/acausal_electrical/VoltageSensor pins: p,n (+1 signal out)');
  // A block with no ports still lists its parameters.
  expect(catalog).toContain('- nflow/dashboard/dashboardGauge {Limits,LabelPosition} 0in/0out');
});
//=============================================================================
test('skips libraries with no blocks and blocks with no type', () => {
  librariesRef.map = new Map<string, unknown>([
    ['builtin.empty', { id: 'builtin.empty', library: { blocks: [] } }],
    ['builtin.bad', { id: 'builtin.bad', library: { blocks: [{ defaultParams: { X: 1 } }] } }],
  ]);
  expect(buildBlockCatalog()).toBe('');
});
//=============================================================================
