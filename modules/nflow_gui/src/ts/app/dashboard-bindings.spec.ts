//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, expect, test } from 'vitest';
import { installDashboardBindingEvents, setDashboardElement } from './dashboard-bindings.js';
import { state } from './state.js';
//=============================================================================
installDashboardBindingEvents();
beforeEach(() => {
  state.blocks.clear();
  state.variables = {};
  state.variablesText = '';
});
//=============================================================================
test('updates a nested parameter through a stable block path', () => {
  const gain = { id: 'gain', params: { Gain: 2 } };
  state.blocks.set(7, { id: 'sub', params: { subsystem: { blocks: [gain] } } });
  document.dispatchEvent(
    new CustomEvent('nf-dashboard-control-change', {
      detail: {
        value: 9,
        binding: { kind: 'parameter', blockPath: ['sub', 'gain'], paramName: 'Gain' },
      },
    })
  );
  expect(gain.params.Gain).toBe(9);
});
//=============================================================================
test('updates and serializes a composite variable element', () => {
  state.variables.matrix = [
    [1, 2],
    [3, 4],
  ];
  state.variablesText = 'matrix = [1, 2; 3, 4];';
  document.dispatchEvent(
    new CustomEvent('nf-dashboard-control-change', {
      detail: {
        value: 8,
        binding: { kind: 'variable', varName: 'matrix', element: '(2,1)' },
      },
    })
  );
  expect(state.variables.matrix).toEqual([
    [1, 2],
    [8, 4],
  ]);
  expect(state.variablesText).toContain('matrix = [1, 2; 8, 4];');
});
//=============================================================================
test('rejects invalid composite indices without changing the value', () => {
  const value = [1, 2];
  expect(setDashboardElement(value, '(3)', 5)).toBe(value);
  expect(setDashboardElement(value, '(bad)', 5)).toBe(value);
});
//=============================================================================
