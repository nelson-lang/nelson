//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import type { Block } from '../inspector.js';
import {
  collectDashboardTargets,
  dashboardBindingKinds,
  validateDashboardBinding,
} from './dashboard.js';
//=============================================================================
describe('dashboard inspector binding model', () => {
  it('restricts bindings according to the block family', () => {
    expect(dashboardBindingKinds('dashboardGauge')).toEqual(['signal']);
    expect(dashboardBindingKinds('dashboardSlider')).toEqual(['parameter', 'variable']);
    expect(dashboardBindingKinds('dashboardCallbackButton')).toEqual([]);
  });

  it('builds stable nested paths and excludes dashboard targets', () => {
    const blocks = [
      {
        id: 'sub',
        type: 'subsystem',
        params: {
          name: 'Plant',
          subsystem: {
            blocks: [
              { id: 'gain', type: 'gain', params: { name: 'Gain', Gain: 2 } },
              { id: 'ui', type: 'dashboardDisplay', params: {} },
            ],
          },
        },
      },
    ] as unknown as Block[];
    const targets = collectDashboardTargets(blocks);
    expect(targets.map((target) => target.path)).toEqual([['sub'], ['sub', 'gain']]);
    expect(targets[1].label).toContain('Plant / Gain');
  });

  it('validates parameters, variables, elements and signal ports', () => {
    const target = {
      id: 'gain',
      type: 'gain',
      outputs: 1,
      params: { Gain: 2, Vector: [10, 20], Label: 'x' },
    } as unknown as Block;
    const targets = collectDashboardTargets([target]);
    const variables = {
      scalar: 3,
      vector: [
        [1, 2],
        [3, 4],
      ],
    };

    expect(
      validateDashboardBinding(
        'dashboardSlider',
        { kind: 'parameter', blockPath: ['gain'], paramName: 'Vector', element: '(2)' },
        targets,
        variables
      ).valid
    ).toBe(true);
    expect(
      validateDashboardBinding(
        'dashboardSlider',
        { kind: 'variable', varName: 'vector', element: '(2,1)' },
        targets,
        variables
      ).valid
    ).toBe(true);
    expect(
      validateDashboardBinding(
        'dashboardSlider',
        { kind: 'parameter', blockPath: ['gain'], paramName: 'Label' },
        targets,
        variables
      ).message
    ).toBe('Target must be numeric and scalar');
    expect(
      validateDashboardBinding(
        'dashboardDisplay',
        { kind: 'signal', blockPath: ['gain'], outputPortIndex: 2 },
        targets,
        variables
      ).message
    ).toBe('Invalid output port');
  });
});
//=============================================================================
