//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import type { Block } from '../types.js';
import {
  bindContinuousKeyboard,
  createDashboardThrottle,
  dashboardRatio,
  dashboardValue,
  formatDashboardValue,
} from './dashboard-values.js';
//=============================================================================
describe('dashboard value conversion', () => {
  it('maps linear and logarithmic scales in both directions', () => {
    expect(dashboardRatio(50, [0, 100])).toBeCloseTo(0.5);
    expect(dashboardValue(0.25, [0, 100])).toBeCloseTo(25);
    expect(dashboardRatio(10, [1, 100], 'Log')).toBeCloseTo(0.5);
    expect(dashboardValue(0.5, [1, 100], 'Log')).toBeCloseTo(10);
  });

  it('clamps and snaps values to the configured interval', () => {
    expect(dashboardValue(-1, [0, 10], 'Linear', 2)).toBe(0);
    expect(dashboardValue(0.54, [0, 10], 'Linear', 2)).toBe(6);
    expect(dashboardValue(2, [0, 10], 'Linear', 2)).toBe(10);
  });
});
//=============================================================================
describe('dashboard number formatting', () => {
  it.each([
    ['short', 12.5, '12.5000'],
    ['long', 12.5, '12.500000000000000'],
    ['shortE', 12.5, '1.2500e+1'],
    ['longE', 12.5, '1.250000000000000e+1'],
    ['shortG', 12.5, '12.500'],
    ['bank', 12.5, '12.50'],
    ['+', 12.5, '+'],
    ['Integer', 12.5, '13'],
    ['rat', 0.5, '1/2'],
  ])('supports %s', (format, value, expected) => {
    expect(formatDashboardValue(value, format)).toBe(expected);
  });

  it('supports hexadecimal and custom formats', () => {
    expect(formatDashboardValue(1, 'hex')).toBe('3ff0000000000000');
    expect(formatDashboardValue(12.5, 'Custom', '%+08.2f')).toBe('00+12.50');
    expect(formatDashboardValue(Number.NaN)).toBe('--');
  });
});
//=============================================================================
describe('dashboard input scheduling', () => {
  it('retains the latest throttled value and flushes it', () => {
    const callback = vi.fn();
    const throttle = createDashboardThrottle(callback, 1000);
    throttle.push(1);
    throttle.push(2);
    throttle.push(3);
    expect(callback).toHaveBeenCalledTimes(1);
    throttle.flush();
    expect(callback.mock.calls).toEqual([[1], [3]]);
  });

  it('supports arrows and range boundary keys', () => {
    const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const block = { group, params: { Limits: [0, 2, 10] } } as unknown as Block;
    let value = 4;
    bindContinuousKeyboard(
      block,
      () => value,
      (next) => {
        value = next;
      }
    );

    group.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true }));
    expect(value).toBe(6);
    group.dispatchEvent(new KeyboardEvent('keydown', { key: 'Home', bubbles: true }));
    expect(value).toBe(0);
    group.dispatchEvent(new KeyboardEvent('keydown', { key: 'End', bubbles: true }));
    expect(value).toBe(10);
    expect(group.getAttribute('role')).toBe('slider');
  });
});
//=============================================================================
