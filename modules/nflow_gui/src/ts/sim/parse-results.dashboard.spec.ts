//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test, vi } from 'vitest';
import { applyDashboardUpdates } from './parse-results.js';
//=============================================================================
test('dispatches typed dashboard updates without losing scope samples', () => {
  const listener = vi.fn();
  document.addEventListener('nf-dashboard-update', listener);
  applyDashboardUpdates([
    {
      id: 'scope-ui',
      time: 0.2,
      value: [1, 2],
      valueType: 'double',
      connected: true,
      times: [0, 0.1, 0.2],
      series: [[0, 1, 2]],
    },
  ] as any);
  expect(listener).toHaveBeenCalledOnce();
  expect((listener.mock.calls[0][0] as CustomEvent).detail.series[0]).toEqual([0, 1, 2]);
  document.removeEventListener('nf-dashboard-update', listener);
});
//=============================================================================
