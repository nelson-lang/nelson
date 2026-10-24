//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { computePortYs } from './helpers';
//=============================================================================
test('computePortYs returns array for given ports', () => {
  const ports = [{}, {}, {}];
  const ys = computePortYs(ports, 12);
  expect(Array.isArray(ys)).toBe(true);
});
//=============================================================================
