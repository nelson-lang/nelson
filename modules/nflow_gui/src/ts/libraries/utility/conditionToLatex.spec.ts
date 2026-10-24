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
import { conditionToLatex } from './helpers';
//=============================================================================
test('conditionToLatex returns string for simple condition', () => {
  const out = conditionToLatex('x>0');
  expect(typeof out).toBe('string');
});
//=============================================================================
