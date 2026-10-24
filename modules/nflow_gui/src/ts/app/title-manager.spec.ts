//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as tm from './title-manager';
//=============================================================================
test('title-manager module loads', () => {
  expect(typeof tm).toBe('object');
});
//=============================================================================
import { buildTitleName } from '../../../dist/app/title-manager.js';
//=============================================================================
describe('buildTitleName', () => {
  test('returns app name when no diagram name provided', () => {
    expect(buildTitleName()).toBe('NFlow');
  });

  test('trims and appends diagram name', () => {
    expect(buildTitleName('  My Diagram  ')).toBe('NFlow - My Diagram');
  });

  test('works with non-string input', () => {
    expect(buildTitleName(123)).toBe('NFlow - 123');
  });
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/title-manager.js');
});
//=============================================================================
test('buildTitleName returns app name when blank', () => {
  expect(mod.buildTitleName('')).toBe('NFlow');
  expect(mod.buildTitleName(null)).toBe('NFlow');
});
//=============================================================================
test('buildTitleName includes diagram name', () => {
  expect(mod.buildTitleName('My Diagram')).toBe('NFlow - My Diagram');
  expect(mod.buildTitleName('  Trim  ')).toBe('NFlow - Trim');
});
//=============================================================================
export {};
//=============================================================================
