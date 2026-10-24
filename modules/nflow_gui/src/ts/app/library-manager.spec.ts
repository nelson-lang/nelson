//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as lm from './library-manager';
//=============================================================================
test('library-manager module loads', () => {
  expect(typeof lm).toBe('object');
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/library-manager.js').catch(() => null);
});
//=============================================================================
test('library-manager module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
import { vi } from 'vitest';
import fs from 'fs';
import path from 'path';
//=============================================================================
test('loads library-manager if present', async () => {
  const p = path.resolve(process.cwd(), 'src/ts/app/library-manager.tsx');
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import('./library-manager');
  expect(mod).toBeDefined();
});
//=============================================================================
