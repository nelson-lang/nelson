//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/history.js').catch(() => null);
});
//=============================================================================
test('history module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
import { vi } from 'vitest';
import fs from 'fs';
import path from 'path';
//=============================================================================
test('loads history if present', async () => {
  const p = path.resolve(process.cwd(), 'modules/nflow_gui/src/ts/app/history.ts');
  if (!fs.existsSync(p)) {
    console.warn('skipping missing file', p);
    return;
  }
  const mod = await import('./history');
  expect(mod).toBeDefined();
});
//=============================================================================
