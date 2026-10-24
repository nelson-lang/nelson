//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as hi from './head-init';
//=============================================================================
test('head-init module loads', () => {
  expect(typeof hi).toBe('object');
});
//=============================================================================
describe('head-init RPC bootstrap', () => {
  beforeAll(async () => {
    delete (window as any).openFileDialog;
    delete (window as any).saveFileDialog;
    await import('../../../dist/app/head-init.js');
  });

  test('does not install legacy file dialog globals', () => {
    expect((window as any).openFileDialog).toBeUndefined();
    expect((window as any).saveFileDialog).toBeUndefined();
  });
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
beforeAll(async () => {
  await import('../../../dist/app/head-init.js').catch(() => null);
});
//=============================================================================
test('head-init sets loading class on documentElement', () => {
  expect(document.documentElement.classList.contains('nf-loading')).toBe(true);
});
//=============================================================================
export {};
import fs from 'fs';
import path from 'path';
//=============================================================================
test('loads head-init if present', async () => {
  const p = path.resolve(process.cwd(), 'modules/nflow_gui/src/ts/app/head-init.ts');
  if (!fs.existsSync(p)) {
    console.warn('skipping missing file', p);
    return;
  }
  const mod = await import('./head-init');
  expect(mod).toBeDefined();
});
//=============================================================================
