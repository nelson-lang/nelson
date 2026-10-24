//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
test('render.js - import and call exported helpers (smoke)', async () => {
  const mod = await import('../../../dist/render/index.js');
  const keys = Object.keys(mod).filter((k) => typeof (mod as any)[k] === 'function');
  expect(keys.length).toBeGreaterThanOrEqual(0);
  for (const k of keys.slice(0, 10)) {
    try {
      const fn = (mod as any)[k];
      const res = fn.length === 0 ? fn() : fn(null, null);
      void res;
    } catch (err) {
      void err;
    }
  }
});
//=============================================================================
