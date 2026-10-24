//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
test('loads route-worker.ts if present', async () => {
  const mod = await import('./wire/route-worker');
  expect(mod).toBeDefined();
});
//=============================================================================
