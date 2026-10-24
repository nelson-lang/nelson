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
import fs from 'fs';
import path from 'path';
//=============================================================================
test('loads inspector if present', async () => {
  const p = path.resolve(process.cwd(), 'modules/nflow_gui/src/ts/render/inspector.ts');
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import(String('./inspector'));
  expect(mod).toBeDefined();
});
//=============================================================================
