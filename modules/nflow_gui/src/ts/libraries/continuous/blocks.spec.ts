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
import builtinRegistry from '../../app/blocks-registry.js';
//=============================================================================
test('loads blocks.ts if present or registry contains continuous blocks', async () => {
  const p = path.resolve(
    process.cwd(),
    'modules/nflow_gui/src/ts',
    'libraries/continuous/blocks.ts'
  );
  if (fs.existsSync(p)) {
    const mod = await import(String('./blocks'));
    expect(mod).toBeDefined();
    expect(mod.blockDefinitions || mod.blockRegistry || mod.renderers).toBeDefined();
    return;
  }

  const defs = builtinRegistry.getDefinitions();
  const hasAny = defs['integrator'] || defs['tf'] || defs['pid'];
  expect(hasAny).toBeDefined();
});
//=============================================================================
