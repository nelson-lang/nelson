//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import builtinRegistry from '../../app/blocks-registry.js';
//=============================================================================
test('loads blocks.ts if present or registry contains nonlinear blocks', async () => {
  const defs = builtinRegistry.getDefinitions();
  const hasAny = defs['saturation'] || defs['rate'] || defs['backlash'];
  expect(hasAny).toBeDefined();
  expect(defs['deadZone']).toBeDefined();
  expect(defs['quantizer']).toBeDefined();
});
//=============================================================================
