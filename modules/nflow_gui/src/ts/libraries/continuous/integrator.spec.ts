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
let integrator: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  integrator = br.getDefinitions()['integrator']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const createSvgElement = vi.fn(() => document.createElement('div'));
  const renderTeXMath = vi.fn();
  return { createSvgElement, renderTeXMath } as any;
}
//=============================================================================
test('integrator renderer calls helpers', () => {
  const h = makeHelpers();
  const g = document.createElement('div');
  const b: any = { group: g, width: 22, height: 12 };
  integrator(b, h);
  expect(h.createSvgElement).toHaveBeenCalled();
  expect(h.renderTeXMath).toHaveBeenCalled();
});
//=============================================================================
test('loads integrator.ts if present', async () => {
  const p = path.resolve(
    process.cwd(),
    'modules/nflow_gui/src/ts',
    'libraries/continuous/integrator.ts'
  );
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import(String('./integrator'));
  expect(mod).toBeDefined();
});
//=============================================================================
