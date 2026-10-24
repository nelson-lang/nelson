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
let userFunc: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  userFunc = br.getDefinitions()['expression']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const createSvgElement = vi.fn((tag: string, attrs: any) => document.createElement('div'));
  const renderTeXMath = vi.fn();
  return { createSvgElement, renderTeXMath } as any;
}
//=============================================================================
describe('userFunc block renderer', () => {
  test('creates body and calls renderTeXMath with provided expr', () => {
    const helpers = makeHelpers();
    const g = document.createElement('div');
    const block: any = { group: g, width: 40, height: 16, params: { expr: 'a + b' } };
    userFunc(block, helpers);
    expect(helpers.createSvgElement).toHaveBeenCalled();
    expect(helpers.renderTeXMath).toHaveBeenCalled();
    expect(g.childElementCount).toBeGreaterThanOrEqual(2);
  });
});
//=============================================================================
