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
let min: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  min = br.getDefinitions()['min']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const createSvgElement = vi.fn((tag: string, attrs: any) => document.createElement('div'));
  const renderTeXMath = vi.fn();
  return { createSvgElement, renderTeXMath } as any;
}
//=============================================================================
describe('min block renderer', () => {
  test('appends block body and calls renderTeXMath', () => {
    const helpers = makeHelpers();
    const g = document.createElement('div');
    const block: any = { group: g, width: 10, height: 5 };
    min(block, helpers);
    expect(helpers.createSvgElement).toHaveBeenCalled();
    expect(helpers.renderTeXMath).toHaveBeenCalled();
  });
});
//=============================================================================
