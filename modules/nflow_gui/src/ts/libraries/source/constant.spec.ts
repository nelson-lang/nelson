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
let constant: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  constant = br.getDefinitions()['constant']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const svgRect = vi.fn(() => document.createElement('div'));
  const createSvgElement = vi.fn(() => document.createElement('div'));
  const renderTeXMath = vi.fn();
  return { svgRect, createSvgElement, renderTeXMath } as any;
}
//=============================================================================
test('constant renderer uses helpers and renders value', () => {
  const h = makeHelpers();
  const g = document.createElement('div');
  const b: any = { group: g, width: 40, height: 16, params: { value: 3.14 } };
  constant(b, h);
  expect(h.svgRect).toHaveBeenCalled();
  expect(h.createSvgElement).toHaveBeenCalled();
  expect(h.renderTeXMath).toHaveBeenCalled();
});
//=============================================================================
