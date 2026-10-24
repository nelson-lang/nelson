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
let abs: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  abs = br.getDefinitions()['abs']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const createSvgElement = vi.fn((tag: string, attrs: any, text?: string) => {
    const el = document.createElement(tag === 'text' ? 'span' : 'div');
    if (attrs) Object.keys(attrs).forEach((k) => el.setAttribute(k, String(attrs[k])));
    if (text) el.textContent = text;
    return el;
  });
  const renderTeXMath = vi.fn();
  return { createSvgElement, renderTeXMath } as any;
}
//=============================================================================
describe('abs block renderer', () => {
  test('appends rect and math group and calls renderTeXMath', () => {
    const helpers = makeHelpers();
    const g = document.createElement('div');
    const block: any = { group: g, width: 50, height: 20 };
    abs(block, helpers);
    expect(helpers.createSvgElement).toHaveBeenCalled();
    expect(helpers.renderTeXMath).toHaveBeenCalled();
    expect(g.childElementCount).toBeGreaterThanOrEqual(2);
  });
});
//=============================================================================
