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
let mult: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  mult = br.getDefinitions()['mult']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const createSvgElement = vi.fn((tag: string, attrs: any) => {
    const el = document.createElement('div');
    if (attrs) Object.keys(attrs).forEach((k) => el.setAttribute(k, String(attrs[k])));
    return el;
  });
  return { createSvgElement } as any;
}
//=============================================================================
describe('mult block renderer', () => {
  test('creates circle and two lines', () => {
    const helpers = makeHelpers();
    const g = document.createElement('div');
    const block: any = { group: g };
    mult(block, helpers);
    expect(helpers.createSvgElement).toHaveBeenCalledTimes(3);
    expect(g.childElementCount).toBe(3);
  });
});
//=============================================================================
