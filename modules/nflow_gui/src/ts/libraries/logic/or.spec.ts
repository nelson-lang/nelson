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
let or: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  or = br.getDefinitions()['or']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const svgRect = vi.fn(() => document.createElement('div'));
  const createSvgElement = vi.fn((tag: string, attrs: any) => document.createElement('div'));
  const renderTeXMath = vi.fn();
  return { svgRect, createSvgElement, renderTeXMath } as any;
}
//=============================================================================
describe('or block renderer', () => {
  test('appends block body and calls renderTeXMath', () => {
    const helpers = makeHelpers();
    const g = document.createElement('div');
    const block: any = { group: g, width: 10, height: 5 };
    or(block, helpers);
    expect(helpers.svgRect).toHaveBeenCalled();
    expect(helpers.createSvgElement).toHaveBeenCalled();
    const gCalls = (helpers.createSvgElement as vi.Mock).mock.calls.filter(
      (c: any[]) => c[0] === 'g'
    );
    expect(gCalls.length).toBeGreaterThan(0);
    const attrs = gCalls[0][1] || {};
    const cls = String((attrs as any).class ?? (attrs as any)['class'] ?? '');
    expect(cls).toMatch(/formula$/);
    expect(helpers.renderTeXMath).toHaveBeenCalled();
  });
});
//=============================================================================
