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
let tf: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  tf = br.getDefinitions()['tf']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const createSvgElement = vi.fn(() => document.createElement('div'));
  const renderTeXMath = vi.fn();
  const buildTransferTeX = vi.fn(() => '\frac{1}{s}');
  return { createSvgElement, renderTeXMath, buildTransferTeX } as any;
}
//=============================================================================
test('tf renderer calls helpers', () => {
  const h = makeHelpers();
  const g = document.createElement('div');
  const b: any = {
    group: g,
    width: 20,
    height: 10,
    params: { Numerator: [1], Denominator: [1, 1] },
  };
  tf(b, h);
  expect(h.createSvgElement).toHaveBeenCalled();
  expect(h.buildTransferTeX).toHaveBeenCalled();
  expect(h.renderTeXMath).toHaveBeenCalled();
});
//=============================================================================
