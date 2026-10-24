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
let delay: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  delay = br.getDefinitions()['delay']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const svgRect = vi.fn((x: number, y: number, w: number, h: number, cls: string) =>
    document.createElement('div')
  );
  const createSvgElement = vi.fn((tag: string, attrs: any) => document.createElement('div'));
  const renderTeXMath = vi.fn();
  return { svgRect, createSvgElement, renderTeXMath } as any;
}
//=============================================================================
test('delay renderer calls helpers', () => {
  const h = makeHelpers();
  const g = document.createElement('div');
  const b: any = { group: g, width: 30, height: 12 };
  delay(b, h);
  expect(h.svgRect).toHaveBeenCalled();
  expect(h.createSvgElement).toHaveBeenCalled();
  expect(h.renderTeXMath).toHaveBeenCalled();
});
//=============================================================================
