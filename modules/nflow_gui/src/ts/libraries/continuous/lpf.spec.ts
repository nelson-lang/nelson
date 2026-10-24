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
let lpf: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  lpf = br.getDefinitions()['lpf']?.render as any;
});
//=============================================================================
function makeHelpers() {
  const renderSourcePlot = vi.fn(() => {});
  return { renderSourcePlot } as any;
}
//=============================================================================
test('lpf renderer calls helpers or appends a graphic element', () => {
  const h = makeHelpers();
  const g = document.createElement('div');
  const b: any = { group: g, width: 22, height: 12 };
  lpf(b, h);
  const called = !!(
    h.renderSourcePlot &&
    (h.renderSourcePlot as any).mock &&
    (h.renderSourcePlot as any).mock.calls.length > 0
  );
  const hasImage = !!g.querySelector('image') || !!g.querySelector('img');
  const hasPath = !!g.querySelector('path');
  expect(called || hasImage || hasPath).toBeTruthy();
});
//=============================================================================
