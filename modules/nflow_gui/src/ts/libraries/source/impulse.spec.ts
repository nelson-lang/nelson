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
let impulse: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  impulse = br.getDefinitions()['impulse']?.render as any;
});
//=============================================================================
test('impulse renderer calls renderSourcePlot or appends a graphic element', () => {
  const renderSourcePlot = vi.fn();
  const helpers: any = { renderSourcePlot };
  const g = document.createElement('div');
  const b: any = { group: g, width: 60, height: 30 };
  impulse(b, helpers);
  const called = !!(
    renderSourcePlot &&
    (renderSourcePlot as any).mock &&
    (renderSourcePlot as any).mock.calls.length > 0
  );
  const hasImage = !!g.querySelector('image') || !!g.querySelector('img');
  const hasPath = !!g.querySelector('path');
  expect(called || hasImage || hasPath).toBeTruthy();
});
//=============================================================================
