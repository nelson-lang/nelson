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
import switchRenderer from './switch';
//=============================================================================
function makeHelpers() {
  const createSvgElement = vi.fn((tag: string, attrs: any) => document.createElement('div'));
  const renderTeXMath = vi.fn();
  return { createSvgElement, renderTeXMath } as any;
}
//=============================================================================
describe('switch renderer', () => {
  test('renders and calls renderTeXMath with condition latex', () => {
    const helpers = makeHelpers();
    const g = document.createElement('div');
    const block: any = {
      group: g,
      width: 80,
      height: 40,
      params: { Criteria: FieldNames.kNe, Threshold: 5 },
    };
    switchRenderer(block, helpers);
    expect(helpers.createSvgElement).toHaveBeenCalled();
    expect(helpers.renderTeXMath).toHaveBeenCalled();
    const arg = (helpers.renderTeXMath as vi.Mock).mock.calls[0][1] as string;
    expect(arg).toContain('\\ne');
    expect(arg).toContain('5');
  });
});
//=============================================================================
