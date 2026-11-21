//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
let sumRender: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  sumRender = br.getDefinitions()['sum']?.render as any;
});
//=============================================================================
function makeHelpersForSum() {
  const createSvgElement = vi.fn((tag: string, attrs: any, content?: string) => {
    if (typeof document !== 'undefined' && typeof document.createElementNS === 'function') {
      const ns = 'http://www.w3.org/2000/svg';
      const el = document.createElementNS(ns, tag);
      if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
      if (content) el.textContent = content;
      return el;
    }
    return document.createElement('div');
  });
  return { createSvgElement } as any;
}
//=============================================================================
describe('sum block renderer', () => {
  test('renders a sign for each input', () => {
    const helpers = makeHelpersForSum();
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    svg.appendChild(g);
    const block: any = { group: g, width: 20, height: 20, params: { Inputs: '+-+' } };
    sumRender(block, helpers);
    const signs = Array.from(g.querySelectorAll('.sum-sign')).map((el) => el.textContent);
    expect(signs).toEqual(['+', '-', '+']);
  });
});
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
