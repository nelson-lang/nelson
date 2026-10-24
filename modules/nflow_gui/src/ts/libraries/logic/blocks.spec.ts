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
import builtinRegistry from '../../app/blocks-registry.js';
//=============================================================================
const renderers: Record<string, any> = Object.fromEntries(
  Object.entries(builtinRegistry.getDefinitions()).map(([k, v]) => [k, (v as any)?.render])
);
//=============================================================================
function svgHelpers() {
  return {
    createSvgElement: (tagName: string, attrs?: Record<string, unknown>, content?: string) => {
      const el = document.createElementNS('http://www.w3.org/2000/svg', tagName);
      Object.entries(attrs || {}).forEach(([k, v]) => el.setAttribute(k, String(v)));
      if (content) el.textContent = content;
      return el;
    },
  };
}
//=============================================================================
describe('logic/blocks', () => {
  test('exports expected renderer keys', () => {
    expect(renderers).toBeDefined();
    const keys = Object.keys(renderers);
    expect(keys).toEqual(
      expect.arrayContaining([
        'and',
        'or',
        'xor',
        'not',
        'compareToConstant',
        'compareToZero',
        'relationalOperator',
      ])
    );
    ['and', 'or', 'xor', 'not', 'compareToConstant', 'compareToZero', 'relationalOperator'].forEach(
      (k) => {
        // @ts-ignore
        expect(typeof renderers[k]).toBe('function');
      }
    );
  });

  test('comparison blocks display their configured operator', () => {
    const defs = builtinRegistry.getDefinitions();
    const cases = [
      ['compareToConstant', 'lt', '<'],
      ['compareToZero', 'ne', '!='],
      ['relationalOperator', 'le', '<='],
    ];

    for (const [type, operator, expected] of cases) {
      const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      defs[type].render(
        {
          id: type,
          type,
          group,
          width: 100,
          height: 80,
          params: { Operator: operator, relop: operator },
        },
        svgHelpers()
      );
      expect(group.querySelector('text.logic-expression-display')?.textContent).toContain(expected);
    }
  });
});
//=============================================================================
test('loads blocks.ts if present or registry contains logic blocks', async () => {
  const defs = builtinRegistry.getDefinitions();
  const hasAny = defs['and'] || defs['or'] || defs['xor'] || defs['not'];
  expect(hasAny).toBeDefined();
});
//=============================================================================
