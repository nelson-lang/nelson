//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { vi } from 'vitest';
import fs from 'fs';
import path from 'path';
//=============================================================================
let labelSource: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  labelSource = br.getDefinitions()['labelSource']?.render as any;
});
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeHelpers = (overrides: any = {}) => ({
  createSvgElement: (tag: string, attrs?: any) => {
    const el = document.createElementNS(ns, tag) as any;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    return el;
  },
  renderLabelNode: vi.fn(),
  ...overrides,
});
//=============================================================================
const makeBlock = (overrides: any = {}) => ({
  id: 'lsrc1',
  group: document.createElementNS(ns, 'g') as any,
  width: 50,
  height: 40,
  params: { name: 'signal' },
  ...overrides,
});
//=============================================================================
describe('labelSource renderer', () => {
  test('calls renderLabelNode when present', () => {
    const helpers = makeHelpers();
    const block = makeBlock();

    labelSource(block as any, helpers as any);

    expect(helpers.renderLabelNode).toHaveBeenCalledTimes(1);
  });

  test('always appends a label-node line', () => {
    const block = makeBlock();

    labelSource(block as any, makeHelpers() as any);

    expect(block.group.querySelector('line.label-node')).not.toBeNull();
  });

  test('does not call renderLabelNode when helper absent', () => {
    const helpers = makeHelpers({ renderLabelNode: undefined });
    const block = makeBlock();
    expect(() => labelSource(block as any, helpers as any)).not.toThrow();
  });
});
//=============================================================================
