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
vi.mock('../icon-cache.js', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../icon-cache.js')>()),
  builtinSpritePending: () => false,
  getIconSync: () => null,
  getIcon: async () => ({ colors: null }),
}));
import { blockDefinitions as lookup } from './blocks';
import { blockDefinitions as utility } from '../utility/blocks';
const ns = 'http://www.w3.org/2000/svg';
const helpers = {
  createSvgElement: (tag: string, attrs: Record<string, unknown>, text?: string) => {
    const el = document.createElementNS(ns, tag);
    Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, String(value)));
    if (text) el.textContent = text;
    return el;
  },
};
test.each([
  ['lookupND', lookup.lookupND, 'lookup'],
  ['directLookup', lookup.directLookup, 'lookup'],
  ['multiportSwitch', utility.multiportSwitch, 'utility'],
])('%s resolves its library image', (_type, def: any, dir) => {
  const group = document.createElementNS(ns, 'g');
  def.render({ group, width: 90, height: 80, params: {} }, helpers);
  const image = group.querySelector('image');
  expect(image).not.toBeNull();
  expect(image?.getAttribute('href')).toContain('/libraries/' + dir + '/exports/');
});
