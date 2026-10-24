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
import builtinRegistry from '../../app/blocks-registry.js';
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
test('loads blocks.ts if present or registry contains discrete blocks', async () => {
  const defs = builtinRegistry.getDefinitions();
  const hasAny = defs['zoh'] || defs['foh'] || defs['dtf'];
  expect(hasAny).toBeDefined();
  expect(defs['unitDelay']).toBeDefined();
  expect(defs['difference']).toBeDefined();

  for (const type of ['unitDelay', 'difference']) {
    const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    defs[type].render({ id: type, type, group, width: 80, height: 80, params: {} }, svgHelpers());
    expect(group.querySelector('image')).toBeTruthy();
  }
});
//=============================================================================
