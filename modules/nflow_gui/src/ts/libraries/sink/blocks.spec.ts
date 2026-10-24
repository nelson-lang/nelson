//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
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
test('loads blocks.ts if present or registry contains sink blocks', async () => {
  const defs = builtinRegistry.getDefinitions();
  expect(defs).toBeDefined();
  const hasAnySink =
    defs['display'] ||
    defs['scope'] ||
    defs['xyScope'] ||
    defs['xyzScope'] ||
    defs['fileSink'] ||
    defs['labelSink'];
  expect(hasAnySink).toBeDefined();
  expect(defs['terminator']).toBeDefined();

  const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  defs['terminator'].render(
    { id: 'terminator', type: 'terminator', group, width: 40, height: 40, params: {} },
    svgHelpers()
  );
  expect(group.querySelector('image')).toBeTruthy();
});
//=============================================================================
