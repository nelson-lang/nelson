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
test('loads blocks.ts if present or registry contains source blocks', async () => {
  const defs = builtinRegistry.getDefinitions();
  const hasAny =
    defs['constant'] || defs['step'] || defs['ramp'] || defs['sine'] || defs['fileSource'];
  expect(hasAny).toBeDefined();
  expect(defs['clock']).toBeDefined();
  expect(defs['clock'].defaultParams).toMatchObject({
    DisplayTime: false,
    Decimation: 10,
  });

  const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  defs['clock'].render(
    { id: 'clock1', type: 'clock', group, width: 80, height: 80, params: { DisplayTime: true } },
    {
      createSvgElement: (tagName: string, attrs?: Record<string, unknown>, content?: string) => {
        const el = document.createElementNS('http://www.w3.org/2000/svg', tagName);
        Object.entries(attrs || {}).forEach(([k, v]) => el.setAttribute(k, String(v)));
        if (content) el.textContent = content;
        return el;
      },
    }
  );
  expect(group.querySelector('image')).toBeTruthy();
  expect(group.querySelector('text.clock-time-display')?.textContent).toBe('0');

  const labelGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  defs['clock'].render(
    { id: 'clock2', type: 'clock', group: labelGroup, width: 80, height: 80, params: {} },
    {
      createSvgElement: (tagName: string, attrs?: Record<string, unknown>, content?: string) => {
        const el = document.createElementNS('http://www.w3.org/2000/svg', tagName);
        Object.entries(attrs || {}).forEach(([k, v]) => el.setAttribute(k, String(v)));
        if (content) el.textContent = content;
        return el;
      },
    }
  );
  expect(labelGroup.querySelector('text.clock-time-display')?.textContent).toBe('Clock');
});
//=============================================================================
