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
let chirp: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  chirp = br.getDefinitions()['chirp']?.render as any;
});
//=============================================================================
test('chirp renderer renders image pointing to chirp.svg', () => {
  const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const b: any = { group: g, width: 100, height: 40, id: 'b1', params: {} };
  chirp(b, {} as any);
  const img = g.querySelector('image');
  expect(img).not.toBeNull();
  const href =
    img?.getAttribute('href') || img?.getAttributeNS('http://www.w3.org/1999/xlink', 'href');
  expect(href).toEqual(expect.stringContaining('chirp.svg'));
});
//=============================================================================
