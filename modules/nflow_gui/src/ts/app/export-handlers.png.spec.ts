//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment jsdom */
import { dataUrlToBlob } from './export-handlers';
//=============================================================================
describe('dataUrlToBlob (PNG export helper)', () => {
  test('decodes a base64 PNG data URL to an image/png Blob', () => {
    const b = dataUrlToBlob(
      'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR4nGNgYAAAAAMAASsJTYQAAAAASUVORK5CYII='
    );
    expect(b).toBeTruthy();
    expect(b!.type).toBe('image/png');
    expect(b!.size).toBeGreaterThan(0);
  });

  test('returns null for a string that is not a data URL', () => {
    expect(dataUrlToBlob('not a data url')).toBeNull();
  });
});
//=============================================================================
