//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, it } from 'vitest';
import { FigureImagePayloadCache } from '../../src/panels/figureImagePayloadCache';
import type { DrawPrimitive, FigureDisplayListResult } from '../../src/rpc/types';
//=============================================================================
const frame = (primitives: DrawPrimitive[]): FigureDisplayListResult => ({
  handle: 1,
  width: 4,
  height: 4,
  primitives,
});
//=============================================================================
it('retains an image from a skipped frame, including raw dimensions', () => {
  const cache = new FigureImagePayloadCache();
  const image: DrawPrimitive = { kind: 'image', imageId: 'fixed', x: 0, y: 0, w: 4, h: 4 };
  const rgbaBytes = new Uint8Array([255, 0, 0, 255]).buffer;
  cache.retain([{ ...image, rgbaBytes, imageWidth: 1, imageHeight: 1 }]);
  expect(cache.hydrate(frame([image])).primitives[0]).toEqual({
    ...image,
    rgbaBytes,
    imageWidth: 1,
    imageHeight: 1,
  });
  cache.clear();
  expect(cache.hydrate(frame([image])).primitives[0]).toEqual(image);
});
//=============================================================================
it('bounds retained animation data and keeps recently referenced sprites', () => {
  const cache = new FigureImagePayloadCache();
  const image = (imageId: string): DrawPrimitive => ({
    kind: 'image',
    imageId,
    x: 0,
    y: 0,
    w: 4,
    h: 4,
  });
  for (let i = 0; i < 64; i++) cache.retain([{ ...image(String(i)), image: 'data:png' }]);
  cache.hydrate(frame([image('0')]));
  cache.retain([{ ...image('64'), image: 'data:png' }]);
  expect(cache.hydrate(frame([image('0')])).primitives[0]).toHaveProperty('image', 'data:png');
  expect(cache.hydrate(frame([image('1')])).primitives[0]).not.toHaveProperty('image');
});
//=============================================================================
