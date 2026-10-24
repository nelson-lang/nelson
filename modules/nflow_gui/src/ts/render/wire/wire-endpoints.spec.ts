//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect } from 'vitest';
import { desnapEndpoints } from './wire-endpoints';
//=============================================================================
describe('desnapEndpoints', () => {
  it('turns a routed straight line between aligned off-grid ports into one exact line', () => {
    const out = desnapEndpoints(
      [
        { x: 300, y: 250 },
        { x: 560, y: 250 },
      ],
      { x: 300, y: 245 },
      'right',
      { x: 560, y: 245 },
      'left'
    );
    expect(out).toEqual([
      { x: 300, y: 245 },
      { x: 560, y: 245 },
    ]);
  });

  it('gives two ports a few pixels apart a single mid-span jog', () => {
    const out = desnapEndpoints(
      [
        { x: 300, y: 250 },
        { x: 560, y: 250 },
      ],
      { x: 300, y: 250 },
      'right',
      { x: 560, y: 245 },
      'left'
    );
    expect(out).toEqual([
      { x: 300, y: 250 },
      { x: 430, y: 250 },
      { x: 430, y: 245 },
      { x: 560, y: 245 },
    ]);
  });

  it('moves the end runs of a longer path onto the exact ports and stays orthogonal', () => {
    const out = desnapEndpoints(
      [
        { x: 220, y: 160 },
        { x: 260, y: 160 },
        { x: 260, y: 190 },
        { x: 300, y: 190 },
      ],
      { x: 220, y: 163 },
      'right',
      { x: 300, y: 187 },
      'left'
    );
    expect(out).toEqual([
      { x: 220, y: 163 },
      { x: 260, y: 163 },
      { x: 260, y: 187 },
      { x: 300, y: 187 },
    ]);
  });

  it('handles a vertical port on an L path', () => {
    const out = desnapEndpoints(
      [
        { x: 100, y: 200 },
        { x: 180, y: 200 },
        { x: 180, y: 300 },
      ],
      { x: 100, y: 203 },
      'right',
      { x: 183, y: 300 },
      'top'
    );
    expect(out).toEqual([
      { x: 100, y: 203 },
      { x: 183, y: 203 },
      { x: 183, y: 300 },
    ]);
  });

  it('leaves a path alone when its ends are not near the ports', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 100, y: 0 },
    ];
    expect(desnapEndpoints(pts, { x: 0, y: 40 }, 'right', { x: 100, y: 0 }, 'left')).toBe(pts);
  });
});
//=============================================================================
