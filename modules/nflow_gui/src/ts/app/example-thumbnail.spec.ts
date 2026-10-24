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
import { renderThumbnailSvg } from './example-thumbnail.js';
//=============================================================================
describe('renderThumbnailSvg', () => {
  it('returns null for empty or invalid input', () => {
    expect(renderThumbnailSvg(null)).toBeNull();
    expect(renderThumbnailSvg({})).toBeNull();
    expect(renderThumbnailSvg({ blocks: [] })).toBeNull();
    expect(renderThumbnailSvg({ blocks: [{ type: 'sum' }] })).toBeNull();
  });

  it('renders one rect per positioned block and fits the viewBox to content', () => {
    const svg = renderThumbnailSvg({
      blocks: [
        { id: 'a', type: 'step', x: 0, y: 0, width: 80, height: 60 },
        { id: 'b', type: 'scope', x: 200, y: 100, width: 80, height: 60 },
      ],
    });
    expect(svg).toBeTypeOf('string');
    expect(svg).toContain('<svg');
    // two blocks => two rects.
    expect((svg as string).match(/<rect /g)?.length).toBe(2);
    // viewBox starts padded before the top-left block (0,0) with PAD=24.
    expect(svg).toContain('viewBox="-24 -24 ');
  });

  it('draws a connection polyline from saved routing points', () => {
    const svg = renderThumbnailSvg({
      blocks: [
        { id: 'a', type: 'step', x: 0, y: 0, width: 80, height: 60 },
        { id: 'b', type: 'scope', x: 200, y: 0, width: 80, height: 60 },
      ],
      connections: [
        {
          from: 'a',
          to: 'b',
          points: [
            { x: 80, y: 30 },
            { x: 200, y: 30 },
          ],
        },
      ],
    });
    expect(svg).toContain('<polyline');
    expect(svg).toContain('80,30 200,30');
  });

  it('falls back to a straight link between block centers when points are missing', () => {
    const svg = renderThumbnailSvg({
      blocks: [
        { id: 'a', type: 'step', x: 0, y: 0, width: 80, height: 60 },
        { id: 'b', type: 'scope', x: 200, y: 0, width: 80, height: 60 },
      ],
      connections: [{ from: 'a', to: 'b' }],
    });
    expect(svg).toContain('<polyline');
    // source right-center (80,30) -> target left-center (200,30).
    expect(svg).toContain('80,30 200,30');
  });

  it('escapes nothing dangerous and produces well-formed markup', () => {
    const svg = renderThumbnailSvg({
      blocks: [{ id: 'a', type: 'weird"type', x: 0, y: 0 }],
    }) as string;
    expect(svg).not.toContain('weird"type');
    expect(svg.startsWith('<svg')).toBe(true);
    expect(svg.endsWith('</svg>')).toBe(true);
  });
});
//=============================================================================
