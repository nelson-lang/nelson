//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Rounded bends (opt-in path option): quarter arcs at orientation changes,
// disabled near hop arcs and on short segments; default output unchanged.
//=============================================================================
import { buildSegments, buildPathWithHops } from './wire/wire-paths.js';
//=============================================================================
const L_SHAPE = [
  { x: 0, y: 0 },
  { x: 100, y: 0 },
  { x: 100, y: 100 },
];
//=============================================================================
describe('rounded wire bends', () => {
  it('default output is unchanged (no arcs without the option)', () => {
    const segs = buildSegments(L_SHAPE, { from: 'a', to: 'b' });
    const d = buildPathWithHops(segs, []);
    expect(d).not.toContain('a 4 4');
    expect(d).toBe('M 0 0 L 100 0 L 100 100');
  });

  it('emits a quarter arc at the bend when enabled', () => {
    const segs = buildSegments(L_SHAPE, { from: 'a', to: 'b' });
    const d = buildPathWithHops(segs, [], null, { roundedCorners: true });
    // Right then down gives a clockwise quarter arc.
    expect(d).toBe('M 0 0 L 96 0 a 4 4 0 0 1 4 4 L 100 100');
  });

  it('uses the opposite sweep for a counter-clockwise bend', () => {
    const up = [
      { x: 0, y: 0 },
      { x: 100, y: 0 },
      { x: 100, y: -100 },
    ];
    const segs = buildSegments(up, { from: 'a', to: 'b' });
    const d = buildPathWithHops(segs, [], null, { roundedCorners: true });
    expect(d).toBe('M 0 0 L 96 0 a 4 4 0 0 0 4 -4 L 100 -100');
  });

  it('skips rounding when a segment is too short', () => {
    const tiny = [
      { x: 0, y: 0 },
      { x: 6, y: 0 },
      { x: 6, y: 100 },
    ];
    const segs = buildSegments(tiny, { from: 'a', to: 'b' });
    const d = buildPathWithHops(segs, [], null, { roundedCorners: true });
    expect(d).not.toContain('a 4 4');
  });

  it('skips rounding near a hop arc but keeps the hop', () => {
    // Crossing wire near the corner (x=90 crossing on the H run, corner x=100).
    const other = buildSegments(
      [
        { x: 90, y: -50 },
        { x: 90, y: 50 },
      ],
      { from: 'z', to: 'w' }
    );
    const segs = buildSegments(L_SHAPE, { from: 'a', to: 'b' });
    const d = buildPathWithHops(segs, other, null, { roundedCorners: true });
    // Hop drawn (a:0->b:0 < z:0->w:0): full-diameter horizontal arc (8 0).
    expect(d).toContain('a 4 4 0 0 1 8 0');
    // Corner left sharp: no quarter arc (4 4 / 4 -4 deltas), plain L corner.
    expect(d).not.toContain('a 4 4 0 0 1 4 4');
    expect(d).not.toContain('a 4 4 0 0 0 4 -4');
    expect(d).toContain('L 100 0 L 100 100');
  });

  it('keeps the terminal stub straight for the arrow marker', () => {
    // Bend right before the final 10px stub: rounding uses r=4, leaving a
    // straight tail of at least 6px for the marker.
    const path = [
      { x: 0, y: 0 },
      { x: 100, y: 0 },
      { x: 100, y: 50 },
      { x: 110, y: 50 },
    ];
    const segs = buildSegments(path, { from: 'a', to: 'b' });
    const d = buildPathWithHops(segs, [], null, { roundedCorners: true });
    // The path still ends with a straight L to the endpoint.
    expect(d.trim().endsWith('L 110 50')).toBe(true);
  });
});
//=============================================================================
