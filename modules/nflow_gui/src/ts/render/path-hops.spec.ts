//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('wire-path hops', () => {
  let render: any;

  beforeAll(async () => {
    render = await import('../../../dist/render/wire/wire-paths.js');
  });

  test('buildPathWithHops respects hopClaims Map and Set-style claims', () => {
    const segH: any = {
      orientation: 'H',
      a: { x: 0, y: 0 },
      b: { x: 40, y: 0 },
      minX: 0,
      maxX: 40,
      y: 0,
      isStub: false,
      owner: 'a',
    };
    const segV: any = {
      orientation: 'V',
      a: { x: 10, y: -10 },
      b: { x: 10, y: 10 },
      minY: -10,
      maxY: 10,
      x: 10,
      isStub: false,
      owner: 'b',
    };

    const claimsMap = new Map();
    const d1 = render.buildPathWithHops([segH], [segV], claimsMap);
    expect(typeof d1).toBe('string');
    const d2 = render.buildPathWithHops([segH], [segV], claimsMap);
    expect(typeof d2).toBe('string');

    const claimsSet = new Set();
    const d3 = render.buildPathWithHops([segH], [segV], claimsSet);
    expect(d3.includes('a ')).toBe(true);
    const key = '10,0|V';
    claimsSet.add(key);
    const d4 = render.buildPathWithHops([segH], [segV], claimsSet);
    expect(d4.includes('a ')).toBe(false);
  });

  test('type contracts', () => {
    const p = render.buildFallbackPath({ x: 0, y: 0 }, { x: 10, y: 0 });
    expect(Array.isArray(p)).toBe(true);
    p.forEach((pt: any) => {
      expect(typeof pt.x).toBe('number');
      expect(typeof pt.y).toBe('number');
    });
    const d = render.buildPathWithHops([], []);
    expect(typeof d).toBe('string');
  });
});
//=============================================================================
