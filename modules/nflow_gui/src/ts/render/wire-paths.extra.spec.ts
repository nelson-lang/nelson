//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for wire-paths
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/wire/wire-paths.js');
});
//=============================================================================
test('buildSegments creates vertical and horizontal segments', () => {
  const pts = [
    { x: 0, y: 0 },
    { x: 0, y: 10 },
    { x: 5, y: 10 },
  ];
  const segs = mod.buildSegments(pts, 'owner');
  expect(segs.length).toBe(2);
  expect(segs[0].orientation).toBe('V');
  expect(segs[1].orientation).toBe('H');
});
//=============================================================================
test('buildPathWithHops emits arc commands for crossings', () => {
  const segH = {
    orientation: 'H',
    a: { x: 0, y: 10 },
    b: { x: 100, y: 10 },
    minX: 0,
    maxX: 100,
    y: 10,
    isStub: false,
  };
  const segV = {
    orientation: 'V',
    a: { x: 50, y: 0 },
    b: { x: 50, y: 20 },
    minY: 0,
    maxY: 20,
    x: 50,
    isStub: false,
  };
  const d = mod.buildPathWithHops([segH], [segV]);
  expect(typeof d).toBe('string');
  expect(d.includes('a ')).toBe(true);
});
//=============================================================================
test('normalizeWirePoints filters invalid and dedupes', () => {
  const raw = [{ x: 0, y: 0 }, [0, 0], { x: '10', y: '5' }, { x: NaN, y: 1 }, [20, 20], [20, 20]];
  const out = mod.normalizeWirePoints(raw);
  expect(out).toEqual([
    { x: 0, y: 0 },
    { x: 10, y: 5 },
    { x: 20, y: 20 },
  ]);
});
//=============================================================================
test('collectSharedSourceSplitPoints finds split point', () => {
  const c1 = {
    from: 's',
    fromIndex: 0,
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 20, y: 0 },
    ],
  };
  const c2 = {
    from: 's',
    fromIndex: 0,
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 10, y: 10 },
    ],
  };
  const res = mod.collectSharedSourceSplitPoints([c1, c2], (c: any) => c.points);
  expect(Array.isArray(res)).toBe(true);
  expect(res.some((p: any) => p.x === 10 && p.y === 0)).toBe(true);
});
//=============================================================================
