//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/geometry.js');
});
//=============================================================================
test('rotatePoint returns same for 0 and rotates for 90/180/270', () => {
  const block = { x: 0, y: 0, width: 10, height: 20, rotation: 0 };
  const p = { x: 1, y: 2 };
  expect(mod.rotatePoint(p, block)).toEqual(p);
  block.rotation = 90;
  const r90 = mod.rotatePoint({ x: block.x + block.width, y: block.y + block.height / 2 }, block);
  expect(typeof r90.x).toBe('number');
  block.rotation = 180;
  const r180 = mod.rotatePoint({ x: block.x + 1, y: block.y + 2 }, block);
  expect(typeof r180.x).toBe('number');
  block.rotation = 270;
  const r270 = mod.rotatePoint({ x: block.x + 1, y: block.y + 2 }, block);
  expect(typeof r270.x).toBe('number');
});
//=============================================================================
test('blockPortAnchor prefers the centre-line port, then the first input', () => {
  const selector = {
    x: 0,
    y: 0,
    width: 85,
    height: 70,
    ports: [
      { x: 85, y: 23, type: 'out', index: 0 },
      { x: 0, y: 35, type: 'in', index: 0 },
    ],
  };
  expect(mod.blockPortAnchor(selector)).toEqual({ x: 0, y: 35 });
  const twoInputs = {
    x: 0,
    y: 0,
    width: 100,
    height: 60,
    ports: [
      { x: 0, y: 18, type: 'in', index: 0 },
      { x: 0, y: 42, type: 'in', index: 1 },
      { x: 100, y: 25, type: 'out', index: 0 },
    ],
  };
  expect(mod.blockPortAnchor(twoInputs)).toEqual({ x: 0, y: 18 });
  expect(mod.blockPortAnchor({ x: 0, y: 0, width: 50, height: 50, ports: [] })).toBeNull();
});
//=============================================================================
test('blockPortAnchor follows the block rotation', () => {
  const block = {
    x: 100,
    y: 100,
    width: 80,
    height: 60,
    rotation: 180,
    ports: [{ x: 0, y: 30, type: 'in', index: 0 }],
  };
  expect(mod.blockPortAnchor(block)).toEqual({ x: 80, y: 30 });
});
//=============================================================================
test('alignBlockToGrid puts the anchor port on the grid, not the corner', () => {
  // A 70 px tall block whose centred port would sit 5 px off any grid line
  // when the corner is snapped: y=205 keeps the port at 240.
  expect(mod.alignBlockToGrid({ x: 560, y: 205 }, { x: 0, y: 35 })).toEqual({ x: 560, y: 205 });
  expect(mod.alignBlockToGrid({ x: 563, y: 201 }, { x: 0, y: 35 })).toEqual({ x: 560, y: 205 });
  // Without an anchor the corner snaps as before.
  expect(mod.alignBlockToGrid({ x: 563, y: 201 }, null)).toEqual({ x: 560, y: 200 });
});
//=============================================================================
test('getRotatedBounds swaps dimensions for 90/270', () => {
  const b: any = { x: 10, y: 20, width: 30, height: 40, rotation: 0 };
  const noRot = mod.getRotatedBounds(b);
  expect(noRot.right - noRot.left).toBeCloseTo(30);
  b.rotation = 90;
  const rot = mod.getRotatedBounds(b);
  expect(rot.right - rot.left).toBeCloseTo(40);
});
//=============================================================================
test('getPortSide returns correct side', () => {
  const block = { x: 0, y: 0, width: 100, height: 50 };
  const center = { x: 50, y: 25 };
  expect(mod.getPortSide(block, { x: 10, y: 25 })).toBe('left');
  expect(mod.getPortSide(block, { x: 90, y: 25 })).toBe('right');
  expect(mod.getPortSide(block, { x: 50, y: 5 })).toBe('top');
  expect(mod.getPortSide(block, { x: 50, y: 45 })).toBe('bottom');
});
//=============================================================================
test('blockBounds expands for ports', () => {
  const block: any = { x: 0, y: 0, width: 20, height: 20, rotation: 0, ports: [{ x: 30, y: 10 }] };
  const b = mod.blockBounds(block);
  expect(b.right).toBeGreaterThan(block.x + block.width / 2);
});
//=============================================================================
test('segmentHitsRect handles vertical and horizontal segments', () => {
  const rect = { left: 0, right: 10, top: 0, bottom: 10 };
  expect(mod.intersectsAxisAlignedRect({ a: { x: 5, y: -5 }, b: { x: 5, y: 5 } }, rect)).toBe(true);
  expect(mod.intersectsAxisAlignedRect({ a: { x: -1, y: 5 }, b: { x: 5, y: 5 } }, rect)).toBe(true);
  expect(mod.intersectsAxisAlignedRect({ a: { x: -5, y: -5 }, b: { x: -1, y: -1 } }, rect)).toBe(
    false
  );
});
//=============================================================================
test('segmentsIntersect detects crossing and ignores parallel', () => {
  const h: any = { a: { x: 0, y: 5 }, b: { x: 10, y: 5 }, orientation: 'H' };
  const v: any = { a: { x: 5, y: 0 }, b: { x: 5, y: 10 }, orientation: 'V' };
  expect(mod.segmentsIntersect(h, v)).toBe(true);
  const h2: any = { a: { x: 20, y: 5 }, b: { x: 30, y: 5 }, orientation: 'H' };
  expect(mod.segmentsIntersect(h, h2)).toBe(false);
});
//=============================================================================
test('segmentsOverlap detects overlapping colinear segments', () => {
  const a: any = { a: { x: 0, y: 5 }, b: { x: 10, y: 5 }, orientation: 'H' };
  const b: any = { a: { x: 5, y: 5 }, b: { x: 15, y: 5 }, orientation: 'H' };
  expect(mod.segmentsOverlap(a, b)).toBe(true);
  const c: any = { a: { x: 11, y: 5 }, b: { x: 20, y: 5 }, orientation: 'H' };
  expect(mod.segmentsOverlap(a, c)).toBe(false);
  const v: any = { a: { x: 5, y: 0 }, b: { x: 5, y: 10 }, orientation: 'V' };
  expect(mod.segmentsOverlap(a, v)).toBe(false);
});
//=============================================================================
let g: any;
beforeEach(async () => {
  g = await import('../../../dist/render/geometry.js');
});
//=============================================================================
test('rotatePoint handles 0/90/180/270 rotations', () => {
  const pt = { x: 10, y: 10 };
  const block = { x: 0, y: 0, width: 20, height: 20 };
  expect(g.rotatePoint(pt, { ...block, rotation: 0 })).toEqual(pt);
  const center = { x: 10, y: 10 };
  expect(g.rotatePoint(center, { ...block, rotation: 90 })).toEqual(center);
  const right = { x: 20, y: 10 };
  const rotated90 = g.rotatePoint(right, { ...block, rotation: 90 });
  expect(rotated90.x).toBeLessThanOrEqual(10);
  expect(rotated90.y).toBeGreaterThanOrEqual(10);
  const rotated180 = g.rotatePoint(right, { ...block, rotation: 180 });
  expect(rotated180.x).toBeGreaterThanOrEqual(0);
});
//=============================================================================
test('getRotatedBounds swaps width/height for 90/270', () => {
  const b = { x: 0, y: 0, width: 40, height: 20, rotation: 90 };
  const r = g.getRotatedBounds(b);
  expect(r.right - r.left).toBeCloseTo(20);
  expect(r.bottom - r.top).toBeCloseTo(40);
});
//=============================================================================
test('getPortSide returns correct side', () => {
  const block = { x: 0, y: 0, width: 20, height: 20 };
  const left = { x: -10, y: 10 };
  expect(g.getPortSide(block, left)).toBe('left');
  const right = { x: 30, y: 10 };
  expect(g.getPortSide(block, right)).toBe('right');
  const top = { x: 10, y: -5 };
  expect(g.getPortSide(block, top)).toBe('top');
  const bottom = { x: 10, y: 50 };
  expect(g.getPortSide(block, bottom)).toBe('bottom');
});
//=============================================================================
test('blockBounds includes ports radius in bounds', () => {
  const block = { x: 0, y: 0, width: 20, height: 20, rotation: 0, ports: [{ x: 30, y: 10 }] };
  const bounds = g.blockBounds(block);
  expect(bounds.right).toBeGreaterThan(20);
});
//=============================================================================
test('segmentHitsRect works for vertical and horizontal segments', () => {
  const rect = { left: 0, right: 10, top: 0, bottom: 10 };
  expect(g.intersectsAxisAlignedRect({ a: { x: 5, y: -5 }, b: { x: 5, y: 5 } }, rect)).toBe(true);
  expect(g.intersectsAxisAlignedRect({ a: { x: -5, y: 5 }, b: { x: 5, y: 5 } }, rect)).toBe(true);
  expect(g.intersectsAxisAlignedRect({ a: { x: 0, y: 0 }, b: { x: 5, y: 5 } }, rect)).toBe(false);
});
//=============================================================================
test('segmentsIntersect detects orthogonal intersection', () => {
  const h = { a: { x: 0, y: 5 }, b: { x: 10, y: 5 } };
  const v = { a: { x: 5, y: 0 }, b: { x: 5, y: 10 } };
  expect(g.segmentsIntersect(h, v)).toBe(true);
  expect(g.segmentsIntersect(h, { a: { x: 0, y: 6 }, b: { x: 10, y: 6 } })).toBe(false);
});
//=============================================================================
test('segmentsOverlap detects overlapping colinear segments', () => {
  const a = { orientation: 'H', a: { x: 0, y: 0 }, b: { x: 10, y: 0 } };
  const b = { orientation: 'H', a: { x: 5, y: 0 }, b: { x: 15, y: 0 } };
  expect(g.segmentsOverlap(a, b)).toBe(true);
  const c = { orientation: 'H', a: { x: 11, y: 0 }, b: { x: 20, y: 0 } };
  expect(g.segmentsOverlap(a, c)).toBe(false);
});
//=============================================================================
describe('type contracts', () => {
  test('rotatePoint and getRotatedBounds return point/bounds with numeric x/y/left/right', () => {
    const p = g.rotatePoint({ x: 1, y: 2 }, { x: 0, y: 0, width: 4, height: 4 });
    expect(typeof p.x).toBe('number');
    expect(typeof p.y).toBe('number');
    const b = g.getRotatedBounds({ x: 0, y: 0, width: 4, height: 4, rotation: 0 });
    ['left', 'right', 'top', 'bottom'].forEach((k: any) => expect(typeof b[k]).toBe('number'));
  });

  test('getPortSide returns one of expected strings and segments functions return boolean', () => {
    const side = g.getPortSide({ x: 0, y: 0, width: 10, height: 10 }, { x: -1, y: 5 });
    expect(['left', 'right', 'top', 'bottom']).toContain(side);
    const h = { a: { x: 0, y: 5 }, b: { x: 10, y: 5 } };
    const v = { a: { x: 5, y: 0 }, b: { x: 5, y: 10 } };
    expect(typeof g.segmentsIntersect(h, v)).toBe('boolean');
    const a = { orientation: 'H', a: { x: 0, y: 0 }, b: { x: 10, y: 0 } };
    const b = { orientation: 'H', a: { x: 5, y: 0 }, b: { x: 15, y: 0 } };
    expect(typeof g.segmentsOverlap(a, b)).toBe('boolean');
  });
});
//=============================================================================
