//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { computePhysicalBusPath } from './renderer-wire-apply.js';
//=============================================================================
function leftFlangeBlock(x: number, y: number): any {
  return {
    x,
    y,
    width: 100,
    height: 80,
    rotation: 0,
    ports: [{ type: 'phys', index: 0, x: 0, y: 40 }],
  };
}
//=============================================================================
test('collinear same-side physical pins render a clean bracket (no hook/overshoot)', () => {
  const top = leftFlangeBlock(1500, 1360);
  const bottom = leftFlangeBlock(1500, 1540);
  const path = computePhysicalBusPath(top, bottom, 0, 0);
  expect(path).not.toBeNull();
  const p = path!;
  expect(p.length).toBe(4); // pin -> stub corner -> stub corner -> pin
  const [a, c1, c2, b] = p;
  expect(a.x).toBe(b.x);
  expect(c1.x).toBe(c2.x);
  expect(c1.x).toBeLessThan(a.x);
  expect(c1.y).toBe(a.y);
  expect(c2.y).toBe(b.y);
  expect(Math.min(c1.y, c2.y)).toBe(Math.min(a.y, b.y));
  expect(Math.max(c1.y, c2.y)).toBe(Math.max(a.y, b.y));
});
//=============================================================================
test('non-collinear same-side physical pins fall back to the general router (null)', () => {
  const path = computePhysicalBusPath(
    leftFlangeBlock(1500, 1360),
    leftFlangeBlock(1620, 1540),
    0,
    0
  );
  expect(path).toBeNull();
});
//=============================================================================
function twoFlangeBlock(x: number, y: number): any {
  return {
    x,
    y,
    width: 90,
    height: 70,
    rotation: 0,
    ports: [
      { type: 'phys', index: 0, x: 0, y: 35 },
      { type: 'phys', index: 1, x: 90, y: 35 },
    ],
  };
}
//=============================================================================
test('facing pins on the same row render one straight segment (no jog, no over-the-top drape)', () => {
  const joint = twoFlangeBlock(330, 180); // right flange (index 1) at (420, 215)
  const body = twoFlangeBlock(540, 180); // left flange (index 0) at (540, 215)
  const path = computePhysicalBusPath(joint, body, 1, 0);
  expect(path).not.toBeNull();
  expect(path!.length).toBe(2); // dead straight: pin -> pin
  expect(path![0].y).toBe(path![1].y); // same row
  expect(path![1].x).toBeGreaterThan(path![0].x); // spans left-to-right across the gap
});
//=============================================================================
test('facing pins on different rows render a single mid-span jog (not over the top)', () => {
  const joint = twoFlangeBlock(330, 180); // right flange at y ~215
  const body = twoFlangeBlock(540, 210); // left flange 30px lower
  const path = computePhysicalBusPath(joint, body, 1, 0);
  expect(path).not.toBeNull();
  expect(path!.length).toBe(4); // pin -> jog corner -> jog corner -> pin
  const [a, c1, c2, b] = path!;
  expect(c1.x).toBe(c2.x); // the single vertical jog sits at one x (mid-span)
  expect(c1.x).toBeGreaterThan(a.x);
  expect(c2.x).toBeLessThan(b.x);
  expect(c1.y).toBe(a.y);
  expect(c2.y).toBe(b.y);
});
//=============================================================================
test('back-to-back facing pins (target block behind the source) fall back (null)', () => {
  // to-block is to the LEFT of the from-block: the pins do not face across a gap.
  const path = computePhysicalBusPath(twoFlangeBlock(540, 180), twoFlangeBlock(330, 180), 1, 0);
  expect(path).toBeNull();
});
//=============================================================================
test('a signal wire off a dual-port block (phys + out at index 0) is NOT treated as a bus', () => {
  // sensor: phys pin "a" on the left at index 0 AND a signal output at index 0.
  const sensor: any = {
    x: 330,
    y: 180,
    width: 90,
    height: 70,
    rotation: 0,
    ports: [
      { type: 'phys', index: 0, x: 0, y: 35 },
      { type: 'out', index: 0, x: 45, y: 70 },
    ],
  };
  // sink has a phys pin on its left at index 0 (would look "facing" geometrically).
  const sink = twoFlangeBlock(540, 180);
  expect(computePhysicalBusPath(sensor, sink, 0, 0)).toBeNull();
});
//=============================================================================
test('signal (non-physical) ports are not treated as a bus (null)', () => {
  const src: any = {
    x: 1500,
    y: 1360,
    width: 100,
    height: 80,
    rotation: 0,
    ports: [{ type: 'out', index: 0, x: 100, y: 40 }],
  };
  const dst: any = {
    x: 1500,
    y: 1540,
    width: 100,
    height: 80,
    rotation: 0,
    ports: [{ type: 'in', index: 0, x: 0, y: 40 }],
  };
  expect(computePhysicalBusPath(src, dst, 0, 0)).toBeNull();
});
//=============================================================================
