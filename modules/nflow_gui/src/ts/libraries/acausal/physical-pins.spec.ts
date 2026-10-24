//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  pinWorldPosition,
  hitTestPin,
  layoutDefaultPins,
  layoutPhysicalPinsWithFrames,
  physicalPinMarkerSvg,
  pinInk,
  isPhysicalPinBlock,
  appendPhysicalPins,
  type PhysicalPin,
} from './physical-pins';
//=============================================================================
const RES_PINS: PhysicalPin[] = [
  { name: 'p', x: 0, y: 30, side: 'left', domain: 'electrical' },
  { name: 'n', x: 100, y: 30, side: 'right', domain: 'electrical' },
];
//=============================================================================
test('pinWorldPosition offsets by the block origin', () => {
  expect(pinWorldPosition({ x: 0, y: 30 }, 200, 100)).toEqual({ x: 200, y: 130 });
});
//=============================================================================
test('hitTestPin grabs the nearest pin within the radius', () => {
  expect(hitTestPin(RES_PINS, 200, 100, 202, 132)?.name).toBe('p');
  expect(hitTestPin(RES_PINS, 200, 100, 298, 131)?.name).toBe('n');
  expect(hitTestPin(RES_PINS, 200, 100, 250, 130)).toBeNull(); // midway, too far
});
//=============================================================================
test('layoutDefaultPins centers a single pin and spreads several', () => {
  const one = layoutDefaultPins(['a'], 80, 60, 'left', 'thermal');
  expect(one[0]).toMatchObject({ x: 0, y: 30, side: 'left', domain: 'thermal' });
  const three = layoutDefaultPins(['a', 'b', 'c'], 80, 60, 'right', 'electrical');
  expect(three.map((p) => p.x)).toEqual([80, 80, 80]);
  expect(three[0].y).toBeLessThan(three[1].y);
  expect(three[1].y).toBeLessThan(three[2].y);
  const top = layoutDefaultPins(['a', 'b'], 80, 60, 'top', 'rotational');
  expect(top.every((p) => p.y === 0)).toBe(true);
  expect(top[0].x).toBeLessThan(top[1].x);
});
//=============================================================================
// A PlanarBody: manifest "com" on the left; frames "prox" (left) and "dist"
// (right). Geometry mirrors the double-pendulum demo (90x70 blocks).
const PLANAR_COM: PhysicalPin[] = [{ name: 'com', x: 0, y: 35, side: 'left', domain: 'planar' }];
const PLANAR_FRAMES = { prox: [-0.5, 0], dist: [0.5, 0] };
test('layoutPhysicalPinsWithFrames centres the frame on a mixed side, margins the manifest pin', () => {
  const pins = layoutPhysicalPinsWithFrames(PLANAR_COM, PLANAR_FRAMES, 90, 70, 'planar');
  const by = new Map(pins.map((p) => [p.name, p]));
  // The trunk frame "prox" (shares the LEFT edge with "com") sits dead-centre so
  // its wire to a centred joint pin is straight; "com" yields to the margin.
  expect(by.get('prox')).toMatchObject({ x: 0, side: 'left' });
  expect(by.get('prox')!.y).toBe(35);
  expect(by.get('com')!.y).toBeLessThan(35);
  // "dist" is alone on the RIGHT edge -> stays centred (also a straight wire).
  expect(by.get('dist')).toMatchObject({ x: 90, y: 35, side: 'right' });
  // Order is preserved (manifest first, then declaration order) for index stability.
  expect(pins.map((p) => p.name)).toEqual(['com', 'prox', 'dist']);
});
//=============================================================================
test('layoutPhysicalPinsWithFrames keeps a lone manifest pin centred when its frame is on another edge', () => {
  // Spring-pendulum body: "com" left (alone), "top" frame on the top edge.
  const pins = layoutPhysicalPinsWithFrames(PLANAR_COM, { top: [0, 0.3] }, 90, 70, 'planar');
  const by = new Map(pins.map((p) => [p.name, p]));
  expect(by.get('com')).toMatchObject({ x: 0, y: 35, side: 'left' });
  expect(by.get('top')).toMatchObject({ y: 0, side: 'top' });
  expect(by.get('top')!.x).toBe(45); // lone frame on its edge -> centred
});
//=============================================================================
test('physicalPinMarkerSvg is a hollow ring with NO arrowhead (undirected)', () => {
  const svg = physicalPinMarkerSvg(RES_PINS[0]);
  expect(svg).toContain('class="physical-pin"');
  expect(svg).toContain('data-pin="p"');
  expect(svg).toContain('data-domain="electrical"');
  expect(svg).toContain('cx="0"');
  expect(svg).toContain('cy="30"');
  expect(svg).not.toContain('marker-end');
  expect(svg).not.toContain('polygon');
  expect(svg).not.toContain('arrow');
});
//=============================================================================
test('pinInk maps each domain to a distinct colour', () => {
  const inks = new Set(
    ['electrical', 'thermal', 'translational', 'rotational', 'planar'].map(pinInk)
  );
  expect(inks.size).toBe(5); // all distinct
  expect(pinInk('unknown-domain')).toBe('#333333');
});
//=============================================================================
test('isPhysicalPinBlock only for acausal blocks', () => {
  expect(isPhysicalPinBlock({ acausal: true })).toBe(true);
  expect(isPhysicalPinBlock({ acausal: false })).toBe(false);
  expect(isPhysicalPinBlock({})).toBe(false);
  expect(isPhysicalPinBlock(undefined)).toBe(false);
});
//=============================================================================
test('appendPhysicalPins draws markers and registers connectable ports', () => {
  const NS = 'http://www.w3.org/2000/svg';
  const createSvgElement = (tag: string, attrs: Record<string, string | number>) => {
    const el = document.createElementNS(NS, tag);
    for (const [k, v] of Object.entries(attrs)) {
      el.setAttribute(k, String(v));
    }
    return el as unknown as { appendChild?: (c: unknown) => void } & Record<string, unknown>;
  };
  const group = document.createElementNS(NS, 'g');
  const block = { ports: [] as never[] };
  const els = appendPhysicalPins(
    group as unknown as { appendChild: (c: unknown) => void },
    block as never,
    RES_PINS,
    createSvgElement
  );
  expect(group.querySelectorAll('circle.physical-pin').length).toBe(2);
  expect(els).toHaveLength(2);
  expect(block.ports).toHaveLength(2);
  expect((block.ports as Array<{ type: string; name: string }>).map((p) => p.type)).toEqual([
    'phys',
    'phys',
  ]);
  expect((block.ports as Array<{ name: string }>).map((p) => p.name)).toEqual(['p', 'n']);
  expect((block.ports as Array<{ index: number }>).map((p) => p.index)).toEqual([0, 1]);
  const first = group.querySelector('circle.physical-pin')!;
  expect(first.getAttribute('data-pin')).toBe('p');
  expect(first.getAttribute('data-domain')).toBe('electrical');
  expect(first.getAttribute('stroke')).toBe(pinInk('electrical'));
  expect(group.querySelector('polygon')).toBeNull();
});
//=============================================================================
