//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  setSignalValues,
  clearSignalValues,
  getSignalValue,
  formatSignalValue,
  initSignalBadges,
} from './signal-badges.js';
import { state } from './state.js';
//=============================================================================
beforeEach(() => {
  clearSignalValues();
  state.connections.length = 0;
  document.body.innerHTML = `
    <svg id="svgCanvas">
      <g id="wireLayer"></g>
      <g id="blockLayer"></g>
      <g id="overlayLayer"></g>
    </svg>`;
});
//=============================================================================
test('getSignalValue reads per-port values and handles missing data', () => {
  setSignalValues({ b1: [1.5, -2], b2: [42] });
  expect(getSignalValue('b1', 0)).toBe(1.5);
  expect(getSignalValue('b1', 1)).toBe(-2);
  expect(getSignalValue('b2', undefined)).toBe(42);
  expect(getSignalValue('nope', 0)).toBeNull();
  clearSignalValues();
  expect(getSignalValue('b1', 0)).toBeNull();
});
//=============================================================================
test('formatSignalValue uses exponent notation for extreme magnitudes', () => {
  expect(formatSignalValue(0.5)).toBe('0.5');
  expect(formatSignalValue(12345.6)).toBe('1.235e+4');
  expect(formatSignalValue(0.0000012)).toBe('1.200e-6');
  expect(formatSignalValue(0)).toBe('0');
});
//=============================================================================
test('hovering a wire shows the badge with the source port value', () => {
  const svg = document.getElementById('svgCanvas') as unknown as SVGSVGElement;
  initSignalBadges(svg);
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  document.getElementById('wireLayer')!.appendChild(path);
  (state.connections as unknown as any[]).push({ from: 'b1', fromIndex: 0, to: 'b2', path });
  setSignalValues({ b1: [3.25] });

  path.dispatchEvent(
    new PointerEvent('pointerover', { bubbles: true, clientX: 100, clientY: 100 })
  );
  const badge = document.getElementById('signalBadge')!;
  expect(badge.hidden).toBe(false);
  expect(badge.textContent).toBe('3.25');

  path.dispatchEvent(new PointerEvent('pointerout', { bubbles: true }));
  expect(badge.hidden).toBe(true);
});
//=============================================================================
test('no badge without simulation data', () => {
  const svg = document.getElementById('svgCanvas') as unknown as SVGSVGElement;
  initSignalBadges(svg);
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  document.getElementById('wireLayer')!.appendChild(path);
  (state.connections as unknown as any[]).push({ from: 'b1', fromIndex: 0, to: 'b2', path });
  path.dispatchEvent(new PointerEvent('pointerover', { bubbles: true, clientX: 10, clientY: 10 }));
  const badge = document.getElementById('signalBadge');
  expect(badge === null || badge.hidden).toBe(true);
});
//=============================================================================
test('getSignalBusInfo exposes the per-port bus descriptor', async () => {
  const { getSignalBusInfo } = await import('./signal-badges.js');
  setSignalValues({ bc: [[1, 2, 7]] }, { bc: ['double'] }, { bc: [[3, 1]] }, null, {
    bc: [
      {
        name: '',
        nonvirtual: false,
        members: [
          { name: 'pos', offset: 0, width: 2 },
          { name: 'count', offset: 2, width: 1 },
        ],
      },
    ],
    other: [null],
  });
  const bus = getSignalBusInfo('bc', 0) as { members?: Array<{ name?: string }> } | null;
  expect(bus).not.toBeNull();
  expect(bus?.members?.map((m) => m.name)).toEqual(['pos', 'count']);
  expect(getSignalBusInfo('other', 0)).toBeNull();
  expect(getSignalBusInfo('missing', 0)).toBeNull();
});
//=============================================================================
