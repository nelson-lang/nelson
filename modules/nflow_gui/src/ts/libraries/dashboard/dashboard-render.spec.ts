//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment jsdom */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { describe, test, expect, vi } from 'vitest';
import { dashboardRenderer } from './dashboard';
//=============================================================================
const SVG = 'http://www.w3.org/2000/svg';
function makeBlock(type: string, params: Record<string, unknown>): any {
  const svg = document.createElementNS(SVG, 'svg');
  const group = document.createElementNS(SVG, 'g');
  svg.appendChild(group);
  document.body.appendChild(svg);
  return { id: 'b1', type, width: 140, height: 120, params, group };
}
//=============================================================================
describe('dashboard widget re-render', () => {
  test('rebuilds without stacking duplicate visuals (gauge scale)', () => {
    const block = makeBlock('dashboardGauge', { Limits: [0, -1, 100] });
    dashboardRenderer(block, undefined as never);
    expect(block.group.querySelectorAll('.dashboard-scale').length).toBe(1);

    // A new render (what an inspector edit triggers) must replace, not append.
    block.params.Limits = [0, -1, 500];
    dashboardRenderer(block, undefined as never);
    expect(block.group.querySelectorAll('.dashboard-scale').length).toBe(1);
    expect(block.group.querySelectorAll('.dashboard-frame').length).toBe(1);
  });

  test('re-applies a changed body param (lamp color) on re-render', () => {
    const block = makeBlock('dashboardLamp', { ColorDefault: [1, 0, 0] });
    dashboardRenderer(block, undefined as never);
    const fill1 = block.group.querySelector('.dashboard-lamp')?.getAttribute('fill');

    block.params.ColorDefault = [0, 0, 1];
    dashboardRenderer(block, undefined as never);
    const lamps = block.group.querySelectorAll('.dashboard-lamp');
    const fill2 = lamps[0]?.getAttribute('fill');

    expect(lamps.length).toBe(1);
    expect(fill1).toBeTruthy();
    expect(fill2).toBeTruthy();
    expect(fill2).not.toBe(fill1);
  });

  test('redraws the caption from LabelPosition/Label without duplicating it', () => {
    const block = makeBlock('dashboardSlider', {
      LabelPosition: 'Top',
      Label: 'Speed',
      Limits: [0, -1, 10],
    });
    dashboardRenderer(block, undefined as never);
    expect(block.group.querySelectorAll('.dashboard-widget-label').length).toBe(1);
    expect(block.group.querySelector('.dashboard-widget-label')?.textContent).toBe('Speed');

    block.params.LabelPosition = 'Hide';
    dashboardRenderer(block, undefined as never);
    expect(block.group.querySelectorAll('.dashboard-widget-label').length).toBe(0);
  });

  test('keeps the visuals above the drag handle after a re-render', () => {
    const block = makeBlock('dashboardGauge', { Limits: [0, -1, 100] });
    dashboardRenderer(block, undefined as never);
    // Emulate the creation flow which inserts the drag handle as the first child.
    const dragRect = document.createElementNS(SVG, 'rect');
    dragRect.setAttribute('class', 'drag-handle');
    block.group.insertBefore(dragRect, block.group.firstChild);
    block.dragRect = dragRect;

    block.params.Limits = [0, -1, 500];
    dashboardRenderer(block, undefined as never);

    expect(block.group.firstChild).toBe(dragRect);
    // The gauge frame must sit right after the drag handle, not before it.
    expect(dragRect.nextSibling).toBe(block.group.querySelector('.dashboard-frame'));
  });

  test('does not stack keyboard listeners across re-renders (slider)', () => {
    const block = makeBlock('dashboardSlider', { Limits: [0, 1, 10], DashboardValue: 5 });
    dashboardRenderer(block, undefined as never);
    dashboardRenderer(block, undefined as never); // re-render must drop the old listener
    block.group.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowUp', bubbles: true }));
    // A single surviving listener advances by one step (5 -> 6), not two (-> 7).
    expect(block.group.getAttribute('aria-valuenow')).toBe('6');
  });

  test('unsubscribes the previous simulation listener before re-subscribing', () => {
    const block = makeBlock('dashboardDisplay', {});
    dashboardRenderer(block, undefined as never);
    const off1 = block._dashboardOff;
    expect(typeof off1).toBe('function');

    const unsub = vi.fn(off1);
    block._dashboardOff = unsub;
    dashboardRenderer(block, undefined as never);
    expect(unsub).toHaveBeenCalledTimes(1);
    expect(typeof block._dashboardOff).toBe('function');
  });
});
//=============================================================================
