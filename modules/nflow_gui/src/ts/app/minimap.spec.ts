//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { mountMinimap, toggleMinimap, diagramBounds, refreshMinimap } from './minimap.js';
import { state } from './state.js';
import { initViewBox, setSvg, setViewBox } from './workspace-handlers.js';
import { _resetUiPrefsForTests } from './ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  document.body.innerHTML = `
    <div id="canvas">
      <svg id="svgCanvas">
        <g id="wireLayer"></g>
        <g id="blockLayer"></g>
        <g id="overlayLayer"></g>
      </svg>
    </div>`;
  state.blocks.clear();
  try {
    localStorage.clear();
  } catch {
    /* ignore */
  }
  setSvg(document.getElementById('svgCanvas') as unknown as SVGSVGElement);
  initViewBox();
  setViewBox({ x: 0, y: 0, w: 800, h: 600 });
});
//=============================================================================
test('mountMinimap adds the overview with live layer references', () => {
  mountMinimap();
  const map = document.getElementById('minimap')!;
  expect(map).not.toBeNull();
  expect(map.hidden).toBe(false);
  const uses = map.querySelectorAll('use');
  expect(uses.length).toBe(2);
  expect(document.getElementById('minimapViewport')).not.toBeNull();
});
//=============================================================================
test('viewport rectangle follows setViewBox', () => {
  mountMinimap();
  setViewBox({ x: 100, y: 50, w: 400, h: 300 });
  const rect = document.getElementById('minimapViewport')!;
  expect(rect.getAttribute('x')).toBe('100');
  expect(rect.getAttribute('y')).toBe('50');
  expect(rect.getAttribute('width')).toBe('400');
});
//=============================================================================
test('diagramBounds covers blocks and viewport', () => {
  state.blocks.set('a' as never, { x: -200, y: 0, width: 60, height: 40 } as never);
  state.blocks.set('b' as never, { x: 900, y: 700, width: 60, height: 40 } as never);
  const bounds = diagramBounds();
  expect(bounds.x).toBeLessThanOrEqual(-200);
  expect(bounds.x + bounds.w).toBeGreaterThanOrEqual(960);
  expect(bounds.y + bounds.h).toBeGreaterThanOrEqual(740);
});
//=============================================================================
test('toggleMinimap hides/shows and persists', () => {
  mountMinimap();
  toggleMinimap();
  expect(document.getElementById('minimap')!.hidden).toBe(true);
  expect(localStorage.getItem('nf-minimap-open')).toBe('0');
  toggleMinimap();
  expect(document.getElementById('minimap')!.hidden).toBe(false);
  refreshMinimap();
});
//=============================================================================
