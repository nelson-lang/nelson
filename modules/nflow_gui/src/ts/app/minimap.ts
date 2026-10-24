//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Diagram overview (minimap): a corner SVG that references the wire/block
// layers via <use> and draws the viewport as a rectangle. Clicking recenters
// the view.
//=============================================================================
import { state } from './state.js';
import { getViewBox, setViewBox } from './workspace-handlers.js';
import { getUiPref, setUiPref } from './ui-prefs.js';
import { renderMinimapView } from './components/minimap-view.js';
import { onAppEvent } from './app-events.js';
//=============================================================================
const OPEN_KEY = 'nf-minimap-open';
const PADDING = 40;
type BlockLike = { x?: number; y?: number; width?: number; height?: number };
/** World-space bounds of the diagram (blocks + current viewport). */
export function diagramBounds(): { x: number; y: number; w: number; h: number } {
  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  state.blocks.forEach((b) => {
    const blk = b as BlockLike;
    const x = Number(blk.x) || 0;
    const y = Number(blk.y) || 0;
    const w = Number(blk.width) || 0;
    const h = Number(blk.height) || 0;
    minX = Math.min(minX, x);
    minY = Math.min(minY, y);
    maxX = Math.max(maxX, x + w);
    maxY = Math.max(maxY, y + h);
  });
  const vb = getViewBox();
  if (vb && Number.isFinite(vb.w) && vb.w > 0) {
    minX = Math.min(minX, vb.x);
    minY = Math.min(minY, vb.y);
    maxX = Math.max(maxX, vb.x + vb.w);
    maxY = Math.max(maxY, vb.y + vb.h);
  }
  if (!Number.isFinite(minX)) {
    return { x: 0, y: 0, w: 800, h: 600 };
  }
  return {
    x: minX - PADDING,
    y: minY - PADDING,
    w: Math.max(1, maxX - minX + 2 * PADDING),
    h: Math.max(1, maxY - minY + 2 * PADDING),
  };
}
//=============================================================================
function readOpen(): boolean {
  const v = getUiPref(OPEN_KEY, null);
  return v === null ? true : v === '1';
}
//=============================================================================
export function refreshMinimap(): void {
  if (typeof document === 'undefined') return;
  const map = document.getElementById('minimap');
  if (!map || map.hidden) return;
  const svg = map.querySelector('svg');
  const rect = document.getElementById('minimapViewport');
  if (!svg || !rect) return;
  const bounds = diagramBounds();
  svg.setAttribute('viewBox', `${bounds.x} ${bounds.y} ${bounds.w} ${bounds.h}`);
  const vb = getViewBox();
  if (vb) {
    rect.setAttribute('x', String(vb.x));
    rect.setAttribute('y', String(vb.y));
    rect.setAttribute('width', String(vb.w));
    rect.setAttribute('height', String(vb.h));
  }
}
//=============================================================================
export function toggleMinimap(force?: boolean): void {
  const map = document.getElementById('minimap');
  if (!map) return;
  const open = force !== undefined ? force : map.hidden;
  map.hidden = !open;
  setUiPref(OPEN_KEY, open ? '1' : '0');
  if (open) refreshMinimap();
}
//=============================================================================
export function mountMinimap(): void {
  if (typeof document === 'undefined') return;
  if (document.getElementById('minimap')) return;
  const canvas = document.getElementById('canvas');
  if (!canvas) return;

  const map = document.createElement('div');
  map.id = 'minimap';
  map.setAttribute('aria-label', 'Diagram overview');
  renderMinimapView(map);
  canvas.appendChild(map);

  map.addEventListener('click', (ev) => {
    const svg = map.querySelector('svg');
    if (!svg) return;
    const box = map.getBoundingClientRect();
    if (!box.width || !box.height) return;
    const bounds = diagramBounds();
    const scale = Math.min(box.width / bounds.w, box.height / bounds.h);
    const offsetX = (box.width - bounds.w * scale) / 2;
    const offsetY = (box.height - bounds.h * scale) / 2;
    const wx = bounds.x + (ev.clientX - box.left - offsetX) / scale;
    const wy = bounds.y + (ev.clientY - box.top - offsetY) / scale;
    const vb = getViewBox();
    if (vb && Number.isFinite(vb.w)) {
      setViewBox({ x: wx - vb.w / 2, y: wy - vb.h / 2, w: vb.w, h: vb.h });
    }
  });

  window.addEventListener('diagramChanged', () => refreshMinimap());
  onAppEvent('nf-viewbox-changed', () => refreshMinimap());
  window.addEventListener('resize', () => refreshMinimap());

  map.hidden = !readOpen();
  refreshMinimap();
}
//=============================================================================
export default mountMinimap;
//=============================================================================
