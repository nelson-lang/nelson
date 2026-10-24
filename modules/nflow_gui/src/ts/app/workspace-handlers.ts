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
import { state } from './state.js';
import { getGridSize } from './grid-manager.js';
import { emitAppEvent } from './app-events.js';
//=============================================================================
export type ViewBox = { x: number; y: number; w: number; h: number };
// Shared zoom bounds so every entry point (fit, zoom buttons, mouse wheel)
// agrees. The floor is low enough that "reset view" can always frame a large
// model; the cap prevents magnifying a tiny model past 3x.
export const MIN_ZOOM_SCALE = 0.01;
export const MAX_ZOOM_SCALE = 3;
export function clampZoomScale(scale: number): number {
  return Math.max(MIN_ZOOM_SCALE, Math.min(MAX_ZOOM_SCALE, scale));
}
//=============================================================================
type RendererRef = { current: { clearWorkspace?: () => void } | null } | null;
type UpdateStatusBar = ((s: string | null, n: number | null, zoom: number) => void) | null;
let rendererRef: RendererRef = null;
let viewBox: ViewBox = { x: 0, y: 0, w: 0, h: 0 };
let zoomScale = 1;
let svg: SVGSVGElement | null = null;
let updateStatusBar: UpdateStatusBar = null;
const WORLD = { w: 4000, h: 3000 };
export function setRendererRef(ref: RendererRef): void {
  rendererRef = ref;
}
//=============================================================================
export function setSvg(el: SVGSVGElement | null): void {
  svg = el;
}
//=============================================================================
export function getSvg(): SVGSVGElement | null {
  return svg;
}
//=============================================================================
export function setUpdateStatusBar(fn: UpdateStatusBar): void {
  updateStatusBar = fn;
}
//=============================================================================
export function getUpdateStatusBar(): UpdateStatusBar {
  return updateStatusBar;
}
//=============================================================================
export function getViewBox(): ViewBox {
  return viewBox;
}
//=============================================================================
export function setViewBox(newViewBox: ViewBox): void {
  viewBox = newViewBox;
  if (svg) {
    svg.setAttribute('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
    const canvas = document.getElementById('canvas');
    updateGrid(canvas as HTMLElement | null, zoomScale, viewBox);
  }
  emitAppEvent('nf-viewbox-changed');
}
//=============================================================================
export function getZoomScale(): number {
  return zoomScale;
}
//=============================================================================
export function setZoomScale(newScale: number): void {
  zoomScale = newScale;
}
//=============================================================================
export function updateGrid(canvas: HTMLElement | null, scale: number, viewbox: ViewBox): void {
  if (!canvas) return;
  const userGridSize = getGridSize();
  const gridPx = userGridSize * scale;
  const mod = (value: number, modValue: number) => ((value % modValue) + modValue) % modValue;
  const offsetX = -mod(viewbox.x * scale, gridPx);
  const offsetY = -mod(viewbox.y * scale, gridPx);
  (canvas.style as CSSStyleDeclaration).setProperty('--grid-size', `${gridPx}px`);
  (canvas.style as CSSStyleDeclaration).setProperty('--grid-offset-x', `${offsetX}px`);
  (canvas.style as CSSStyleDeclaration).setProperty('--grid-offset-y', `${offsetY}px`);
}
//=============================================================================
export function initViewBox(): void {
  const { w, h } = getViewportSize();
  if (viewBox.w === 0 || viewBox.h === 0) {
    zoomScale = 1.5;
    const vbW = w / zoomScale;
    const vbH = h / zoomScale;
    viewBox = {
      x: (WORLD.w - vbW) / 2,
      y: (WORLD.h - vbH) / 2,
      w: vbW,
      h: vbH,
    };
    if (svg) {
      svg.setAttribute('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
      svg.dataset.worldWidth = String(WORLD.w);
      svg.dataset.worldHeight = String(WORLD.h);
    }
    const canvas = document.getElementById('canvas');
    updateGrid(canvas as HTMLElement | null, zoomScale, viewBox);
    if (updateStatusBar) updateStatusBar(null, null, zoomScale);
    return;
  }
  const center = { x: viewBox.x + viewBox.w / 2, y: viewBox.y + viewBox.h / 2 };
  viewBox = {
    x: center.x - w / zoomScale / 2,
    y: center.y - h / zoomScale / 2,
    w: w / zoomScale,
    h: h / zoomScale,
  };
  if (svg) {
    svg.setAttribute('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
  }
  const canvas = document.getElementById('canvas');
  updateGrid(canvas as HTMLElement | null, zoomScale, viewBox);
  if (updateStatusBar) updateStatusBar(null, null, zoomScale);
}
//=============================================================================
export function ensureLeftMargin(px: number, padding = 0): void {
  if (!state || !state.blocks || state.blocks.size === 0) return;
  let minX = Infinity;
  state.blocks.forEach((block: any) => {
    const b = block as { x?: number } | undefined;
    if (b && typeof b.x === 'number') {
      minX = Math.min(minX, b.x);
    }
  });
  if (!isFinite(minX)) return;
  const desiredPx = (px || 0) + padding;
  const screenX = (minX - viewBox.x) * zoomScale;
  if (screenX < desiredPx) {
    const neededViewX = minX - desiredPx / zoomScale;
    const newViewBox = { x: neededViewX, y: viewBox.y, w: viewBox.w, h: viewBox.h };
    setViewBox(newViewBox);
    if (updateStatusBar) updateStatusBar(null, null, zoomScale);
  }
}
//=============================================================================
// A hidden or not-yet-laid-out canvas reports a zero-size rect. Falling back to
// `|| 1` gives a 1px viewport and a runaway zoom, so use a non-zero default and a
// deferred fit still frames the model.
export const FALLBACK_VIEWPORT = { w: 800, h: 600 } as const;
export function getViewportSize(): { w: number; h: number } {
  const canvas = document.getElementById('canvas');
  if (canvas) {
    const rect = canvas.getBoundingClientRect();
    if (rect.width && rect.height) {
      return { w: rect.width, h: rect.height };
    }
  }
  const svgW = svg?.clientWidth;
  const svgH = svg?.clientHeight;
  if (svgW && svgH) {
    return { w: svgW, h: svgH };
  }
  return { w: FALLBACK_VIEWPORT.w, h: FALLBACK_VIEWPORT.h };
}
//=============================================================================
export function clearWorkspace(): void {
  if (rendererRef?.current) {
    rendererRef.current.clearWorkspace?.();
  }
  state.spawnIndex = 0;
  if (typeof updateStatusBar === 'function') {
    updateStatusBar('Idle', 0, 1);
  }
  const inspectorBody = document.getElementById('inspectorBody');
  if (inspectorBody) {
    inspectorBody.textContent = 'Select a block or wire.';
  }
}
//=============================================================================
export function sanitizeParamsForSave(params: unknown): unknown {
  if (!params || typeof params !== 'object') return params || {};
  const cleaned = { ...(params as Record<string, unknown>) };
  const vis = cleaned._visible;
  if (vis && typeof vis === 'object') {
    const visible: Record<string, unknown> = {};
    Object.entries(vis as Record<string, unknown>).forEach(([key, value]) => {
      if (key === '{}') return;
      if (!value) return;
      visible[key] = value;
    });
    cleaned._visible = visible;
  }
  return cleaned;
}
//=============================================================================
