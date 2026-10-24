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
import { getViewBox, setViewBox, getSvg, getViewportSize } from './workspace-handlers.js';
//=============================================================================
export function updateViewBoxWithAnchor(
  scale: number,
  anchor: { x: number; y: number } | undefined,
  baseViewBox: any = null
) {
  const svg = getSvg();
  if (!svg) return;
  const currentViewBox = getViewBox();
  const actualBaseViewBox = baseViewBox || currentViewBox;
  const { w, h } = getViewportSize();
  const newW = w / scale;
  const newH = h / scale;
  const relX = (anchor!.x - actualBaseViewBox.x) / actualBaseViewBox.w;
  const relY = (anchor!.y - actualBaseViewBox.y) / actualBaseViewBox.h;
  const newViewBox = { x: anchor!.x - relX * newW, y: anchor!.y - relY * newH, w: newW, h: newH };
  setViewBox(newViewBox);
  const canvas = document.getElementById('canvas');
  updateGrid(canvas, scale, newViewBox);
}
//=============================================================================
export function updateGrid(canvas: Element | null, scale: number, viewBox: any) {
  if (!canvas) return;
  void viewBox;
  const pattern = canvas.querySelector('pattern');
  if (pattern) {
    const size = Math.max(5, Math.round(20 / scale));
    pattern.setAttribute('width', String(size));
    pattern.setAttribute('height', String(size));
  }
}
//=============================================================================
