//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Qt-aligned geometry for the dial / scale uicontrols (uigauge, uiknob,
// uislider). Mirrors the native Qt widgets (and the engine's software
// rasterizer) so the SVG export and the live widgets draw the same scale:
// angles in degrees, Qt convention (y up) -> a point is (cx + cos*r, cy - sin*r);
// the dials sweep 225deg clockwise to -45deg.
export const GAUGE_NEEDLE = 'rgb(200,30,30)';
export const KNOB_POINTER = 'rgb(196,38,46)';
export const DISCRETE_POINTER = 'rgb(200,30,30)';
export const FRAME_COLOR = 'rgb(70,70,70)';
export const TICK_COLOR = 'rgb(40,40,40)';
export const TRACK_COLOR = 'rgb(160,160,160)';
export const SWITCH_TRACK = 'rgb(112,112,112)';
export function dialPoint(cx: number, cy: number, r: number, angleDeg: number): [number, number] {
  const a = (angleDeg * Math.PI) / 180;
  return [cx + Math.cos(a) * r, cy - Math.sin(a) * r];
}
//=============================================================================
export function dialFraction(v: number, min: number, max: number): number {
  return max > min ? Math.min(1, Math.max(0, (v - min) / (max - min))) : 0;
}
//=============================================================================
export function withinLimits(tick: number, min: number, max: number): boolean {
  const tol = (max - min) * 1e-9;
  return tick >= min - tol && tick <= max + tol;
}
//=============================================================================
export function autoTicks(min: number, max: number, divisions: number): number[] {
  const ticks: number[] = [];
  if (max <= min || divisions <= 0) return ticks;
  const step = (max - min) / divisions;
  for (let k = 0; k <= divisions; k++) {
    ticks.push(min + k * step);
  }
  return ticks;
}
//=============================================================================
