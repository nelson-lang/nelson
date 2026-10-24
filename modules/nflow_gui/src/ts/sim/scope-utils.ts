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
export function roundToSignificant(value: number, digits = 2): number {
  if (!Number.isFinite(value) || value === 0) return value;
  return Number(value.toPrecision(digits));
}
//=============================================================================
export function refreshParamDisplay(scopeBlock: any) {
  if (typeof window === 'undefined') return;
  if (typeof (window as any).nflowUpdateParamDisplay !== 'function') return;
  (window as any).nflowUpdateParamDisplay(scopeBlock?.id);
}
//=============================================================================
export function parseLimit(value: unknown): number | null {
  if (value == null) return null;
  if (typeof value === 'number' && Number.isFinite(value)) return value as number;
  if (typeof value === 'string') {
    const trimmed = value.trim();
    if (!trimmed) return null;
    const num = Number(trimmed);
    return Number.isFinite(num) ? num : null;
  }
  return null;
}
//=============================================================================
export function niceStep(range: number, target = 5) {
  if (!Number.isFinite(range) || range <= 0) return 1;
  const raw = range / target;
  const pow = Math.pow(10, Math.floor(Math.log10(raw)));
  const scaled = raw / pow;
  let step = 1;
  if (scaled <= 1) step = 1;
  else if (scaled <= 2) step = 2;
  else if (scaled <= 5) step = 5;
  else step = 10;
  return step * pow;
}
//=============================================================================
export function buildTicks(min: number, max: number, step: number) {
  if (!Number.isFinite(min) || !Number.isFinite(max) || !Number.isFinite(step) || step <= 0)
    return [];
  const start = Math.ceil(min / step) * step;
  const ticks: number[] = [];
  for (let v = start; v <= max + step * 0.5; v += step) ticks.push(v);
  return ticks;
}
//=============================================================================
export function formatTick(value: number) {
  if (!Number.isFinite(value)) return '';
  if (value === 0) return '0';
  const abs = Math.abs(value);
  if (abs >= 10000 || abs < 1e-3) return value.toExponential(1).replace('+', '');
  return Number(value.toPrecision(3)).toString();
}
//=============================================================================
/** Default floor for log scale - values below this are clamped to avoid log(0). */
export const LOG_FLOOR = 1e-6;
/** Clamp a value to the log-safe floor and take log10. */
export function toLog(value: number, floor = LOG_FLOOR): number {
  return Math.log10(Math.max(value, floor));
}
/**
 * Build tick values for a log-scale Y axis.
 * Returns powers of 10 that fall in [min, max] (where min/max are real values, not log).
 * Falls back to linear ticks if the range does not span an order of magnitude.
 */
export function buildLogTicks(minVal: number, maxVal: number): number[] {
  if (minVal <= 0) minVal = LOG_FLOOR;
  if (maxVal <= minVal) maxVal = minVal * 10;
  const logMin = Math.floor(Math.log10(minVal));
  const logMax = Math.ceil(Math.log10(maxVal));
  const ticks: number[] = [];
  for (let exp = logMin; exp <= logMax; exp++) {
    const v = Math.pow(10, exp);
    if (v >= minVal && v <= maxVal) ticks.push(v);
    const v2 = 2 * v;
    if (v2 >= minVal && v2 <= maxVal) ticks.push(v2);
    const v5 = 5 * v;
    if (v5 >= minVal && v5 <= maxVal) ticks.push(v5);
  }
  return Array.from(new Set(ticks)).sort((a, b) => a - b);
}
/**
 * Map a real value to a pixel Y coordinate using a log10 scale.
 * Values ≤ 0 are floored to LOG_FLOOR.
 */
export function logValueToPixelY(
  value: number,
  minVal: number,
  maxVal: number,
  plotY: number,
  plotH: number,
  floor = LOG_FLOOR
): number {
  const logMin = toLog(Math.max(minVal, floor), floor);
  const logMax = toLog(Math.max(maxVal, floor), floor);
  const logVal = toLog(Math.max(value, floor), floor);
  const logRange = logMax - logMin;
  if (logRange <= 0) return plotY + plotH / 2;
  const ratio = (logVal - logMin) / logRange;
  return plotY + plotH - ratio * plotH;
}
//=============================================================================
