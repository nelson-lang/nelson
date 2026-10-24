//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Block } from '../types.js';
//=============================================================================
export function dashboardColor(value: unknown, fallback: string): string {
  if (!Array.isArray(value) || value.length < 3) return fallback;
  const channels = value.slice(0, 3).map((item) => {
    const channel = Number(item);
    return Math.max(0, Math.min(255, Math.round((Number.isFinite(channel) ? channel : 0) * 255)));
  });
  return `rgb(${channels.join(',')})`;
}
//=============================================================================
export function applyDashboardOpacity(block: Block): void {
  const opacity = Math.max(0, Math.min(1, Number(block.params?.Opacity ?? 1)));
  block.group.setAttribute('opacity', String(Number.isFinite(opacity) ? opacity : 1));
}
//=============================================================================
export function dashboardLimits(block: Block): [number, number] {
  const limits = block.params?.Limits;
  if (!Array.isArray(limits) || limits.length < 3) return [0, 100];
  const lo = Number(limits[0]);
  const hi = Number(limits[2]);
  return Number.isFinite(lo) && Number.isFinite(hi) && hi > lo ? [lo, hi] : [0, 100];
}
//=============================================================================
export function dashboardTickStep(block: Block): number {
  const limits = block.params?.Limits;
  if (Array.isArray(limits) && Number(limits[1]) > 0) return Number(limits[1]);
  const [lo, hi] = dashboardLimits(block);
  return (hi - lo) / 100;
}
//=============================================================================
export function dashboardScaleType(block: Block): 'Linear' | 'Log' {
  return block.params?.ScaleType === 'Log' ? 'Log' : 'Linear';
}
//=============================================================================
export function dashboardRatio(
  value: number,
  limits: [number, number],
  scaleType: 'Linear' | 'Log' = 'Linear'
): number {
  const [lo, hi] = limits;
  const clamped = Math.max(lo, Math.min(hi, value));
  if (scaleType === 'Log') {
    const positiveLo = lo > 0 ? lo : Math.min(1, hi);
    if (!(positiveLo > 0) || !(hi > positiveLo) || clamped <= 0) return 0;
    return Math.max(
      0,
      Math.min(
        1,
        (Math.log(clamped) - Math.log(positiveLo)) / (Math.log(hi) - Math.log(positiveLo))
      )
    );
  }
  return Math.max(0, Math.min(1, (clamped - lo) / (hi - lo)));
}
//=============================================================================
export function dashboardValue(
  ratio: number,
  limits: [number, number],
  scaleType: 'Linear' | 'Log' = 'Linear',
  step = -1
): number {
  const [lo, hi] = limits;
  const clampedRatio = Math.max(0, Math.min(1, ratio));
  const positiveLo = lo > 0 ? lo : Math.min(1, hi);
  let value =
    scaleType === 'Log' && positiveLo > 0 && hi > positiveLo
      ? Math.exp(Math.log(positiveLo) + clampedRatio * (Math.log(hi) - Math.log(positiveLo)))
      : lo + clampedRatio * (hi - lo);
  if (step > 0) value = lo + Math.round((value - lo) / step) * step;
  return Math.max(lo, Math.min(hi, value));
}
//=============================================================================
function engineering(value: number, digits: number): string {
  if (value === 0) return (0).toFixed(digits);
  const exponent = Math.floor(Math.log10(Math.abs(value)) / 3) * 3;
  return `${(value / 10 ** exponent).toFixed(digits)}e${exponent >= 0 ? '+' : ''}${exponent}`;
}
//=============================================================================
function rational(value: number): string {
  if (!Number.isFinite(value)) return String(value);
  const tolerance = 1e-10;
  let numerator0 = 0;
  let numerator1 = 1;
  let denominator0 = 1;
  let denominator1 = 0;
  let x = Math.abs(value);
  for (let iteration = 0; iteration < 32; iteration += 1) {
    const integer = Math.floor(x);
    const numerator2 = integer * numerator1 + numerator0;
    const denominator2 = integer * denominator1 + denominator0;
    if (denominator2 > 1_000_000) break;
    numerator0 = numerator1;
    numerator1 = numerator2;
    denominator0 = denominator1;
    denominator1 = denominator2;
    if (Math.abs(value - Math.sign(value || 1) * (numerator1 / denominator1)) < tolerance) break;
    const fraction = x - integer;
    if (fraction === 0) break;
    x = 1 / fraction;
  }
  return `${Math.sign(value) * numerator1}/${denominator1}`;
}
//=============================================================================
function customFormat(pattern: string, value: number): string {
  return pattern.replace(
    /%([+\- 0]*)(\d+)?(?:\.(\d+))?([fFeEgGdi])/g,
    (_all, flags, width, precision, kind) => {
      const digits = precision == null ? 6 : Number(precision);
      let text =
        kind.toLowerCase() === 'f'
          ? value.toFixed(digits)
          : kind.toLowerCase() === 'e'
          ? value.toExponential(digits)
          : kind.toLowerCase() === 'g'
          ? value.toPrecision(digits)
          : String(Math.round(value));
      if (flags.includes('+') && value >= 0) text = `+${text}`;
      const targetWidth = Number(width || 0);
      if (text.length < targetWidth)
        text = text.padStart(targetWidth, flags.includes('0') ? '0' : ' ');
      return text;
    }
  );
}
//=============================================================================
export function formatDashboardValue(value: number, format = 'short', formatString = '%d'): string {
  if (!Number.isFinite(value)) return '--';
  switch (format) {
    case 'long':
      return value.toFixed(15);
    case 'shortE':
    case 'short_e':
      return value.toExponential(4);
    case 'longE':
      return value.toExponential(15);
    case 'shortG':
      return value.toPrecision(5);
    case 'longG':
      return value.toPrecision(15);
    case 'shortEng':
      return engineering(value, 4);
    case 'longEng':
      return engineering(value, 15);
    case 'bank':
      return value.toFixed(2);
    case '+':
      return value > 0 ? '+' : value < 0 ? '-' : ' ';
    case 'hex': {
      const buffer = new ArrayBuffer(8);
      new DataView(buffer).setFloat64(0, value, false);
      return [...new Uint8Array(buffer)].map((byte) => byte.toString(16).padStart(2, '0')).join('');
    }
    case 'rat':
      return rational(value);
    case 'Custom':
      return customFormat(formatString, value);
    case 'Integer':
      return String(Math.round(value));
    default:
      return value.toFixed(4);
  }
}
//=============================================================================
export function createDashboardThrottle(
  callback: (value: number) => void,
  interval = 33
): { push: (value: number) => void; flush: (value?: number) => void } {
  let lastSent = -Infinity;
  let pending: number | undefined;
  let timer = 0;
  const send = (value: number) => {
    lastSent = performance.now();
    pending = undefined;
    callback(value);
  };
  const flush = (value?: number) => {
    if (timer) window.clearTimeout(timer);
    timer = 0;
    const next = value ?? pending;
    if (next !== undefined) send(next);
  };
  const push = (value: number) => {
    pending = value;
    const remaining = interval - (performance.now() - lastSent);
    if (remaining <= 0) {
      flush();
      return;
    }
    if (!timer) timer = window.setTimeout(() => flush(), remaining);
  };
  return { push, flush };
}
//=============================================================================
export function bindContinuousKeyboard(
  block: Block,
  read: () => number,
  update: (value: number) => void
): void {
  const [lo, hi] = dashboardLimits(block);
  const step = dashboardTickStep(block);
  block.group.setAttribute('tabindex', '0');
  block.group.setAttribute('role', 'slider');
  block.group.setAttribute('aria-valuemin', String(lo));
  block.group.setAttribute('aria-valuemax', String(hi));
  block.group.addEventListener('keydown', (rawEvent) => {
    const event = rawEvent as KeyboardEvent;
    let next: number | null = null;
    if (event.key === 'ArrowRight' || event.key === 'ArrowUp') next = read() + step;
    else if (event.key === 'ArrowLeft' || event.key === 'ArrowDown') next = read() - step;
    else if (event.key === 'Home') next = lo;
    else if (event.key === 'End') next = hi;
    if (next === null) return;
    event.preventDefault();
    event.stopPropagation();
    update(Math.max(lo, Math.min(hi, next)));
  });
}
//=============================================================================
