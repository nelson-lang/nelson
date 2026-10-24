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
export type DisplayFormat =
  | 'short'
  | 'long'
  | 'short_e'
  | 'long_e'
  | 'bank'
  | 'hex'
  | 'binary'
  | 'decimal'
  | 'octal';
export function formatDisplayValue(value: number, format: DisplayFormat = 'short'): string {
  if (!isFinite(value)) return String(value);
  switch (format) {
    case 'short':
      return parseFloat(value.toPrecision(5)).toString();
    case 'long':
      return parseFloat(value.toPrecision(15)).toString();
    case 'short_e':
      return value.toExponential(4);
    case 'long_e':
      return value.toExponential(14);
    case 'bank':
      return value.toFixed(2);
    case 'hex':
      return '0x' + ((value >>> 0) >>> 0).toString(16).toUpperCase();
    case 'binary':
      return '0b' + ((value >>> 0) >>> 0).toString(2);
    case 'decimal':
      return String(Math.trunc(value));
    case 'octal':
      return '0o' + ((value >>> 0) >>> 0).toString(8);
    default:
      return String(value);
  }
}
//=============================================================================
export function renderDisplay(displayBlock: any): void {
  if (!displayBlock.displayValueEl || displayBlock.displayData == null) return;
  const value = displayBlock.displayData.value;
  const format = (displayBlock.params?.Format as DisplayFormat) ?? 'short';
  if (Array.isArray(value)) {
    // Vector signal: "[v1 v2 …]", truncated past a few elements.
    const maxShown = 4;
    const shown = value
      .slice(0, maxShown)
      .map((v) => (isFinite(v) ? formatDisplayValue(v, format) : '--'))
      .join(' ');
    const suffix = value.length > maxShown ? ' …' : '';
    displayBlock.displayValueEl.textContent = `[${shown}${suffix}]`;
    return;
  }
  if (value == null || !isFinite(value as number)) {
    displayBlock.displayValueEl.textContent = '--';
    return;
  }
  displayBlock.displayValueEl.textContent = formatDisplayValue(value as number, format);
}
//=============================================================================
