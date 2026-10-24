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
import { formatDisplayValue, renderDisplay } from './render-display';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeDisplayBlock = (overrides: any = {}) => ({
  id: 'disp1',
  type: 'display',
  group: document.createElementNS(ns, 'g') as any,
  width: 120,
  height: 60,
  params: { Format: 'short' },
  displayValueEl: document.createElementNS(ns, 'text') as any,
  displayData: null as any,
  ...overrides,
});
//=============================================================================
describe('formatDisplayValue', () => {
  test('short format trims trailing zeros', () => {
    expect(formatDisplayValue(3.14159265, 'short')).toBe('3.1416');
  });

  test('long format uses 15 significant digits', () => {
    const result = formatDisplayValue(1.23456789012345678, 'long');
    expect(result.length).toBeGreaterThan(5);
  });

  test('short_e format uses exponential notation', () => {
    expect(formatDisplayValue(12345.678, 'short_e')).toBe('1.2346e+4');
  });

  test('long_e format uses exponential notation with more digits', () => {
    expect(formatDisplayValue(1.23456789, 'long_e')).toMatch(/e/);
  });

  test('bank format shows 2 decimal places', () => {
    expect(formatDisplayValue(3.14159, 'bank')).toBe('3.14');
  });

  test('hex format shows hexadecimal', () => {
    expect(formatDisplayValue(255, 'hex')).toBe('0xFF');
  });

  test('binary format shows binary', () => {
    expect(formatDisplayValue(5, 'binary')).toBe('0b101');
  });

  test('decimal format truncates to integer', () => {
    expect(formatDisplayValue(3.99, 'decimal')).toBe('3');
  });

  test('octal format shows octal', () => {
    expect(formatDisplayValue(8, 'octal')).toBe('0o10');
  });

  test('returns "NaN" for NaN input', () => {
    expect(formatDisplayValue(NaN)).toBe('NaN');
  });

  test('returns "Infinity" for Infinity input', () => {
    expect(formatDisplayValue(Infinity)).toBe('Infinity');
  });

  test('default format is short', () => {
    expect(formatDisplayValue(1.0)).toBe('1');
  });
});
//=============================================================================
describe('renderDisplay', () => {
  test('does nothing when displayValueEl is absent', () => {
    const block = makeDisplayBlock({ displayValueEl: null });
    expect(() => renderDisplay(block)).not.toThrow();
  });

  test('does nothing when displayData is null', () => {
    const block = makeDisplayBlock({ displayData: null });
    expect(() => renderDisplay(block)).not.toThrow();
  });

  test('sets "--" when value is null', () => {
    const block = makeDisplayBlock({ displayData: { value: null } });

    renderDisplay(block);

    expect(block.displayValueEl.textContent).toBe('--');
  });

  test('sets "--" when value is NaN', () => {
    const block = makeDisplayBlock({ displayData: { value: NaN } });

    renderDisplay(block);

    expect(block.displayValueEl.textContent).toBe('--');
  });

  test('formats numeric value with block format param', () => {
    const block = makeDisplayBlock({
      params: { Format: 'bank' },
      displayData: { value: 3.14159 },
    });

    renderDisplay(block);

    expect(block.displayValueEl.textContent).toBe('3.14');
  });

  test('defaults to short format when no format param', () => {
    const block = makeDisplayBlock({ params: {}, displayData: { value: 3.14159265 } });

    renderDisplay(block);

    expect(block.displayValueEl.textContent).toBe('3.1416');
  });

  test('uses hex format when specified', () => {
    const block = makeDisplayBlock({
      params: { Format: 'hex' },
      displayData: { value: 255 },
    });

    renderDisplay(block);

    expect(block.displayValueEl.textContent).toBe('0xFF');
  });
});
//=============================================================================
