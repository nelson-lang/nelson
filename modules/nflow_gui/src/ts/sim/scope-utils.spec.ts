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
import {
  roundToSignificant,
  refreshParamDisplay,
  parseLimit,
  niceStep,
  buildTicks,
  formatTick,
  toLog,
  buildLogTicks,
  logValueToPixelY,
  LOG_FLOOR,
  applySignalStyle,
} from './scope-utils';
//=============================================================================
describe('applySignalStyle', () => {
  const makePath = () => document.createElementNS('http://www.w3.org/2000/svg', 'path');

  test('writes stroke color, width and dash array', () => {
    const p = makePath();
    applySignalStyle(p, { color: '#123456', strokeWidth: 5, dash: 'dashed' }, 'scope-path-1');
    const style = p.getAttribute('style') || '';
    expect(style).toContain('stroke:#123456');
    expect(style).toContain('stroke-width:5');
    expect(style).toContain('stroke-dasharray:6 3');
  });

  test('swaps a palette class for the fallback only when a color is set', () => {
    const p = makePath();
    p.setAttribute('class', 'scope-path-3');
    applySignalStyle(p, { color: '#abcdef' }, 'scope-path-1');
    expect(p.getAttribute('class')).toBe('scope-path-1');
  });

  test('leaves class and style untouched for empty signal params', () => {
    const p = makePath();
    p.setAttribute('class', 'scope-path-2');
    applySignalStyle(p, undefined, 'scope-path-1');
    expect(p.getAttribute('class')).toBe('scope-path-2');
    expect(p.getAttribute('style')).toBeNull();
  });

  test('solid dash clears the dash array', () => {
    const p = makePath();
    applySignalStyle(p, { dash: 'solid' }, 'scope-path-1');
    expect(p.getAttribute('style') || '').toContain('stroke-dasharray:none');
  });
});
//=============================================================================
describe('roundToSignificant', () => {
  test('rounds a positive number to 2 significant digits', () => {
    expect(roundToSignificant(1.2345, 2)).toBe(1.2);
  });

  test('returns 0 unchanged', () => {
    expect(roundToSignificant(0)).toBe(0);
  });

  test('handles negative values', () => {
    expect(roundToSignificant(-9876, 2)).toBe(-9900);
  });

  test('returns non-finite values unchanged', () => {
    expect(roundToSignificant(Infinity)).toBe(Infinity);
    expect(roundToSignificant(NaN)).toBeNaN();
  });

  test('uses 3 digits when specified', () => {
    expect(roundToSignificant(1.2345, 3)).toBeCloseTo(1.23, 5);
  });
});
//=============================================================================
describe('refreshParamDisplay', () => {
  test('does nothing when nflowUpdateParamDisplay is not defined', () => {
    const block = { id: 'b1' };
    expect(() => refreshParamDisplay(block)).not.toThrow();
  });

  test('calls nflowUpdateParamDisplay when defined', () => {
    const calls: any[] = [];
    (window as any).nflowUpdateParamDisplay = (id: string) => calls.push(id);

    refreshParamDisplay({ id: 'test-id' });

    expect(calls).toContain('test-id');
    delete (window as any).nflowUpdateParamDisplay;
  });
});
//=============================================================================
describe('parseLimit', () => {
  test('returns null for null/undefined', () => {
    expect(parseLimit(null)).toBeNull();
    expect(parseLimit(undefined)).toBeNull();
  });

  test('returns a finite number as-is', () => {
    expect(parseLimit(42)).toBe(42);
    expect(parseLimit(-3.14)).toBe(-3.14);
  });

  test('returns null for non-finite numbers', () => {
    expect(parseLimit(Infinity)).toBeNull();
    expect(parseLimit(NaN)).toBeNull();
  });

  test('parses a valid numeric string', () => {
    expect(parseLimit('3.14')).toBeCloseTo(3.14, 5);
    expect(parseLimit('-100')).toBe(-100);
  });

  test('returns null for empty/non-numeric strings', () => {
    expect(parseLimit('')).toBeNull();
    expect(parseLimit('abc')).toBeNull();
    expect(parseLimit('  ')).toBeNull();
  });
});
//=============================================================================
describe('niceStep', () => {
  test('returns 1 for non-positive or non-finite range', () => {
    expect(niceStep(0)).toBe(1);
    expect(niceStep(-5)).toBe(1);
    expect(niceStep(NaN)).toBe(1);
  });

  test('computes a nice step for a range of 10 with 5 ticks', () => {
    expect(niceStep(10, 5)).toBe(2);
  });

  test('computes a nice step for a range of 100', () => {
    expect(niceStep(100, 5)).toBe(20);
  });

  test('uses 5-target by default', () => {
    const step = niceStep(1);
    expect(typeof step).toBe('number');
    expect(step).toBeGreaterThan(0);
  });
});
//=============================================================================
describe('buildTicks', () => {
  test('returns empty array for invalid inputs', () => {
    expect(buildTicks(NaN, 10, 1)).toEqual([]);
    expect(buildTicks(0, 10, 0)).toEqual([]);
    expect(buildTicks(0, 10, -1)).toEqual([]);
  });

  test('builds ticks from 0 to 10 with step 2', () => {
    const ticks = buildTicks(0, 10, 2);

    expect(ticks).toContain(0);
    expect(ticks).toContain(4);
    expect(ticks).toContain(10);
  });

  test('handles negative range', () => {
    const ticks = buildTicks(-5, 5, 5);
    expect(ticks).toContain(-5);
    expect(ticks).toContain(0);
    expect(ticks).toContain(5);
  });
});
//=============================================================================
describe('formatTick', () => {
  test('returns empty string for non-finite', () => {
    expect(formatTick(Infinity)).toBe('');
    expect(formatTick(NaN)).toBe('');
  });

  test('returns "0" for zero', () => {
    expect(formatTick(0)).toBe('0');
  });

  test('uses exponential for very large values', () => {
    const result = formatTick(99999);
    expect(result).toMatch(/e/i);
  });

  test('uses exponential for very small values', () => {
    const result = formatTick(0.0001);
    expect(result).toMatch(/e/i);
  });

  test('formats normal values as toPrecision(3)', () => {
    // 1.23456 → '1.23' (3 sig figs, may have trailing zero trimmed)
    const result = formatTick(1.23456);
    expect(result).toBe('1.23');
  });
});
//=============================================================================
describe('toLog', () => {
  test('returns log10 of a positive value', () => {
    expect(toLog(100)).toBeCloseTo(2, 10);
    expect(toLog(10)).toBeCloseTo(1, 10);
    expect(toLog(1)).toBeCloseTo(0, 10);
  });

  test('floors zero to LOG_FLOOR', () => {
    expect(toLog(0)).toBeCloseTo(Math.log10(LOG_FLOOR), 10);
  });

  test('floors negative values to LOG_FLOOR', () => {
    expect(toLog(-5)).toBeCloseTo(Math.log10(LOG_FLOOR), 10);
  });

  test('uses custom floor when provided', () => {
    const floor = 0.01;
    expect(toLog(0, floor)).toBeCloseTo(Math.log10(floor), 10);
    expect(toLog(0.001, floor)).toBeCloseTo(Math.log10(floor), 10);
  });
});
//=============================================================================
describe('buildLogTicks', () => {
  test('returns powers of 10 in range [1, 100]', () => {
    const ticks = buildLogTicks(1, 100);
    expect(ticks).toContain(1);
    expect(ticks).toContain(10);
    expect(ticks).toContain(100);
  });

  test('includes 2× and 5× minor ticks', () => {
    const ticks = buildLogTicks(1, 100);
    expect(ticks).toContain(2);
    expect(ticks).toContain(5);
    expect(ticks).toContain(20);
    expect(ticks).toContain(50);
  });

  test('returns sorted, deduplicated ticks', () => {
    const ticks = buildLogTicks(0.1, 1000);
    for (let i = 1; i < ticks.length; i++) {
      expect(ticks[i]).toBeGreaterThan(ticks[i - 1]);
    }
  });

  test('floors minVal <= 0 to LOG_FLOOR', () => {
    const ticks = buildLogTicks(-5, 100);
    expect(ticks.length).toBeGreaterThan(0);
    ticks.forEach((t) => expect(t).toBeGreaterThan(0));
  });

  test('handles range less than one decade', () => {
    const ticks = buildLogTicks(3, 7);
    expect(ticks.length).toBeGreaterThanOrEqual(1);
    ticks.forEach((t) => {
      expect(t).toBeGreaterThanOrEqual(3);
      expect(t).toBeLessThanOrEqual(7);
    });
  });
});
//=============================================================================
describe('logValueToPixelY', () => {
  const plotY = 10;
  const plotH = 200;

  test('maps maxVal to plotY (top of plot)', () => {
    const y = logValueToPixelY(100, 1, 100, plotY, plotH);
    expect(y).toBeCloseTo(plotY, 5);
  });

  test('maps minVal to plotY + plotH (bottom of plot)', () => {
    const y = logValueToPixelY(1, 1, 100, plotY, plotH);
    expect(y).toBeCloseTo(plotY + plotH, 5);
  });

  test('maps midpoint (10 in [1,100]) to middle of plot', () => {
    // log10(10)=1 is the midpoint between log10(1)=0 and log10(100)=2
    const y = logValueToPixelY(10, 1, 100, plotY, plotH);
    expect(y).toBeCloseTo(plotY + plotH / 2, 5);
  });

  test('floors zero to LOG_FLOOR (no NaN/Infinity)', () => {
    const y = logValueToPixelY(0, 1, 100, plotY, plotH);
    expect(Number.isFinite(y)).toBe(true);
  });

  test('floors negative values (no NaN/Infinity)', () => {
    const y = logValueToPixelY(-50, 1, 100, plotY, plotH);
    expect(Number.isFinite(y)).toBe(true);
  });

  test('returns plotY + plotH/2 when logRange is zero', () => {
    const y = logValueToPixelY(5, 5, 5, plotY, plotH);
    expect(y).toBeCloseTo(plotY + plotH / 2, 5);
  });
});
//=============================================================================
