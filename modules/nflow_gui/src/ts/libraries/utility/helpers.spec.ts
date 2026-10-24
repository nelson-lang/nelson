//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  computeSubsystemPortLabelFrame,
  conditionToLatex,
  estimateLabelWidth,
  computePortYs,
  SUBSYSTEM_PORT_LABEL_EDGE_PAD,
} from './helpers';
//=============================================================================
describe('utility/helpers', () => {
  test('computeSubsystemPortLabelFrame calculates left and right frames', () => {
    const left = computeSubsystemPortLabelFrame({
      blockWidth: 200,
      blockHeight: 40,
      portY: 20,
      side: 'left',
    });
    expect(left.width).toBeGreaterThanOrEqual(48);
    expect(left.x).toBe(SUBSYSTEM_PORT_LABEL_EDGE_PAD);
    expect(typeof left.y).toBe('number');

    const right = computeSubsystemPortLabelFrame({
      blockWidth: 200,
      blockHeight: 40,
      portY: 20,
      side: 'right',
    });
    expect(right.x).toBeGreaterThan(left.x);
    expect(right.width).toBe(left.width);
  });

  test('conditionToLatex formats operators correctly', () => {
    expect(conditionToLatex(FieldNames.kGt, 5)).toBe('>\\!\\!5');
    expect(conditionToLatex(FieldNames.kNe, 'x')).toBe('\\ne\\!\\!x');
    expect(conditionToLatex(FieldNames.kGe, 0)).toBe('\\geq\\!\\!0');
  });

  test('estimateLabelWidth returns reasonable pixel widths', () => {
    expect(estimateLabelWidth('a', 7.5, 16)).toBeGreaterThanOrEqual(24);
    expect(estimateLabelWidth('', 7.5, 16)).toBeGreaterThanOrEqual(16);
    expect(estimateLabelWidth('longer text', 8, 10)).toBeGreaterThan(
      estimateLabelWidth('short', 8, 10)
    );
  });

  test('computePortYs edge cases and distribution', () => {
    expect(computePortYs(0, 100)).toEqual([]);
    expect(computePortYs(1, 100)).toEqual([50]);
    const three = computePortYs(3, 100);
    expect(three.length).toBe(3);
    expect(three[0]).toBeLessThan(three[1]);
    expect(three[2]).toBeGreaterThan(three[1]);
  });
});
//=============================================================================
describe('utility helpers', () => {
  test('computeSubsystemPortLabelFrame positions right side', () => {
    const frame = computeSubsystemPortLabelFrame({
      blockWidth: 200,
      blockHeight: 40,
      portY: 20,
      side: 'right',
    });
    expect(frame.width).toBeGreaterThanOrEqual(48);
    expect(frame.x).toBeLessThan(200);
  });

  test('conditionToLatex maps operators', () => {
    expect(conditionToLatex(FieldNames.kGt, 5)).toContain('>');
    expect(conditionToLatex(FieldNames.kNe, 3)).toContain('\\ne');
    expect(conditionToLatex('other', 2)).toContain('\\geq');
  });

  test('estimateLabelWidth computes reasonable width', () => {
    const w = estimateLabelWidth('abc', 7.5, 16);
    expect(w).toBeGreaterThanOrEqual(1);
  });

  test('computePortYs spacing', () => {
    expect(computePortYs(0, 100)).toEqual([]);
    expect(computePortYs(1, 100)).toEqual([50]);
    const three = computePortYs(3, 100);
    expect(three.length).toBe(3);
    expect(three[0]).toBeGreaterThanOrEqual(20);
  });
});
//=============================================================================
