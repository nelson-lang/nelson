//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { computeLabelMathWidth, formatLabelTeX, renderLabelNode } from './block/label';
//=============================================================================
beforeEach(() => {
  // ensure no katex available so measureUserFuncTex returns null
  // @ts-ignore
  delete (global as any).katex;
});
//=============================================================================
test('formatLabelTeX handles names and latex', () => {
  expect(formatLabelTeX('')).toBe('');
  expect(formatLabelTeX('A')).toBe('A');
  expect(formatLabelTeX('ab')).toBe('\\mathsf{ab}');
  expect(formatLabelTeX('OUT_2')).toBe('\\mathsf{OUT}_2');
  // Multi-word underscore names must not produce LaTeX subscripts
  expect(formatLabelTeX('pi_plant_out')).toBe('\\mathsf{pi\\_plant\\_out}');
  expect(formatLabelTeX('p_plant_out')).toBe('\\mathsf{p\\_plant\\_out}');
  expect(formatLabelTeX('plant_out')).toBe('\\mathsf{plant\\_out}');
});
//=============================================================================
test('computeLabelMathWidth returns at least estimated width', () => {
  const w = computeLabelMathWidth('x', 10);
  expect(typeof w).toBe('number');
  expect(w).toBeGreaterThanOrEqual(10);
});
//=============================================================================
test('renderLabelNode appends math group and optional node', () => {
  const ns = 'http://www.w3.org/2000/svg';
  const group = document.createElementNS(ns, 'g');
  const block: any = { group, width: 120, height: 40 };
  renderLabelNode(block, 'abc', { showNode: true });
  expect(group.querySelector('.label-math')).not.toBeNull();
  expect(group.querySelector('circle.label-node')).not.toBeNull();
  const group2 = document.createElementNS(ns, 'g');
  const block2: any = { group: group2, width: 120, height: 40 };
  renderLabelNode(block2, 'abc', { showNode: false });
  expect(group2.querySelector('circle.label-node')).toBeNull();
});
//=============================================================================
