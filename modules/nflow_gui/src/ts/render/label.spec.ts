//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/block/label.js');
  document.body.innerHTML = '';
});
//=============================================================================
test('formatLabelTeX handles empty and plain latex', () => {
  expect(mod.formatLabelTeX('')).toBe('');
  expect(mod.formatLabelTeX(null)).toBe('');
  expect(mod.formatLabelTeX('x')).toBe('x');
});
//=============================================================================
test('formatLabelTeX wraps multi-letter names in mathsf', () => {
  expect(mod.formatLabelTeX('OUT')).toBe('\\mathsf{OUT}');
  expect(mod.formatLabelTeX('Out2')).toBe('\\mathsf{Out2}');
  // keep subscripts
  expect(mod.formatLabelTeX('OUT_2')).toBe('\\mathsf{OUT}_2');
});
//=============================================================================
test('computeLabelMathWidth uses estimate and measured tex', () => {
  const w = mod.computeLabelMathWidth('x', 50);
  expect(typeof w).toBe('number');
  expect(w).toBeGreaterThanOrEqual(50);
});
//=============================================================================
test('renderLabelNode appends math group and optional node', () => {
  const group = document.createElement('g');
  const block: any = { group, width: 100, height: 40 };
  mod.renderLabelNode(block, 'OUT', { showNode: true });
  expect(group.querySelector('.label-math')).toBeTruthy();
  expect(group.querySelector('.label-node')).toBeTruthy();
  const group2 = document.createElement('g');
  const block2: any = { group: group2, width: 100, height: 40 };
  mod.renderLabelNode(block2, 'x', { showNode: false });
  expect(group2.querySelector('.label-math')).toBeTruthy();
  expect(group2.querySelector('.label-node')).toBeNull();
});
//=============================================================================
