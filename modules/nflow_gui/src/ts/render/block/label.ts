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
import { createSvgElement, renderTeXMath, measureUserFuncTex } from '../svg-elements.js';
import { estimateLatexWidth } from '../../utils/expr.js';
//=============================================================================
export function computeLabelMathWidth(tex: string, blockWidth: number) {
  let width = Math.max(blockWidth, estimateLatexWidth(tex) + 12);
  const measured = measureUserFuncTex(tex);
  if (measured && Number.isFinite(measured.w)) {
    // measureUserFuncTex returns CSS px from an offscreen div. Use it as is to avoid
    // renderer-local scale helpers.
    width = Math.max(width, Math.ceil(measured.w + 12));
  }
  return width;
}
//=============================================================================
export function renderLabelNode(block: any, label: any, { showNode = true } = {}) {
  const group = block.group as Element;
  const tex = formatLabelTeX(label);
  const mathWidth = computeLabelMathWidth(tex, block.width);
  const offsetX = (block.width - mathWidth) / 2;
  const mathGroup = createSvgElement('g', {
    class: 'label-math',
    transform: `translate(${offsetX},-24)`,
  });
  group.appendChild(mathGroup);
  renderTeXMath(mathGroup, tex, mathWidth, block.height);
  if (showNode) {
    group.appendChild(createSvgElement('circle', { cx: 20, cy: 20, r: 5, class: 'label-node' }));
  }
}
//=============================================================================
export function formatLabelTeX(label: any) {
  const text = String(label || '').trim();
  if (!text) return '';
  if (/^[A-Za-z][A-Za-z0-9_]*$/.test(text)) {
    const match = text.match(/^([A-Za-z][A-Za-z0-9]*)(.*)$/);
    if (!match) return `\\mathsf{${text.replace(/_/g, '\\_')}}`;
    const base = match[1];
    const rest = match[2] || '';
    if (!rest) {
      // Pure identifier: single letter is math-italic, multi-letter gets mathsf.
      return base.length === 1 ? base : `\\mathsf{${base}}`;
    }
    if (rest.startsWith('_') || rest.startsWith('^')) {
      const suffix = rest.slice(1);
      // Simple subscript/superscript: numeric (OUT_2) or single-char (OUT_x).
      if (/^[0-9]+$/.test(suffix) || (suffix.length === 1 && /[A-Za-z0-9]/.test(suffix))) {
        return `\\mathsf{${base}}${rest}`;
      }
      // Multi-word name like p_plant_out or pi_plant_out: escape underscores.
      return `\\mathsf{${text.replace(/_/g, '\\_')}}`;
    }
  }
  return text;
}
//=============================================================================
