//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { conditionToLatex } from './helpers.js';
import { Block, Helpers } from '../types.js';
//=============================================================================
export function switchRenderer(block: Block, helpers: Helpers): void {
  const { createSvgElement, renderTeXMath } = helpers;
  const group = block.group as Element & { appendChild: (child: Node) => void };
  group.appendChild(
    createSvgElement!('rect', {
      x: 0,
      y: 0,
      width: block.width,
      height: block.height,
      class: 'nf-block',
    })
  );
  group.appendChild(
    createSvgElement!('line', { x1: 0, y1: 10, x2: 16, y2: 10, class: 'sum-line' })
  );
  group.appendChild(
    createSvgElement!('line', { x1: 0, y1: 70, x2: 16, y2: 70, class: 'sum-line' })
  );
  group.appendChild(
    createSvgElement!('line', { x1: 64, y1: 40, x2: 80, y2: 40, class: 'sum-line' })
  );
  group.appendChild(
    createSvgElement!('line', { x1: 16, y1: 10, x2: 64, y2: 40, class: 'sum-line' })
  );
  group.appendChild(createSvgElement!('circle', { cx: 16, cy: 10, r: 2.8, class: 'switch-dot' }));
  group.appendChild(createSvgElement!('circle', { cx: 16, cy: 70, r: 2.8, class: 'switch-dot' }));
  group.appendChild(createSvgElement!('circle', { cx: 64, cy: 40, r: 2.8, class: 'switch-dot' }));
  const mathGroup = createSvgElement!('g', {
    class: 'nf-switch-formula nf-switch-formula--l',
    transform: 'translate(0 17)',
  });
  group.appendChild(mathGroup);
  if (renderTeXMath) {
    const params = block.params as Record<string, unknown> | undefined;
    const condRaw = params?.Criteria;
    const thrRaw = params?.Threshold;
    const cond = typeof condRaw === 'string' ? condRaw : String(condRaw ?? '');
    const thr = typeof thrRaw === 'string' ? thrRaw : String(thrRaw ?? '');
    renderTeXMath(mathGroup, conditionToLatex(cond, thr), 48, 34);
  }
}
//=============================================================================
export default switchRenderer;
//=============================================================================
