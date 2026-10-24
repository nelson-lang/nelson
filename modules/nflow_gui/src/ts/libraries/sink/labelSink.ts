//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { Block, Helpers } from '../types.js';
//=============================================================================
export function labelSink(block: Block, helpers: Helpers): void {
  const { createSvgElement, renderLabelNode } = helpers;
  const showNode = block.params?.ShowNode !== false;
  if (renderLabelNode) {
    renderLabelNode(block, block.params?.GotoTag, { showNode });
    // Fallback circle when the helper is a mock or appended no label node.
    if (showNode) {
      try {
        const existing = (block.group as Element).querySelector('circle.label-node');
        if (!existing) {
          const bw =
            typeof block.width === 'number' && Number.isFinite(block.width)
              ? Number(block.width)
              : 40;
          const cxVal = Math.floor(bw / 2);
          const cxAttr = String(cxVal);
          const cyAttr = String(
            Math.floor(
              (typeof block.height === 'number' && Number.isFinite(block.height)
                ? Number(block.height)
                : 40) / 2
            )
          );
          if (createSvgElement) {
            block.group.appendChild(
              createSvgElement('circle', { cx: cxAttr, cy: cyAttr, r: 5, class: 'label-node' })
            );
          } else if (
            typeof document !== 'undefined' &&
            typeof document.createElementNS === 'function'
          ) {
            const ns = 'http://www.w3.org/2000/svg';
            const c = document.createElementNS(ns, 'circle');
            c.setAttribute('cx', cxAttr);
            c.setAttribute('cy', cyAttr);
            c.setAttribute('r', '5');
            c.setAttribute('class', 'label-node');
            block.group.appendChild(c as unknown as Element);
          }
        }
      } catch {}
    }
  } else {
    // No helper: draw a circle node like the default renderer.
    if (showNode) {
      try {
        const bw =
          typeof block.width === 'number' && Number.isFinite(block.width)
            ? Number(block.width)
            : 40;
        const cxVal = Math.floor(bw / 2);
        const cxAttr = String(cxVal);
        const cyAttr = String(
          Math.floor(
            (typeof block.height === 'number' && Number.isFinite(block.height)
              ? Number(block.height)
              : 40) / 2
          )
        );
        if (createSvgElement) {
          block.group.appendChild(
            createSvgElement('circle', { cx: cxAttr, cy: cyAttr, r: 5, class: 'label-node' })
          );
        } else if (
          typeof document !== 'undefined' &&
          typeof document.createElementNS === 'function'
        ) {
          const ns = 'http://www.w3.org/2000/svg';
          const c = document.createElementNS(ns, 'circle');
          c.setAttribute('cx', cxAttr);
          c.setAttribute('cy', cyAttr);
          c.setAttribute('r', '5');
          c.setAttribute('class', 'label-node');
          block.group.appendChild(c as unknown as Element);
        }
      } catch {}
    }
  }
}
//=============================================================================
export default labelSink;
//=============================================================================
