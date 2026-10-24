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
export function labelSource(block: Block, helpers: Helpers): void {
  const { createSvgElement, renderLabelNode } = helpers;
  if (renderLabelNode) {
    renderLabelNode(block, block.params?.GotoTag);
  }
  block.group.appendChild(
    createSvgElement!('line', { x1: 25, y1: 20, x2: 40, y2: 20, class: 'label-node' })
  );
}
//=============================================================================
export default labelSource;
//=============================================================================
