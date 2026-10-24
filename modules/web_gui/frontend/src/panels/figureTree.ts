//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { InspectorTreeNode } from '../rpc/figureTypes';
import { inspectorList } from '../rpc/figureTypes';
//=============================================================================
// Handle of the axes that holds an object of the browser tree (the axes itself included), 0
// when the object is not under an axes.
export function axesOfTreeNode(tree: InspectorTreeNode | null, target: number): number {
  const walk = (node: InspectorTreeNode, axes: number): number => {
    const own = node.type === 'axes' ? node.handle : axes;
    if (node.handle === target) return own;
    for (const child of inspectorList(node.children)) {
      const found = walk(child, own);
      if (found) return found;
    }
    return 0;
  };
  return tree ? walk(tree, 0) : 0;
}
//=============================================================================
