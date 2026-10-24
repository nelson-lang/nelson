//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Block } from './inspector.js';
import type { BlockPanelDeps } from './inspector-block.js';
//=============================================================================
export type Renderer = (block: Block, deps: BlockPanelDeps) => boolean;
const renderers: Renderer[] = [];
export function registerRenderer(r: Renderer): void {
  renderers.push(r);
}
//=============================================================================
export function renderWithRegistry(block: Block, deps: BlockPanelDeps): boolean {
  for (const r of renderers) {
    try {
      if (r(block, deps)) return true;
    } catch (e) {
      // swallow renderer errors to avoid breaking inspector
    }
  }
  return false;
}
//=============================================================================
export function clearRenderers(): void {
  renderers.length = 0;
}
//=============================================================================
