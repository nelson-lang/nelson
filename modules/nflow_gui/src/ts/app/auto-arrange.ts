//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Re-lays the whole diagram as a left-to-right layered flow and hands the wires
// back to the router. Only coordinates change, so simulation is unaffected.
//=============================================================================
import { computeAutoLayout } from '../utils/auto-layout.js';
import type { LayoutNode, LayoutEdge } from '../utils/auto-layout.js';
import { signalDiagramChanged } from './state.js';
import type { AppState } from './state.js';
//=============================================================================
const DEFAULT_WIDTH = 120;
const DEFAULT_HEIGHT = 60;
type RendererLike = {
  updateBlockTransform?: (block: unknown) => void;
  forceFullRoute?: (budgetMs?: number) => unknown;
  updateConnections?: (full?: boolean) => void;
};
//=============================================================================
type MutableBlock = { x: number; y: number; width?: number; height?: number };
/**
 * Re-arranges every block into a layered flow layout and triggers a full
 * re-route. Returns true when at least one block moved. The model is unchanged.
 */
export function autoArrangeDiagram(state: AppState | null, renderer: RendererLike | null): boolean {
  if (!state || !state.blocks || state.blocks.size < 2) return false;

  const blocks = state.blocks as Map<unknown, MutableBlock>;
  const nodes: LayoutNode[] = [];
  blocks.forEach((block, id) => {
    nodes.push({
      id: String(id),
      width: Number(block?.width) || DEFAULT_WIDTH,
      height: Number(block?.height) || DEFAULT_HEIGHT,
    });
  });

  const edges: LayoutEdge[] = [];
  for (const conn of state.connections || []) {
    const c = conn as { from?: unknown; to?: unknown };
    if (c.from === undefined || c.to === undefined) continue;
    edges.push({ from: String(c.from), to: String(c.to) });
  }

  const positions = computeAutoLayout(nodes, edges);
  if (!positions.size) return false;

  let moved = false;
  blocks.forEach((block, id) => {
    const p = positions.get(String(id));
    if (!p) return;
    if (block.x !== p.x || block.y !== p.y) {
      block.x = p.x;
      block.y = p.y;
      moved = true;
    }
    renderer?.updateBlockTransform?.(block);
    (state.dirtyBlocks as Set<unknown> | undefined)?.add(id);
  });

  if (!moved) return false;

  state.routingDirty = true;
  (state as { fastRouting?: boolean }).fastRouting = false;
  // Prefer a full A* route (also runs the separation/nudge pass); fall back to
  // an incremental refresh if the helper is unavailable.
  try {
    if (renderer?.forceFullRoute) void renderer.forceFullRoute(4000);
    else renderer?.updateConnections?.(true);
  } catch (_e) {
    /* routing is best-effort; positions are already applied */
  }

  signalDiagramChanged();
  try {
    window.dispatchEvent(new Event('diagramCommitted'));
  } catch (_e) {
    /* history/persistence hook optional in headless contexts */
  }
  return true;
}
//=============================================================================
