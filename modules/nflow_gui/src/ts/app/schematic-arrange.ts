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
// Re-lays an ACAUSAL diagram as a circuit schematic (source left, series across
// a top rail, shunts/return along the bottom, reference at the bottom).
// Only coordinates and rotations change. Acausal wiring is by pin name, so
// simulation is unaffected. Falls back to the flow auto-arrange when the
// diagram has no physical pins.
//=============================================================================
import { buildPhysicalWires } from '../libraries/acausal/physical-wires.js';
import {
  computeSchematicLayout,
  type SchematicBlock,
  type SchematicModel,
  type PinSide,
} from '../libraries/acausal/schematic-layout.js';
import { signalDiagramChanged } from './state.js';
import type { AppState } from './state.js';
//=============================================================================
type RendererLike = {
  updateBlockTransform?: (block: unknown) => void;
  forceFullRoute?: (budgetMs?: number) => unknown;
  updateConnections?: (full?: boolean) => void;
};
//=============================================================================
type Port = {
  type?: string;
  name?: string;
  index?: number;
  domain?: string;
  x?: number;
  y?: number;
};
//=============================================================================
type MutableBlock = {
  id?: unknown;
  type?: string;
  x: number;
  y: number;
  width?: number;
  height?: number;
  rotation?: number;
  acausal?: boolean;
  ports?: Port[];
};
//=============================================================================
const sideOfPort = (p: Port, width: number, height: number): PinSide => {
  const x = Number(p.x) || 0;
  const y = Number(p.y) || 0;
  if (x <= 2) return 'left';
  if (x >= width - 2) return 'right';
  if (y <= 2) return 'top';
  if (y >= height - 2) return 'bottom';
  // Interior pin: fall back to the dominant axis from the block centre.
  return Math.abs(x - width / 2) >= Math.abs(y - height / 2)
    ? x < width / 2
      ? 'left'
      : 'right'
    : y < height / 2
    ? 'top'
    : 'bottom';
};
/** True when the diagram carries acausal physical pins. */
export function diagramHasPhysicalPins(state: AppState | null): boolean {
  if (!state || !state.blocks) return false;
  for (const block of (state.blocks as Map<unknown, MutableBlock>).values()) {
    if ((block.ports ?? []).some((p) => p.type === 'phys')) return true;
  }
  return false;
}
/**
 * Re-arranges an acausal diagram into a circuit schematic and triggers a full
 * re-route. Returns true when at least one block moved or rotated. The model
 * itself is unchanged.
 */
export function schematicArrangeDiagram(
  state: AppState | null,
  renderer: RendererLike | null
): boolean {
  if (!state || !state.blocks || state.blocks.size < 2) return false;
  const blocks = state.blocks as Map<unknown, MutableBlock>;

  // Rebuild the physical nets from the current phys-to-phys connections
  // (union-find), the same lift the save path performs.
  const portName = (id: unknown, type: string, index: number): string | null => {
    const blk = blocks.get(id);
    const port = (blk?.ports ?? []).find((p) => p.type === type && p.index === index);
    return port && typeof port.name === 'string' ? port.name : null;
  };
  const isPhys = (id: unknown, index: number): boolean =>
    (blocks.get(id)?.ports ?? []).some((p) => p.type === 'phys' && p.index === index);

  const physConns: Array<{ a: string; b: string }> = [];
  for (const conn of state.connections || []) {
    const c = conn as { from?: unknown; to?: unknown; fromIndex?: number; toIndex?: number };
    if (c.from === undefined || c.to === undefined) continue;
    const fi = c.fromIndex ?? 0;
    const ti = c.toIndex ?? 0;
    if (!isPhys(c.from, fi) || !isPhys(c.to, ti)) continue;
    const an = portName(c.from, 'phys', fi);
    const bn = portName(c.to, 'phys', ti);
    if (an && bn) physConns.push({ a: `${String(c.from)}.${an}`, b: `${String(c.to)}.${bn}` });
  }
  const nets = buildPhysicalWires(physConns);

  const sBlocks: SchematicBlock[] = [];
  blocks.forEach((block, id) => {
    const width = Number(block.width) || 100;
    const height = Number(block.height) || 80;
    const ports = block.ports ?? [];
    const pins = ports
      .filter((p) => p.type === 'phys' && typeof p.name === 'string')
      .map((p) => ({
        name: p.name as string,
        side: sideOfPort(p, width, height),
        domain: String(p.domain ?? ''),
      }));
    sBlocks.push({
      id: String(id),
      type: String(block.type ?? ''),
      acausal: !!block.acausal || pins.length > 0,
      width,
      height,
      pins,
      hasSignalOut: ports.some((p) => p.type === 'out'),
      hasSignalIn: ports.some((p) => p.type === 'in'),
    });
  });

  const connections = (state.connections || [])
    .map((c) => c as { from?: unknown; to?: unknown })
    .filter((c) => c.from !== undefined && c.to !== undefined)
    .map((c) => ({ from: String(c.from), to: String(c.to) }));

  const model: SchematicModel = { blocks: sBlocks, physicalWires: nets, connections };
  const positions = computeSchematicLayout(model);
  if (!positions.size) return false;

  let moved = false;
  blocks.forEach((block, id) => {
    const p = positions.get(String(id));
    if (!p) return;
    if (block.x !== p.x || block.y !== p.y || (block.rotation ?? 0) !== p.rotation) moved = true;
    block.x = p.x;
    block.y = p.y;
    block.rotation = p.rotation;
    renderer?.updateBlockTransform?.(block);
    (state.dirtyBlocks as Set<unknown> | undefined)?.add(id);
  });
  if (!moved) return false;

  state.routingDirty = true;
  (state as { fastRouting?: boolean }).fastRouting = false;
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
