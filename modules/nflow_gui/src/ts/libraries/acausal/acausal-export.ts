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
// Export bridge (GUI side) for acausal blocks. The editor stores every connection as
// a directed edge { from, to, fromIndex, toIndex }, the shape the engine document
// uses. A connection between two PHYSICAL pins is really an undirected node join, so
// on export it is rewritten into the engine's `physicalWires` format consumed by
// nflow_acausal_expand / nflow_planar_expand. The rewrite is a pure function, so it is
// unit-testable headless: given the document and a per-block descriptor, it returns
// { blocks, connections (signal only), physicalWires }.
//=============================================================================
import {
  buildPhysicalWires,
  type PhysicalConnection,
  type PhysicalWire,
  pinKey,
} from './physical-wires';
//=============================================================================
export type EditorConnection = {
  from: string;
  to: string;
  fromIndex?: number;
  toIndex?: number;
};
//=============================================================================
// Per-block physical-pin descriptor: the pin name at a given OUTPUT-side and
// INPUT-side port index, or null when that port is a directed signal port.
// (The editor addresses a source by its output index and a sink by its input
// index; a physical pin appears on both sides since it is undirected.)
export type BlockPinInfo = {
  acausal: boolean;
  // pin name by port index on the "from" (output) side; null = signal port.
  fromPinName?: (index: number) => string | null;
  // pin name by port index on the "to" (input) side; null = signal port.
  toPinName?: (index: number) => string | null;
};
//=============================================================================
export type PinInfoLookup = (blockId: string) => BlockPinInfo | undefined;
export type ExportResult = {
  connections: EditorConnection[]; // directed signal connections that remain
  physicalWires: PhysicalWire[]; // undirected physical nets
};
//=============================================================================
// Resolve one endpoint to a physical pin key, or null if it is a signal port
// (or an unknown / non-acausal block).
function endpointPin(
  blockId: string,
  index: number | undefined,
  side: 'from' | 'to',
  lookup: PinInfoLookup
): string | null {
  const info = lookup(blockId);
  if (!info || !info.acausal) {
    return null;
  }
  const namer = side === 'from' ? info.fromPinName : info.toPinName;
  if (!namer) {
    return null;
  }
  const name = namer(index ?? 0);
  return name === null ? null : pinKey(blockId, name);
}
//=============================================================================
// Rewrite a document's connections: physical<->physical edges become
// physicalWires nets; every other edge stays a directed signal connection.
export function rewriteConnectionsToPhysicalWires(
  connections: EditorConnection[],
  lookup: PinInfoLookup
): ExportResult {
  const physical: PhysicalConnection[] = [];
  const signal: EditorConnection[] = [];
  for (const c of connections) {
    const a = endpointPin(c.from, c.fromIndex, 'from', lookup);
    const b = endpointPin(c.to, c.toIndex, 'to', lookup);
    if (a !== null && b !== null) {
      physical.push({ a, b });
    } else {
      signal.push(c);
    }
  }
  return {
    connections: signal,
    physicalWires: buildPhysicalWires(physical),
  };
}
//=============================================================================
// A block manifest as emitted by nflow_acausal_generate_library: `acausal`
// blocks carry `physicalPins` (undirected) plus optional signal `inputs` /
// `outputs`. This defines the port-index CONVENTION the renderer follows:
// physical pins occupy indices 0..P-1 on BOTH the from (output) and to (input)
// sides; directed signal ports are indexed after the physical pins on their
// respective side. A non-acausal block has no physical pins.
export type BlockManifest = {
  acausal?: boolean;
  physicalPins?: Array<{ name: string }>;
  inputs?: unknown[];
  outputs?: unknown[];
};
//=============================================================================
export function blockPinInfoFromManifest(manifest: BlockManifest | undefined): BlockPinInfo {
  if (!manifest || !manifest.acausal) {
    return { acausal: false };
  }
  const names = (manifest.physicalPins ?? []).map((p) => p.name);
  const nameAt = (i: number): string | null => (i >= 0 && i < names.length ? names[i] : null);
  return { acausal: true, fromPinName: nameAt, toPinName: nameAt };
}
//=============================================================================
// Build a PinInfoLookup for a document from a block-type -> manifest registry
// and the document's blocks (id -> type). This is what the save path uses.
export function pinInfoLookupFor(
  blocks: Array<{ id?: unknown; type?: unknown }>,
  manifestOf: (type: string) => BlockManifest | undefined
): PinInfoLookup {
  const typeById = new Map<string, string>();
  for (const b of blocks) {
    if (b.id !== undefined && typeof b.type === 'string') {
      typeById.set(String(b.id), b.type);
    }
  }
  return (blockId: string) => {
    const type = typeById.get(blockId);
    if (type === undefined) {
      return undefined;
    }
    return blockPinInfoFromManifest(manifestOf(type));
  };
}
//=============================================================================
// Whole-document convenience: returns a shallow-cloned document with the
// physical connections lifted into `physicalWires`. `blocks` is passed through
// untouched. When there are no physical wires the field is omitted so a purely
// causal diagram serialises exactly as before.
export function exportAcausalDocument<
  T extends { blocks: unknown; connections?: EditorConnection[] }
>(doc: T, lookup: PinInfoLookup): T & { physicalWires?: PhysicalWire[] } {
  const { connections, physicalWires } = rewriteConnectionsToPhysicalWires(
    doc.connections ?? [],
    lookup
  );
  const out: T & { physicalWires?: PhysicalWire[] } = {
    ...doc,
    connections,
  };
  if (physicalWires.length > 0) {
    out.physicalWires = physicalWires;
  } else {
    delete out.physicalWires;
  }
  return out;
}
//=============================================================================
