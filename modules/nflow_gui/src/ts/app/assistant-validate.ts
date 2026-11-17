//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Structural checks on a model the assistant just built, so a diagram that is
// syntactically valid but obviously wrong (an input port left dangling, a
// physical model on the wrong solver) is reported and handed back for correction
// instead of being drawn and declared done. The build's own error path only fires
// on a thrown engine error; these are the silent failures that run without one.
//
// Deliberately CONSERVATIVE: it flags only what is almost certainly a mistake for
// a freshly generated model, so it never nags about a legitimately optional port.
import { getLoadedLibraries } from './library-loader.js';
//=============================================================================
interface DiagramDoc {
  blocks?: Array<{ id?: unknown; type?: unknown }>;
  connections?: Array<{ to?: unknown }>;
  physicalWires?: Array<{ pins?: unknown }>;
  solver?: unknown;
}
//=============================================================================
interface RawBlock {
  type?: string;
  inputs?: unknown[];
  physicalPins?: unknown[];
}
//=============================================================================
interface BlockShape {
  inputs: number;
  pins: number;
}
//=============================================================================
// type -> port shape, from the libraries currently loaded in the editor (the same
// source the live block catalog uses). Empty when nothing is loaded; the caller
// then skips a type it cannot resolve rather than guessing.
function buildShapeLookup(): Map<string, BlockShape> {
  const map = new Map<string, BlockShape>();
  let libraries: Map<string, { library?: unknown }>;
  try {
    libraries = getLoadedLibraries() as Map<string, { library?: unknown }>;
  } catch {
    return map;
  }
  for (const entry of libraries.values()) {
    const blocks = (entry.library as { blocks?: RawBlock[] } | undefined)?.blocks;
    if (!Array.isArray(blocks)) {
      continue;
    }
    for (const block of blocks) {
      if (!block || typeof block.type !== 'string' || block.type === '') {
        continue;
      }
      map.set(block.type, {
        inputs: Array.isArray(block.inputs) ? block.inputs.length : 0,
        pins: Array.isArray(block.physicalPins) ? block.physicalPins.length : 0,
      });
    }
  }
  return map;
}
//=============================================================================
// The set of block ids that receive at least one signal connection.
function wiredInputs(doc: DiagramDoc): Set<string> {
  const wired = new Set<string>();
  const connections = Array.isArray(doc.connections) ? doc.connections : [];
  for (const conn of connections) {
    if (conn && typeof conn.to === 'string' && conn.to !== '') {
      wired.add(conn.to);
    }
  }
  return wired;
}
//=============================================================================
// The set of block ids that take part in at least one physical net (pin "id.pin").
function physicalParticipants(doc: DiagramDoc): Set<string> {
  const inNet = new Set<string>();
  const wires = Array.isArray(doc.physicalWires) ? doc.physicalWires : [];
  for (const wire of wires) {
    const pins = Array.isArray(wire?.pins) ? wire.pins : [];
    for (const pin of pins) {
      const id = String(pin).split('.')[0];
      if (id) {
        inNet.add(id);
      }
    }
  }
  return inNet;
}
//=============================================================================
// Human-readable structural issues, or [] when the model looks well-formed.
export function validateBuiltModel(doc: DiagramDoc | null | undefined): string[] {
  const issues: string[] = [];
  const blocks = doc && Array.isArray(doc.blocks) ? doc.blocks : [];
  if (!blocks.length) {
    return issues;
  }
  const shapes = buildShapeLookup();
  if (!shapes.size) {
    // No libraries to resolve ports against: do not guess, let the engine judge.
    return issues;
  }
  const wired = wiredInputs(doc as DiagramDoc);
  const inNet = physicalParticipants(doc as DiagramDoc);
  let hasPhysical = false;

  for (const block of blocks) {
    const type = typeof block?.type === 'string' ? block.type : '';
    const id = typeof block?.id === 'string' ? block.id : '';
    if (!type || !id) {
      continue;
    }
    const shape = shapes.get(type);
    if (!shape) {
      continue;
    }
    if (shape.pins > 0) {
      hasPhysical = true;
    }
    // A block with signal inputs but not a single incoming wire (and not joined
    // into a physical net) is almost always an unfinished model.
    if (shape.inputs > 0 && !wired.has(id) && !inNet.has(id)) {
      const ports = shape.inputs === 1 ? 'input port' : 'input ports';
      issues.push(`block "${id}" (${type}) has ${shape.inputs} ${ports} but none are connected`);
    }
  }

  const solver = typeof doc?.solver === 'string' && doc.solver ? doc.solver : 'discrete';
  if (hasPhysical && solver !== 'dae' && solver !== 'idas') {
    issues.push(
      `the model has physical/acausal blocks but Solver is "${solver}"; use Solver='dae'`
    );
  }
  return issues;
}
//=============================================================================
