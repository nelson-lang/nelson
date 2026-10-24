//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Helpers to build the simulation diagram and scope map
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { evalExpression } from '../utils/expr.js';
import * as FieldNames from '../FieldNames.js';
import type { ScopeBlock } from './parse-results.js';
import {
  rewriteConnectionsToPhysicalWires,
  blockPinInfoFromManifest,
} from '../libraries/acausal/acausal-export.js';
import type { BlockManifest } from '../libraries/acausal/acausal-export.js';
//=============================================================================
export function buildDiagramAndScopeMap(
  state: any,
  runtimeInput?: HTMLInputElement | null,
  baseDir?: string | null
): {
  diagram: unknown;
  scopeBlockMap: Map<string, ScopeBlock>;
} {
  const _skipResolveKeys = new Set([
    FieldNames.kExpr,
    FieldNames.kCondition,
    FieldNames.kPath,
    FieldNames.kTimes,
    FieldNames.kValues,
    FieldNames.kName,
    FieldNames.kGotoTag,
    FieldNames.kOnLabel,
    FieldNames.kOffLabel,
    FieldNames.kSumInputs,
    FieldNames.kRelop,
    FieldNames.kOperator,
    FieldNames.kSubsystem,
    FieldNames.kExternalInputs,
    FieldNames.kExternalOutputs,
    'lastCsv',
    FieldNames.kIsExternalPort,
    FieldNames.kVariableName,
    FieldNames.kInterpolate,
    FieldNames.kOutputAfterFinalValue,
    FieldNames.kMaxDataPoints,
    FieldNames.kSaveFormat,
    FieldNames.kFcn,
  ]);
  const _cppVariables = state.variables || { pi: Math.PI, e: Math.E };
  const _resolveForCpp = (value: any, key: string) => {
    if (_skipResolveKeys.has(key)) return value;
    if (Array.isArray(value)) {
      return value.map((v) => {
        const out = evalExpression(v, _cppVariables);
        // A non-numeric element (a bus member name like "pos") evaluates to NaN:
        // keep it verbatim, or string-list params (MemberNames, SelectedSignals,
        // AssignedSignals) become [0, 0, ...] and the engine rejects them.
        return Number.isNaN(out) ? v : out;
      });
    }
    const out = evalExpression(value, _cppVariables);
    return Number.isNaN(out) ? value : out;
  };

  const blocks = Array.from((state.blocks || new Map()).values()).map((b: any) => {
    const raw = b.params || {};
    const params: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(raw)) params[k] = _resolveForCpp(v, k);
    return {
      id: b.id,
      type: b.type,
      inputs: b.inputs,
      outputs: b.outputs ?? 1,
      params,
    };
  });

  let connections = (state.connections || []).map((c: any) => ({
    from: c.from,
    to: c.to,
    fromIndex: c.fromIndex ?? 0,
    toIndex: c.toIndex ?? 0,
  }));

  // The engine (nflow_acausal_expand / nflow_planar_expand) only lowers a model
  // that carries `physicalWires`, so lift physical<->physical edges as the save
  // path does; otherwise Run sends un-lowered blocks (Inertia, ...) and gets
  // unsupported_block. No-op for a purely causal diagram.
  let physicalWires: unknown[] | undefined;
  try {
    const byId = new Map<string, unknown>();
    for (const b of (state.blocks || new Map()).values()) {
      byId.set(String((b as any).id), b);
    }
    const lookup = (blockId: string) =>
      blockPinInfoFromManifest(byId.get(blockId) as BlockManifest | undefined);
    const lifted = rewriteConnectionsToPhysicalWires(connections as any, lookup);
    if (lifted.physicalWires.length > 0) {
      connections = lifted.connections as any;
      physicalWires = lifted.physicalWires;
    }
  } catch {
    /* ignore: fall back to raw connections (causal path) */
  }

  const requestedDuration = Number((runtimeInput as HTMLInputElement | undefined)?.value);
  const duration =
    Number.isFinite(requestedDuration) && requestedDuration >= 0 ? requestedDuration : 10;

  const variables = state.variables || { pi: Math.PI, e: Math.E };

  // Directory of the .nflow file, to resolve relative asset paths (FMU
  // packages). Runtime only: absent for an unsaved diagram, never saved.
  const resolvedBaseDir = typeof baseDir === 'string' && baseDir.length > 0 ? baseDir : undefined;

  // Absent or "discrete" keeps the fixed-step engine; a variable-step name
  // routes to CVODES.
  const solver =
    typeof state.solver === 'string' && state.solver && state.solver !== 'discrete'
      ? state.solver
      : undefined;

  const diagram = {
    blocks,
    connections,
    ...(physicalWires ? { physicalWires } : {}),
    sampleTime: state.sampleTime ?? (variables as any).dt ?? (variables as any).sampleTime ?? 0.01,
    duration,
    variables,
    ...(typeof state.realTime === 'boolean' ? { realTime: state.realTime } : {}),
    ...(resolvedBaseDir ? { baseDir: resolvedBaseDir } : {}),
    ...(solver ? { solver } : {}),
    ...(solver && typeof state.relTol === 'number' ? { relTol: state.relTol } : {}),
    ...(solver && typeof state.absTol === 'number' ? { absTol: state.absTol } : {}),
    ...(solver && typeof state.maxStep === 'number' && state.maxStep > 0
      ? { maxStep: state.maxStep }
      : {}),
    // StopFcn: Nelson commands run when the simulation stops.
    ...(typeof state.stopFcn === 'string' && state.stopFcn.trim()
      ? { stopFcn: state.stopFcn }
      : {}),
  };

  const scopeBlockMap: Map<string, ScopeBlock> = new Map(
    Array.from((state.blocks || new Map()).values()).map((b: any) => [b.id, b as ScopeBlock])
  );

  return { diagram, scopeBlockMap };
}
/**
 * Flatten virtual mux/demux routing into direct connections.
 * Mirrors the C++ DiagramFlattener::flattenMuxDemux algorithm.
 */
export function flattenMuxDemux(diagram: any): any {
  if (!diagram || !Array.isArray(diagram.blocks) || !Array.isArray(diagram.connections))
    return diagram;

  const blocks: Record<string, any> = {};
  for (const b of diagram.blocks) blocks[b.id] = b;

  const inbound: Record<string, any[]> = {};
  const outbound: Record<string, any[]> = {};
  for (const c of diagram.connections || []) {
    const from = c.from;
    const to = c.to;
    outbound[from] = outbound[from] || [];
    inbound[to] = inbound[to] || [];
    outbound[from].push(c);
    inbound[to].push(c);
  }

  const newConnections: any[] = [];
  const seen = new Set<string>();

  for (const conn of diagram.connections || []) {
    const from = conn.from;
    const to = conn.to;
    const fromIndex = conn.fromIndex ?? 0;
    const toIndex = conn.toIndex ?? 0;
    const fromBlock = blocks[from];
    const toBlock = blocks[to];
    const isFromMux = fromBlock && fromBlock.type === 'mux';
    const isToDemux = toBlock && toBlock.type === 'demux';
    const isFromDemux = fromBlock && fromBlock.type === 'demux';

    if (isFromDemux) {
      let demuxInputSource = '';

      for (const inConn of inbound[from] || []) {
        if ((inConn.toIndex ?? 0) === 0) {
          demuxInputSource = inConn.from;
          break;
        }
      }

      // A `mux -> demux` pair is a pure pass-through: rewrite each demux output
      // to the matching mux input and drop the demux edge. When the demux is fed
      // by a real (non-mux) source, leave the edge in place so downstream
      // vector-expand can slice the source; dropping it would strand consumers.
      let demuxFedByMux = false;
      if (demuxInputSource) {
        const demuxSrcBlock = blocks[demuxInputSource];
        if (demuxSrcBlock && demuxSrcBlock.type === 'mux') {
          demuxFedByMux = true;
          let muxInputSource = '';
          let muxInputSourceIndex = 0;
          for (const muxInConn of inbound[demuxInputSource] || []) {
            if ((muxInConn.toIndex ?? 0) === fromIndex) {
              muxInputSource = muxInConn.from;
              muxInputSourceIndex = muxInConn.fromIndex ?? 0;
              break;
            }
          }

          if (muxInputSource) {
            const directConn = {
              from: muxInputSource,
              fromIndex: muxInputSourceIndex,
              to,
              toIndex,
            };
            const k = `${directConn.from}:${directConn.fromIndex}->${directConn.to}:${directConn.toIndex}`;
            if (!seen.has(k)) {
              seen.add(k);
              newConnections.push(directConn);
            }
            continue;
          }
        }
      }

      if (demuxFedByMux) continue;
    }

    if (isFromMux && isToDemux) continue;

    const key = `${from}:${fromIndex}->${to}:${toIndex}`;
    if (!seen.has(key)) {
      seen.add(key);
      newConnections.push(conn);
    }
  }

  const out = { ...diagram, connections: newConnections };
  return out;
}
//=============================================================================
