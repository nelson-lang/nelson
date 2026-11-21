//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Pure helpers for the "structural" blocks whose port count is driven by a
// parameter (mux/demux/sum/multiportSwitch). The inspector uses them to decide
// when a param edit must rebuild the block (so its ports follow) and which of
// its connections can survive the new port layout.
/* eslint-disable @typescript-eslint/no-explicit-any */
import { parseProductInputs } from './product-inputs.js';
//=============================================================================
export interface PortCounts {
  inputs: number;
  outputs: number;
}
export interface SimpleConnection {
  from: string;
  to: string;
  fromIndex: number;
  toIndex: number;
  label?: string;
  labelOffset?: { x: number; y: number };
  points?: Array<{ x: number; y: number; pin?: boolean }>;
}
//=============================================================================
const toInt = (value: unknown, min = 1): number => Math.max(min, Math.trunc(Number(value) || 0));
//=============================================================================
/** Port counts a block *should* have given its params, or null when the block
 *  type does not derive its ports from a parameter. */
export function desiredPortCounts(block: any): PortCounts | null {
  const type = String(block?.type ?? '');
  const params = (block?.params ?? {}) as Record<string, unknown>;
  switch (type) {
    case 'sum': {
      const raw = String(params.Inputs ?? '');
      const signs = raw.match(/[+-]/g);
      const count = /^\d+$/.test(raw.trim()) ? Number(raw) : signs?.length ?? 0;
      return { inputs: Math.max(1, count), outputs: 1 };
    }
    case 'busCreator': {
      const names = Array.isArray(params.MemberNames) ? params.MemberNames.length : 0;
      return {
        inputs: Math.max(2, Number.parseInt(String(params.Inputs ?? names), 10) || 2),
        outputs: 1,
      };
    }
    case 'busSelector': {
      const paths = Array.isArray(params.SelectedSignals) ? params.SelectedSignals : [];
      return { inputs: 1, outputs: params.OutputAsBus ? 1 : Math.max(1, paths.length) };
    }
    case 'trigFunction': {
      const fn = String(params.Function ?? 'sin');
      return { inputs: fn === 'atan2' ? 2 : 1, outputs: fn === 'sincos' ? 2 : 1 };
    }
    case 'mathFunction': {
      const fn = String(params.Function ?? 'exp');
      return { inputs: ['pow', 'hypot', 'rem', 'mod'].includes(fn) ? 2 : 1, outputs: 1 };
    }
    case 'mult':
      return {
        inputs:
          parseProductInputs(params.Inputs ?? 3, params.Multiplication === 'Matrix(*)')?.count ?? 3,
        outputs: 1,
      };
    case 'mux':
      return { inputs: toInt(params.Inputs), outputs: 1 };
    case 'demux':
      return { inputs: 1, outputs: toInt(params.Outputs) };
    case 'multiportSwitch':
      // One control input ("top") plus the data inputs.
      return { inputs: toInt(params.DataPortCount) + 1, outputs: 1 };
    default:
      return null;
  }
}
//=============================================================================
/** Port counts a block currently has rendered (from its `ports` array). */
export function actualPortCounts(block: any): PortCounts {
  const ports = Array.isArray(block?.ports) ? block.ports : [];
  let inputs = 0;
  let outputs = 0;
  for (const port of ports) {
    if (port?.type === 'in') inputs += 1;
    else if (port?.type === 'out') outputs += 1;
  }
  return { inputs, outputs };
}
//=============================================================================
/** True when a param-driven block's rendered ports no longer match its params. */
export function portCountsChanged(block: any): boolean {
  const want = desiredPortCounts(block);
  if (!want) return false;
  const have = actualPortCounts(block);
  return want.inputs !== have.inputs || want.outputs !== have.outputs;
}
//=============================================================================
/** Preserve connections to the same selected bus element when outputs are reordered. */
export function remapSelectorConnections(
  connections: SimpleConnection[],
  id: string,
  before: string[],
  after: string[],
  wasBus: boolean,
  isBus: boolean
): SimpleConnection[] {
  return connections.map((connection) => {
    if (connection.from !== id) return connection;
    if (wasBus !== isBus) return { ...connection, fromIndex: -1 };
    if (wasBus) return connection;
    const oldIndex = connection.fromIndex;
    const name = before[oldIndex];
    if (name === undefined) return { ...connection, fromIndex: -1 };
    const occurrence = before.slice(0, oldIndex).filter((value) => value === name).length;
    const candidates = after
      .map((value, index) => (value === name ? index : -1))
      .filter((index) => index >= 0);
    return { ...connection, fromIndex: candidates[occurrence] ?? -1 };
  });
}
//=============================================================================
/** The connections that still address a valid port after the block is rebuilt
 *  with `counts`. The far endpoint of each connection is left to the caller to
 *  validate (it is unchanged by this block's rebuild). */
export function survivingConnections(
  connections: SimpleConnection[],
  blockId: string,
  counts: PortCounts
): SimpleConnection[] {
  return connections.filter((conn) => {
    const fromOk =
      conn.from !== blockId ||
      (Number(conn.fromIndex) >= 0 && Number(conn.fromIndex) < counts.outputs);
    const toOk =
      conn.to !== blockId || (Number(conn.toIndex) >= 0 && Number(conn.toIndex) < counts.inputs);
    return fromOk && toOk;
  });
}
//=============================================================================
