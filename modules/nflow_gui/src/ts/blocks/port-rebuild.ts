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
      const signs = String(params.Inputs ?? '').match(/[+-]/g);
      return { inputs: Math.max(1, signs ? signs.length : 0), outputs: 1 };
    }
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
/** The connections that still address a valid port after the block is rebuilt
 *  with `counts`. The far endpoint of each connection is left to the caller to
 *  validate (it is unchanged by this block's rebuild). */
export function survivingConnections(
  connections: SimpleConnection[],
  blockId: string,
  counts: PortCounts
): SimpleConnection[] {
  return connections.filter((conn) => {
    const fromOk = conn.from !== blockId || Number(conn.fromIndex) < counts.outputs;
    const toOk = conn.to !== blockId || Number(conn.toIndex) < counts.inputs;
    return fromOk && toOk;
  });
}
//=============================================================================
