//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
type CanApplyParams = {
  jobEpoch?: number;
  currentEpoch?: number;
  jobSignature?: string;
  currentSignature?: string;
  jobPositionHash?: string;
  currentPositionHash?: string;
  routes?: unknown[];
  connectionCount?: number;
};
/**
 * Build a compact hash of block positions (`id:x:y` joined). The connection
 * signature only covers topology; mid-drag route application additionally
 * needs positions to be unchanged between job start and result arrival.
 */
export function buildBlockPositionHash(blocks: Iterable<unknown>): string {
  const parts: string[] = [];
  for (const b of blocks || []) {
    if (!b || typeof b !== 'object') continue;
    const r = b as Record<string, unknown>;
    parts.push(`${String(r.id ?? '')}:${Number(r.x) || 0}:${Number(r.y) || 0}`);
  }
  return parts.join('|');
}
/**
 * Build a compact textual signature that describes connection topology.
 * Each connection is rendered as `from:fromIndex->to:toIndex` and
 * entries are joined with `|`.
 */
export function buildConnectionSignature(connections: unknown[]): string {
  if (!Array.isArray(connections)) return '';
  const parts: string[] = [];
  for (let i = 0; i < connections.length; i += 1) {
    const c = connections[i];
    if (!c || typeof c !== 'object') {
      parts.push('');
      continue;
    }
    const record = c as Record<string, unknown>;
    const left = record.from ?? '';
    const right = record.to ?? '';
    const leftIdx = Number(record.fromIndex ?? 0) || 0;
    const rightIdx = Number(record.toIndex ?? 0) || 0;
    parts.push(`${String(left)}:${leftIdx}->${String(right)}:${rightIdx}`);
  }
  return parts.join('|');
}
/**
 * Validate whether a worker-generated `routes` array can be applied to the
 * current state. Returns true when all checks (shape, epoch, signature)
 * indicate compatibility.
 */
export function canApplyWorkerRoutes(params: CanApplyParams): boolean {
  const p = params || {};
  const incoming = Array.isArray(p.routes) ? (p.routes as unknown[]) : null;
  if (!incoming) return false;

  const expectedCount = Number(p.connectionCount ?? NaN);
  if (!Number.isFinite(expectedCount) || expectedCount < 0) return false;
  if (incoming.length !== expectedCount) return false;

  const je = p.jobEpoch;
  const ce = p.currentEpoch;
  if (je !== undefined && ce !== undefined) {
    const nJe = Number(je);
    const nCe = Number(ce);
    if (Number.isFinite(nJe) && Number.isFinite(nCe) && nJe !== nCe) return false;
  }

  const jobSig = p.jobSignature;
  const curSig = p.currentSignature;
  if (typeof jobSig === 'string' && typeof curSig === 'string' && jobSig !== curSig) return false;

  const jobPos = p.jobPositionHash;
  const curPos = p.currentPositionHash;
  if (typeof jobPos === 'string' && typeof curPos === 'string' && jobPos !== curPos) return false;

  return true;
}
//=============================================================================
