//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { RendererCtx, Conn } from './renderer-types.js';
import { segmentLengthStats } from '../geometry.js';
import { segmentsIntersect } from '../geometry.js';
//=============================================================================
interface DebugExtCtx {
  debugLog?: Element | null;
  buildDebugSnapshot?: () => string;
  checkPortDirections?: (
    conn: Conn,
    points: Array<{ x: number; y: number }>,
    issues: string[]
  ) => boolean;
  segmentHitsAnyBlock?: (
    segs: Array<{
      a: { x: number; y: number };
      b: { x: number; y: number };
      orientation: 'H' | 'V';
    }>,
    conn: Conn,
    issues: string[]
  ) => boolean;
  segmentOverlapsOtherWire?: (
    segs: Array<{
      a: { x: number; y: number };
      b: { x: number; y: number };
      orientation: 'H' | 'V';
    }>,
    conn: Conn,
    issues: string[]
  ) => boolean;
}
//=============================================================================
const DEBUG_WIRE_CHECKS = false;
export function makeDebugHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as DebugExtCtx;
  function refreshDebugLog() {
    if (!DEBUG_WIRE_CHECKS || !C.debugLog) return;
    try {
      C.debugLog.textContent = C.buildDebugSnapshot?.() ?? '';
    } catch (err: any) {
      C.debugLog.textContent = `Debug error: ${err?.message || err}`;
    }
  }

  function writeDebug(logEl: Element | null | undefined, text: string) {
    if (!logEl) return;
    if (text.startsWith('[wire')) return;
    logEl.textContent += `\n${text}`;
  }

  function formatWireIssue(conn: Conn, issues: string[]) {
    return `[wire ${conn.from}->${conn.to}] ${issues.join('; ')}`;
  }

  function formatWireDebugInfo(conn: Conn) {
    const parts: string[] = [];
    if ((conn as any).turnCheck) {
      parts.push(`turns=${(conn as any).turnCheck.actual}`);
      if (Number.isFinite((conn as any).turnCheck.minimal))
        parts.push(`min=${(conn as any).turnCheck.minimal}`);
    }
    const points = conn.points || [];
    if (points.length < 2) return parts.length > 0 ? ` (${parts.join(' ')})` : '';
    const stats = segmentLengthStats(points);
    parts.push(`len=${stats.length}`);
    parts.push(`seg1=${stats.seg1}`);
    parts.push(`seg2=${stats.seg2}`);
    parts.push(`shortPenalty=${stats.shortPenalty}`);
    return ` (${parts.join(' ')})`;
  }

  function checkWireIssues(conn: Conn, logEl?: Element | null) {
    const hardIssues: string[] = [];
    const softIssues: string[] = [];
    const points =
      conn.debugRenderPoints && conn.debugRenderPoints.length
        ? conn.debugRenderPoints
        : conn.points || [];
    if (points.length < 2) {
      hardIssues.push('not enough points');
      writeDebug(logEl, formatWireIssue(conn, hardIssues));
      return true;
    }
    if (!C.checkPortDirections?.(conn, points, hardIssues))
      hardIssues.push('invalid port direction');
    const segments: Array<{ a: { x: number; y: number }; b: { x: number; y: number } }> = [];
    for (let i = 0; i < points.length - 1; i += 1) {
      const a = points[i];
      const b = points[i + 1];
      if (a.x !== b.x && a.y !== b.y)
        hardIssues.push(`diagonal segment ${a.x},${a.y} -> ${b.x},${b.y}`);
      segments.push({ a, b });
    }
    const connSegments = segments.map((seg) => ({
      ...seg,
      orientation: (seg.a.x === seg.b.x ? 'V' : 'H') as 'V' | 'H',
    }));
    if (C.segmentHitsAnyBlock?.(connSegments, conn, hardIssues))
      hardIssues.push('crosses block keepout');
    if (C.segmentOverlapsOtherWire?.(connSegments, conn, hardIssues))
      hardIssues.push('overlaps another wire');
    if ((conn as any).turnCheck && Number.isFinite((conn as any).turnCheck.minimal)) {
      if ((conn as any).turnCheck.actual > (conn as any).turnCheck.minimal)
        softIssues.push(
          `extra turns (${(conn as any).turnCheck.actual} > ${(conn as any).turnCheck.minimal})`
        );
    }
    for (let i = 0; i < connSegments.length; i += 1) {
      for (let j = i + 2; j < connSegments.length; j += 1) {
        if (segmentsIntersect(connSegments[i], connSegments[j]))
          hardIssues.push(`self-cross segments ${i}/${j}`);
      }
    }
    conn.debugIssues = [...hardIssues, ...softIssues];
    const metrics = formatWireDebugInfo(conn);
    if (hardIssues.length === 0) {
      writeDebug(logEl, `[wire ${conn.from}->${conn.to}] OK${metrics}`);
      return false;
    }
    writeDebug(logEl, `${formatWireIssue(conn, [...hardIssues, ...softIssues])}${metrics}`);
    return true;
  }

  return { refreshDebugLog, writeDebug, formatWireIssue, formatWireDebugInfo, checkWireIssues };
}
//=============================================================================
