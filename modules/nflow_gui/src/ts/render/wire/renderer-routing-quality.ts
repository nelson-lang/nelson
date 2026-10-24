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
import { routeAllConnections, analyzeConnectionGeometry } from '../../router/router.js';
import { GRID_SIZE } from '../geometry.js';
import { blockBounds, segmentHitsRect } from '../geometry.js';
import type { RendererCtx, RendererState } from '../core/renderer-types.js';
//=============================================================================
const DIRTY_ROUTE_SCORE_WORSE_MARGIN = 8;
const DIRTY_ROUTE_SCORE_WORSE_RATIO = 1.2;
const FORCE_FULL_ROUTE_TIME_LIMIT_MS = 4000;
interface ExtCtx {
  svg: SVGSVGElement | null;
  routeHelpers?: { routeAllInWorker?: (timeLimitMs?: number) => Promise<any> };
  applyWirePaths?: (paths: Map<any, any>) => void;
  updateSelectionBox?: () => void;
  refreshDebugLog?: () => void;
  updateWireCornerHandles?: () => void;
}
//=============================================================================
export function makeRoutingQualityHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as ExtCtx;
  const _S = ctx.state as unknown as RendererState;
  void _S;

  function captureConnectionQuality(connectionSet: Set<any>) {
    const quality = new Map();
    (connectionSet || new Set()).forEach((conn) => {
      const pts = Array.isArray(conn?.points)
        ? conn.points.map((pt: any) => ({ x: Number(pt.x), y: Number(pt.y) }))
        : [];
      quality.set(conn, { score: routeQualityScore(pts), hasPath: pts.length >= 2 });
    });
    return quality;
  }

  function routeQualityScore(points: Array<{ x: number; y: number }>) {
    if (!Array.isArray(points) || points.length < 2) return Number.POSITIVE_INFINITY;
    let turnCount = 0;
    let prevDir: string | null = null;
    let length = 0;
    for (let i = 0; i < points.length - 1; i += 1) {
      const a = points[i];
      const b = points[i + 1];
      const dx = Math.abs((b?.x ?? 0) - (a?.x ?? 0));
      const dy = Math.abs((b?.y ?? 0) - (a?.y ?? 0));
      if (dx && dy) return Number.POSITIVE_INFINITY;
      const dir = dx ? 'H' : dy ? 'V' : null;
      if (dir) {
        if (prevDir && prevDir !== dir) turnCount += 1;
        prevDir = dir;
      }
      length += dx + dy;
    }
    return turnCount * 20 + length / GRID_SIZE;
  }

  function evaluateDirtyRouteQuality(dirtySet: Set<any>, beforeQuality: Map<any, any> | null) {
    const analysis = analyzeConnectionGeometry(ctx.state.connections, { ignoreSharedPorts: true });
    (ctx.state as any).debugQualityTotals = analysis?.totals || null;
    if (analysis?.totals?.nonOrthogonal > 0) return 'non-orthogonal segments';
    if (analysis?.totals?.selfCrosses > 0) return 'self-crossing wire';
    if (analysis?.totals?.overlaps > 0) return 'parallel wire overlap';

    const degraded: string[] = [];
    (dirtySet || new Set()).forEach((conn) => {
      const before = beforeQuality?.get(conn);
      if (!before?.hasPath || !Number.isFinite(before.score)) return;
      const afterScore = routeQualityScore(conn.points);
      if (!Number.isFinite(afterScore)) {
        degraded.push(`${conn.from}->${conn.to}:invalid`);
        return;
      }
      const worseByMargin = afterScore > before.score + DIRTY_ROUTE_SCORE_WORSE_MARGIN;
      const worseByRatio = afterScore > before.score * DIRTY_ROUTE_SCORE_WORSE_RATIO;
      if (worseByMargin && worseByRatio)
        degraded.push(`${conn.from}:${conn.fromIndex ?? 0}->${conn.to}:${conn.toIndex ?? 0}`);
    });
    if (degraded.length) return `degraded dirty route (${degraded.slice(0, 4).join(',')})`;
    return '';
  }

  async function forceFullRoute(timeLimitMs = FORCE_FULL_ROUTE_TIME_LIMIT_MS) {
    try {
      const result = await (ctx as any).routeHelpers.routeAllInWorker(timeLimitMs);
      if (!result?.stale && result?.routes) {
        (ctx as any).applyWorkerFullRoute(result.routes);
      }
      return;
    } catch (_error) {
      const worldW = Number(C.svg?.dataset.worldWidth) || (C.svg?.clientWidth ?? 1) || 1;
      const worldH = Number(C.svg?.dataset.worldHeight) || (C.svg?.clientHeight ?? 1) || 1;
      const paths = routeAllConnections(ctx.state, worldW, worldH, { x: 0, y: 0 }, timeLimitMs);
      C.applyWirePaths?.(paths);
      (ctx.state as any).routingDirty = false;
      if ((ctx.state as any).dirtyBlocks) (ctx.state as any).dirtyBlocks.clear();
      if ((ctx.state as any).dirtyConnections) (ctx.state as any).dirtyConnections.clear();
      C.updateSelectionBox?.();
      C.refreshDebugLog?.();
      C.updateWireCornerHandles?.();
    }
  }

  function computeDirtyConnections() {
    const dirty = new Set((ctx.state as any).dirtyConnections || []);
    if (!(ctx.state as any).dirtyBlocks || (ctx.state as any).dirtyBlocks.size === 0) return dirty;
    const movedBlocks = Array.from(((ctx.state as any).dirtyBlocks as Set<string>) || new Set())
      .map((id: string) => (ctx.state as any).blocks.get(id))
      .filter(Boolean);
    const keepouts = movedBlocks.map((block: any) => blockBounds(block));
    const movedIds = new Set((ctx.state as any).dirtyBlocks);
    ctx.state.connections.forEach((conn) => {
      if (dirty.has(conn)) return;
      if (movedIds.has(conn.from) || movedIds.has(conn.to)) {
        dirty.add(conn);
        return;
      }
      const points = conn.points || [];
      if (points.length < 2) {
        dirty.add(conn);
        return;
      }
      for (let i = 0; i < points.length - 1; i += 1) {
        const a = points[i];
        const b = points[i + 1];
        for (let j = 0; j < keepouts.length; j += 1) {
          if (segmentHitsRect(a, b, keepouts[j])) {
            dirty.add(conn);
            return;
          }
        }
      }
    });
    return dirty;
  }

  return {
    captureConnectionQuality,
    routeQualityScore,
    evaluateDirtyRouteQuality,
    forceFullRoute,
    computeDirtyConnections,
  };
}
//=============================================================================
