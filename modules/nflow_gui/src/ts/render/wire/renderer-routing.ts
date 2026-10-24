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
import {
  routeAllConnections,
  routeDirtyConnections,
  normalizeConnectionJunctions,
} from '../../router/router.js';
import { nudgeConnections } from '../../router/nudge.js';
import { physicalNetKeys } from '../../router/physical-net.js';
import { orderFanOutTurns } from '../../router/fan-order.js';
import { repairConnForDrag } from './wire-local-repair.js';
import type { RepairBaseline } from './wire-local-repair.js';
import type { RendererCtx, RendererState } from '../core/renderer-types.js';
//=============================================================================
interface ExtCtx {
  svg: SVGSVGElement | null;
  routeAfterPreviewScheduled?: boolean;
  routeHelpers?: { routeAllInWorker?: (timeLimitMs?: number) => Promise<any> };
  detectBlockOverlap?: (ids: string[]) => boolean;
  buildFastDragPathFromPorts?: (conn: any) => any;
  buildFallbackPathFromPorts?: (conn: any) => any;
  buildBlockAwareFallbackFromPorts?: (conn: any) => any;
  wireCrossesForeignBlock?: (conn: any, points?: any[]) => boolean;
  applyWirePaths?: (paths: Map<any, any>) => void;
  applyWirePathsFast?: (set: Set<any>) => void;
  updateSelectionBox?: () => void;
  refreshDebugLog?: () => void;
  updateWireCornerHandles?: () => void;
  computeDirtyConnections?: () => Set<any> | null;
  captureConnectionQuality?: (s: Set<any>) => Map<any, any> | null;
  evaluateDirtyRouteQuality?: (s: Set<any>, before: Map<any, any> | null) => string | null;
  countTurns?: (points: any[]) => number;
  segmentHitsAnyBlock?: (segments: any[], conn: any, issues: string[]) => boolean;
}
//=============================================================================
interface ExtState extends RendererState {
  isPanning?: boolean;
  isPinching?: boolean;
  routingDirty?: boolean;
  routingScheduled?: boolean;
  fastRouting?: boolean;
  dirtyConnections?: Set<any>;
  dirtyBlocks?: Set<string>;
  deferRouting?: boolean;
  deferRoutingIds?: Set<string>;
  autoRoute?: boolean | null;
  debugRouteMode?: string;
  debugRouteTimeLimit?: number;
  debugDirtySetSize?: number;
  debugDirtyConnections?: string[];
  debugFallbackFullRoute?: boolean;
  debugFallbackReason?: string;
  debugQualityIssue?: string;
  debugOverlapCount?: number;
}
//=============================================================================
/** Max ms for incremental (dirty-only) re-routing on the main thread. */
const SYNC_DIRTY_LIMIT_MS = 150;
/** Max ms for a full re-route when no worker is available (graceful fallback). */
const SYNC_FULL_LIMIT_MS = 300;
/** Max ms for a full re-route when offloaded to the worker. */
const WORKER_FULL_LIMIT_MS = 4000;
/** Pointer-quiet delay before a live (mid-drag) native route is attempted. */
const LIVE_ROUTE_DEBOUNCE_MS = 50;
/** Time budget for a live (mid-drag) native route. */
const LIVE_ROUTE_TIME_LIMIT_MS = 60;
/**
 * Above this connection count, live mid-drag routing is skipped. The bridge
 * overhead (~70-110 ms, ~170 ms at 300 wires) exceeds the 60 ms budget, so the
 * local repair stays as the drag feedback.
 */
const LIVE_ROUTE_MAX_CONNECTIONS = 300;
export function makeRoutingHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as ExtCtx;
  const S = ctx.state as unknown as ExtState;

  // Per-drag-session baselines for local repair. Cleared when a real route lands,
  // so the next drag deforms the freshly routed shape.
  let dragBaselines: WeakMap<object, RepairBaseline> | null = null;

  // Live (mid-drag) routing: after a short pointer pause the native router is asked
  // for real routes; they replace the repaired preview only when better.
  let liveRouteTimer: ReturnType<typeof setTimeout> | 0 = 0;
  let liveRouteInFlight = false;

  function cancelLiveRoute() {
    if (liveRouteTimer) {
      clearTimeout(liveRouteTimer);
      liveRouteTimer = 0;
    }
  }

  function scheduleLiveRoute() {
    if (!C.routeHelpers?.routeAllInWorker) return;
    if ((ctx.state.connections?.length ?? 0) > LIVE_ROUTE_MAX_CONNECTIONS) return;
    cancelLiveRoute();
    liveRouteTimer = setTimeout(() => {
      liveRouteTimer = 0;
      void runLiveRoute();
    }, LIVE_ROUTE_DEBOUNCE_MS);
  }

  /**
   * Per-connection anti-flicker acceptance: apply the routed shape only when
   * the repaired preview collides with a block, has strictly more bends, or
   * is degenerate. Applying also replaces the drag baseline so subsequent
   * pointer moves deform the routed shape (no snap).
   */
  function applyLiveRoutes(routes: any[]) {
    const routeList = Array.isArray(routes) ? routes : [];
    const applied = new Set<any>();
    ctx.state.connections.forEach((conn: any, idx: number) => {
      const routed = routeList[idx];
      if (!Array.isArray(routed) || routed.length < 2) return;
      const current = Array.isArray(conn.points) ? conn.points : [];
      let accept = current.length < 2;
      if (!accept && C.segmentHitsAnyBlock) {
        const segments = [];
        let valid = true;
        for (let i = 1; i < current.length; i += 1) {
          const a = current[i - 1];
          const b = current[i];
          if (a.x !== b.x && a.y !== b.y) {
            valid = false;
            break;
          }
          segments.push({ a, b, orientation: a.x === b.x ? 'V' : 'H' });
        }
        if (!valid) accept = true;
        else if (segments.length && C.segmentHitsAnyBlock(segments, conn, [])) accept = true;
      }
      if (!accept && C.countTurns) {
        accept = C.countTurns(routed) < C.countTurns(current);
      }
      if (!accept) return;

      conn.points = routed.map((p: any) => ({ x: p.x, y: p.y }));
      if (!dragBaselines) dragBaselines = new WeakMap();
      dragBaselines.set(conn, {
        points: conn.points.map((p: any) => ({ ...p })),
        fromPos: { ...conn.points[0] },
        toPos: { ...conn.points[conn.points.length - 1] },
      });
      applied.add(conn);
    });
    if (applied.size) C.applyWirePathsFast?.(applied);
  }

  async function runLiveRoute() {
    if (!S.fastRouting || liveRouteInFlight) return;
    if (!C.routeHelpers?.routeAllInWorker) return;
    liveRouteInFlight = true;
    try {
      const res = await C.routeHelpers.routeAllInWorker(LIVE_ROUTE_TIME_LIMIT_MS);
      // Discard when the drag ended (the drop-time route supersedes) or the
      // guard flagged the result stale (epoch / topology / block positions).
      if (!S.fastRouting) return;
      if (!res || res.stale || !Array.isArray(res.routes)) return;
      applyLiveRoutes(res.routes);
    } catch (_err) {
      // Native router unavailable; live routing silently disabled.
    } finally {
      liveRouteInFlight = false;
    }
  }

  function applyWorkerFullRoute(routes: any) {
    const routeList = Array.isArray(routes) ? routes : [];
    ctx.state.connections.forEach((conn, idx) => {
      const points = routeList[idx];
      if (!Array.isArray(points) || points.length < 2) {
        // The native router could not route this wire. Saved points may be stale (off-canvas
        // detours), so replace them with an orthogonal path from the current port positions.
        // Skip pinned wires: their anchors are user intent.
        const hasPins = Array.isArray(conn.points) && conn.points.some((p: any) => p?.pin);
        if (!hasPins) {
          // Prefer the block-aware fallback so the route does not cut through a block.
          const fallback =
            C.buildBlockAwareFallbackFromPorts?.(conn) ?? C.buildFallbackPathFromPorts?.(conn);
          if (Array.isArray(fallback) && fallback.length >= 2) conn.points = fallback;
        }
        return;
      }
      const localHasPins = Array.isArray(conn.points) && conn.points.some((p: any) => p?.pin);
      const routedHasPins = points.some((p: any) => p?.pin);
      // A pinned wire only accepts native results that carry the pin flags, otherwise
      // the user anchors would be stripped.
      if (localHasPins && !routedHasPins) return;
      conn.points = points.map((p: any) =>
        p.pin ? { x: p.x, y: p.y, pin: true } : { x: p.x, y: p.y }
      );
      // If the accepted route cuts through a foreign block, prefer a clean block-aware path.
      if (!localHasPins && C.wireCrossesForeignBlock?.(conn)) {
        const alt = C.buildBlockAwareFallbackFromPorts?.(conn);
        if (Array.isArray(alt) && alt.length >= 2 && !C.wireCrossesForeignBlock?.(conn, alt)) {
          conn.points = alt;
        }
      }
    });
    normalizeConnectionJunctions(ctx.state.connections);
    // Separation pass on native results. Branches of one physical net share a trunk on
    // purpose, so they are grouped by net key and never spread apart.
    const netKeys = physicalNetKeys(ctx.state.connections as any[], (ctx.state as any).blocks);
    nudgeConnections(
      ctx.state.connections as any[],
      ((ctx.state as any).blocks && typeof (ctx.state as any).blocks.values === 'function'
        ? (ctx.state as any).blocks.values()
        : []) as any,
      { netKeys }
    );
    orderFanOutTurns(
      ctx.state.connections as any[],
      ((ctx.state as any).blocks && typeof (ctx.state as any).blocks.values === 'function'
        ? (ctx.state as any).blocks.values()
        : []) as any
    );
    C.applyWirePaths?.(new Map());
    dragBaselines = null;
    cancelLiveRoute();
    S.routingDirty = false;
    if (S.dirtyBlocks) S.dirtyBlocks.clear();
    if (S.dirtyConnections) S.dirtyConnections.clear();
    C.updateSelectionBox?.();
    C.refreshDebugLog?.();
    C.updateWireCornerHandles?.();
  }

  function queueRouteAfterPreview() {
    if (C.routeAfterPreviewScheduled) return;
    C.routeAfterPreviewScheduled = true;
    requestAnimationFrame(() => {
      requestAnimationFrame(() => {
        C.routeAfterPreviewScheduled = false;
        updateConnections();
      });
    });
  }

  function clearDirtyState() {
    S.routingDirty = false;
    S.dirtyBlocks?.clear();
    S.dirtyConnections?.clear();
  }

  function finishRouting() {
    dragBaselines = null;
    cancelLiveRoute();
    clearDirtyState();
    C.updateSelectionBox?.();
    C.updateWireCornerHandles?.();
  }

  /**
   * Run a full route. Prefers the worker bridge (C.routeHelpers); falls back to a
   * time-capped synchronous call so the main thread is never blocked.
   */
  async function runFullRoute(worldW: number, worldH: number): Promise<Map<any, any>> {
    if (C.routeHelpers?.routeAllInWorker) {
      try {
        await C.routeHelpers.routeAllInWorker(WORKER_FULL_LIMIT_MS);
        // Worker applies results internally; return empty map as signal.
        return new Map();
      } catch (_err) {
        // Worker failed; fall through to synchronous fallback.
      }
    }
    return routeAllConnections(ctx.state, worldW, worldH, { x: 0, y: 0 }, SYNC_FULL_LIMIT_MS);
  }

  function runDirtyRoute(dirtySet: Set<any>, worldW: number, worldH: number): Map<any, any> {
    return routeDirtyConnections(
      ctx.state,
      worldW,
      worldH,
      { x: 0, y: 0 },
      dirtySet,
      SYNC_DIRTY_LIMIT_MS
    );
  }

  /**
   * Yield to the browser for one task boundary before expensive work.
   * MessageChannel resolves faster than setTimeout(fn, 0) in most engines.
   */
  function yieldToBrowser(): Promise<void> {
    return new Promise((resolve) => {
      const { port1, port2 } = new MessageChannel();
      port1.onmessage = () => resolve();
      port2.postMessage(null);
    });
  }

  // Runs outside rAF callbacks so heavy A* work never blocks a paint frame.

  let _asyncRoutePending = false;

  function scheduleAsyncRoute(force: boolean) {
    if (_asyncRoutePending) return;
    _asyncRoutePending = true;

    // Yield via a resolved Promise so the current rAF callback can finish painting first.
    Promise.resolve().then(() => {
      _asyncRoutePending = false;
      runAsyncRoute(force).catch(() => finishRouting());
    });
  }

  async function runAsyncRoute(force: boolean) {
    if (!force && !S.routingDirty) return;

    const worldW = Number(C.svg?.dataset.worldWidth) || (C.svg?.clientWidth ?? 1) || 1;
    const worldH = Number(C.svg?.dataset.worldHeight) || (C.svg?.clientHeight ?? 1) || 1;

    const needsFullRoute = ctx.state.connections.some(
      (conn) => !conn.points || conn.points.length < 2
    );
    let dirtySet: Set<any> | null = null;
    if (!needsFullRoute) dirtySet = C.computeDirtyConnections?.() ?? null;

    S.debugDirtySetSize = dirtySet ? dirtySet.size : 0;
    S.debugDirtyConnections = dirtySet
      ? Array.from(dirtySet).map(
          (conn) => `${conn.from}:${conn.fromIndex ?? 0}->${conn.to}:${conn.toIndex ?? 0}`
        )
      : [];

    // Case 1: incremental dirty route
    if (!needsFullRoute && dirtySet && dirtySet.size > 0) {
      const beforeQuality = C.captureConnectionQuality?.(dirtySet) ?? null;
      S.debugRouteMode = 'dirty';
      S.debugRouteTimeLimit = SYNC_DIRTY_LIMIT_MS;

      const paths = runDirtyRoute(dirtySet, worldW, worldH);
      C.applyWirePaths?.(paths);

      const dirtyRouter = typeof window !== 'undefined' ? (window as any).__routerLast : null;
      const dirtyFailures: any[] = dirtyRouter?.result?.failures || [];
      const dirtyFailedCount = Number(dirtyRouter?.result?.cost?.failed || 0);
      const dirtyTimedOut = dirtyFailures.some((f: any) => f?.reason === 'timeout');
      const dirtyFailed = dirtyTimedOut || dirtyFailedCount > 0 || dirtyFailures.length > 0;
      const dirtyQualityIssue = C.evaluateDirtyRouteQuality?.(dirtySet, beforeQuality) || '';
      S.debugQualityIssue = dirtyQualityIssue || '';

      if ((S.debugOverlapCount ?? 0) > 0 || dirtyQualityIssue || dirtyFailed) {
        // Quality insufficient; escalate to a full route after yielding, so the
        // dirty-route result can paint first.
        S.debugFallbackFullRoute = true;
        S.debugFallbackReason = dirtyTimedOut
          ? 'dirty route timeout'
          : dirtyFailed
          ? 'dirty route failed'
          : dirtyQualityIssue || 'wire overlap';
        S.debugRouteMode = 'fallback-full';
        S.debugRouteTimeLimit = WORKER_FULL_LIMIT_MS;

        await yieldToBrowser();

        const fullPaths = await runFullRoute(worldW, worldH);
        if (fullPaths.size > 0) C.applyWirePaths?.(fullPaths);
      } else {
        S.debugFallbackFullRoute = false;
        S.debugFallbackReason = '';
      }

      C.refreshDebugLog?.();

      // Case 2: full route required
    } else if (needsFullRoute || !dirtySet) {
      S.debugRouteMode = 'full';
      S.debugRouteTimeLimit = WORKER_FULL_LIMIT_MS;

      const paths = await runFullRoute(worldW, worldH);
      if (paths.size > 0) C.applyWirePaths?.(paths);

      S.debugFallbackFullRoute = false;
      S.debugFallbackReason = '';
      S.debugQualityIssue = '';
      C.refreshDebugLog?.();
    }
    // Case 3: dirtySet exists but is empty; nothing to do, fall through.

    finishRouting();
  }

  function updateConnections(force = false) {
    if (S.isPanning || S.isPinching) return;
    if (!force && !S.routingDirty) return;
    if (S.routingScheduled) return;
    S.routingScheduled = true;

    // Fast-routing path (drag preview): synchronous, no rAF needed.
    if (S.fastRouting) {
      S.routingScheduled = false;
      const dirtySet = new Set(S.dirtyConnections || []);
      if (S.dirtyBlocks && S.dirtyBlocks.size > 0) {
        ctx.state.connections.forEach((conn) => {
          if (dirtySet.has(conn)) return;
          if (
            (S.dirtyBlocks as Set<string>).has(conn.from) ||
            (S.dirtyBlocks as Set<string>).has(conn.to)
          )
            dirtySet.add(conn);
        });
      }
      if (!dragBaselines) dragBaselines = new WeakMap();
      dirtySet.forEach((conn) => {
        // Minimum-disturbance repair of the existing route; fall back to the
        // S/Z preview only when there is no valid baseline (fresh wire).
        const repaired = repairConnForDrag(
          ctx.state as any,
          conn,
          dragBaselines as WeakMap<any, any>
        );
        conn.points = repaired ?? C.buildFastDragPathFromPorts?.(conn);
      });
      C.applyWirePathsFast?.(dirtySet);
      scheduleLiveRoute();
      clearDirtyState();
      C.updateSelectionBox?.();
      C.updateWireCornerHandles?.();
      return;
    }

    // Full / dirty routing path. The rAF callback only runs cheap guard checks, then
    // hands off to scheduleAsyncRoute so the callback returns in under 1 ms.
    const runForced = force;
    requestAnimationFrame(() => {
      S.routingScheduled = false;
      if (S.isPanning || S.isPinching) return;
      if (!runForced && !S.routingDirty) return;

      if (!S.fastRouting && S.deferRouting && S.deferRoutingIds && S.deferRoutingIds.size > 0) {
        const ids = Array.from(S.deferRoutingIds);
        if (C.detectBlockOverlap?.(ids)) {
          clearDirtyState();
          C.updateSelectionBox?.();
          return;
        }
        S.deferRouting = false;
        S.deferRoutingIds.clear();
      }

      // Manual routing mode: nothing to compute.
      if (S.autoRoute === false) {
        S.debugRouteMode = 'manual';
        S.debugRouteTimeLimit = 0;
        C.applyWirePaths?.(new Map());
        C.refreshDebugLog?.();
        finishRouting();
        return;
      }

      scheduleAsyncRoute(runForced);
    });
  }

  return { applyWorkerFullRoute, queueRouteAfterPreview, updateConnections };
}
//=============================================================================
