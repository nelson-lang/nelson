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
  buildConnectionSignature,
  buildBlockPositionHash,
  canApplyWorkerRoutes,
} from '../../router/route-worker-guard.js';
import { nflowRpc, canUseNFlowRpc } from '../../app/native-rpc.js';
//=============================================================================
function hasNativeRouter(): boolean {
  return canUseNFlowRpc('nflow.router.routeWires');
}
//=============================================================================
async function routeAllNative(
  snapshot: any,
  worldW: number,
  worldH: number,
  timeLimitMs: number
): Promise<{ stale: boolean; routes?: any[] }> {
  if (!canUseNFlowRpc('nflow.router.routeWires')) {
    throw new Error('native router RPC not available');
  }
  const payload = JSON.stringify({ snapshot, width: worldW, height: worldH, timeLimitMs });
  const resultJson = await nflowRpc<string>('nflow.router.routeWires', { payload });
  const raw = typeof resultJson === 'string' ? JSON.parse(resultJson) : resultJson;
  if (!raw.ok) throw new Error(raw.error || 'native_router_failed');
  return { stale: false, routes: raw.routes };
}
//=============================================================================
export function makeRouteWorker({ svg, state }: any) {
  let fullRouteSeq = 0;
  let latestFullRouteApplied = 0;

  const buildRoutingSnapshot = () => ({
    blocks: Array.from(state.blocks.values()).map((block: any) => ({
      id: block.id,
      x: Number(block.x) || 0,
      y: Number(block.y) || 0,
      width: Number(block.width) || 0,
      height: Number(block.height) || 0,
      rotation: Number(block.rotation) || 0,
      ports: (block.ports || []).map((port: any) => ({
        x: Number(port.x) || 0,
        y: Number(port.y) || 0,
        type: port.type,
        index: Number(port.index) || 0,
      })),
    })),
    connections: state.connections.map((conn: any) => ({
      from: conn.from,
      to: conn.to,
      fromIndex: Number(conn.fromIndex ?? 0),
      toIndex: Number(conn.toIndex ?? 0),
      // Previous route (pixel space); feeds route hysteresis in the native
      // router; a third element `1` marks pinned (user-anchored) points,
      // which make the wire STATIC for the router.
      points: Array.isArray(conn.points)
        ? conn.points
            .filter((p: any) => Number.isFinite(p?.x) && Number.isFinite(p?.y))
            .map((p: any) => (p.pin ? [Number(p.x), Number(p.y), 1] : [Number(p.x), Number(p.y)]))
        : undefined,
    })),
  });

  const routeAllInWorker = async (timeLimitMs: number): Promise<any> => {
    const snapshot = buildRoutingSnapshot();
    const jobId = ++fullRouteSeq;
    const jobEpoch = Number(state.routeEpoch || 0);
    const jobSignature = buildConnectionSignature(snapshot.connections);
    const jobPositionHash = buildBlockPositionHash(snapshot.blocks);
    const worldW = Number(svg.dataset.worldWidth) || svg.clientWidth || 1;
    const worldH = Number(svg.dataset.worldHeight) || svg.clientHeight || 1;

    const validateAndReturn = (routes: any[]) => {
      const currentEpoch = Number(state.routeEpoch || 0);
      const currentSignature = buildConnectionSignature(state.connections);
      const currentPositionHash = buildBlockPositionHash(state.blocks.values());
      const validForCurrentState = canApplyWorkerRoutes({
        jobEpoch,
        currentEpoch,
        jobSignature,
        currentSignature,
        jobPositionHash,
        currentPositionHash,
        routes,
        connectionCount: state.connections.length,
      });
      if (jobId < latestFullRouteApplied || !validForCurrentState) {
        return { stale: true };
      }
      latestFullRouteApplied = jobId;
      return { stale: false, routes };
    };

    // Require the native router RPC; do not fall back to a Web Worker.
    if (!hasNativeRouter()) {
      throw new Error('native router RPC not available');
    }

    const nativeResult = await routeAllNative(snapshot, worldW, worldH, timeLimitMs);
    if (!nativeResult.stale && Array.isArray(nativeResult.routes)) {
      return validateAndReturn(nativeResult.routes);
    }
    return { stale: true };
  };

  return { routeAllInWorker };
}
//=============================================================================
