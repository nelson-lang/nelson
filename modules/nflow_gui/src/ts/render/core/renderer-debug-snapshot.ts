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
import type { RendererCtx, Block } from './renderer-types.js';
import type { Point } from '../geometry.js';
import { GRID_SIZE } from '../geometry.js';
import { rotatePoint, getPortSide } from '../geometry.js';
import { hasInvalidPoint } from '../create-renderer.partial.js';
//=============================================================================
type DebugObstacle = { x0: number; y0: number; x1: number; y1: number; owner?: string };
export function makeDebugSnapshotHandlers(ctx: RendererCtx) {
  function buildDebugObstacles() {
    const PORT_RADIUS = 6;
    const obstacles: DebugObstacle[] = [];
    ctx.state.blocks.forEach((block: Block) => {
      const bounds = {
        left: block.x,
        right: block.x + (block.width ?? 0),
        top: block.y,
        bottom: block.y + (block.height ?? 0),
      };
      let { left, right, top, bottom } = bounds;
      (block.ports || []).forEach((port: any) => {
        const pos = rotatePoint({ x: block.x + port.x, y: block.y + port.y }, block as any);
        left = Math.min(left, pos.x - PORT_RADIUS);
        right = Math.max(right, pos.x + PORT_RADIUS);
        top = Math.min(top, pos.y - PORT_RADIUS);
        bottom = Math.max(bottom, pos.y + PORT_RADIUS);
      });
      obstacles.push({
        x0: Math.floor(left / GRID_SIZE),
        y0: Math.floor(top / GRID_SIZE),
        x1: Math.floor(right / GRID_SIZE),
        y1: Math.floor(bottom / GRID_SIZE),
        owner: (block as any).id,
      });
    });
    return obstacles;
  }

  function computeDebugObsNear(points: Point[] | undefined, obstacles: DebugObstacle[]) {
    let obs1 = 0;
    let obs2 = 0;
    if (!points || !points.length) return { obs1, obs2 };
    points.forEach((pt) => {
      const gx = Math.round(pt.x / GRID_SIZE);
      const gy = Math.round(pt.y / GRID_SIZE);
      let minDist = Infinity;
      obstacles.forEach((obs) => {
        const dx = gx < obs.x0 ? obs.x0 - gx : gx > obs.x1 ? gx - obs.x1 : 0;
        const dy = gy < obs.y0 ? obs.y0 - gy : gy > obs.y1 ? gy - obs.y1 : 0;
        minDist = Math.min(minDist, dx + dy);
      });
      if (minDist <= 1) obs1 += 1;
      else if (minDist <= 2) obs2 += 1;
    });
    return { obs1, obs2 };
  }

  function buildDebugSnapshot() {
    const summary: string[] = [];
    if ((globalThis as any).nflowDebugExtra) summary.push((globalThis as any).nflowDebugExtra);
    const viewBox = ctx.svg?.getAttribute('viewBox') || '';
    summary.push(`viewBox=${viewBox}`);
    summary.push(`svgSize=${ctx.svg?.clientWidth ?? '?'}x${ctx.svg?.clientHeight ?? '?'}`);
    summary.push(
      `world=${ctx.svg?.dataset.worldWidth || '?'}x${ctx.svg?.dataset.worldHeight || '?'}`
    );
    summary.push(`blocks=${ctx.state.blocks.size} connections=${ctx.state.connections.length}`);
    summary.push(
      `layers=blocks:${ctx.blockLayer?.childElementCount ?? 0} wires:${
        ctx.wireLayer?.childElementCount ?? 0
      }`
    );
    summary.push(
      `flags=pinch:${(ctx.state as any).isPinching} pan:${(ctx.state as any).isPanning} fast:${
        (ctx.state as any).fastRouting
      } dirty:${(ctx.state as any).routingDirty} sched:${(ctx.state as any).routingScheduled}`
    );
    try {
      const style = window.getComputedStyle(ctx.svg as Element);
      summary.push(
        `svgStyle=display:${style.display} visibility:${style.visibility} opacity:${style.opacity}`
      );
    } catch (err: any) {
      summary.push(`svgStyleError=${err?.message || err}`);
    }
    try {
      const style = window.getComputedStyle((globalThis as any).canvas);
      summary.push(
        `canvasStyle=display:${style.display} visibility:${style.visibility} opacity:${style.opacity}`
      );
    } catch (err: any) {
      summary.push(`canvasStyleError=${err?.message || err}`);
    }
    const canvasEl = document.getElementById('canvas');
    if (canvasEl) {
      const rect = canvasEl.getBoundingClientRect();
      summary.push(`canvasRect=${Math.round(rect.width)}x${Math.round(rect.height)}`);
    }
    const workspaceEl = document.querySelector('.workspace');
    if (workspaceEl) {
      const rect = workspaceEl.getBoundingClientRect();
      summary.push(`workspaceRect=${Math.round(rect.width)}x${Math.round(rect.height)}`);
    }
    try {
      const box = (ctx.svg as SVGSVGElement).getBBox();
      summary.push(
        `svgBBox=${Math.round(box.x)},${Math.round(box.y)} ${Math.round(box.width)}x${Math.round(
          box.height
        )}`
      );
    } catch (err: any) {
      summary.push(`svgBBoxError=${err?.message || err}`);
    }
    try {
      const box = (ctx.blockLayer as unknown as SVGGraphicsElement).getBBox();
      summary.push(
        `blockBBox=${Math.round(box.x)},${Math.round(box.y)} ${Math.round(box.width)}x${Math.round(
          box.height
        )}`
      );
    } catch (err: any) {
      summary.push(`blockBBoxError=${err?.message || err}`);
    }
    const nodeLines: string[] = [];
    ctx.state.blocks.forEach((block: any) => {
      block.ports.forEach((port: any) => {
        const raw = rotatePoint({ x: block.x + port.x, y: block.y + port.y }, block);
        const gridX = Math.round(raw.x / GRID_SIZE);
        const gridY = Math.round(raw.y / GRID_SIZE);
        const side = getPortSide(block, raw);
        const dir =
          side === 'left' ? 'left' : side === 'right' ? 'right' : side === 'top' ? 'up' : 'down';
        nodeLines.push(
          `${block.id} port:${port.type}${port.index} px=(${Math.round(raw.x)},${Math.round(
            raw.y
          )}) grid=(${gridX},${gridY}) dir=${dir}`
        );
      });
    });
    const router = (globalThis as any).__routerLast ?? null;
    if (router?.result) {
      summary.push(`routerDurationMs=${router.result.durationMs ?? '?'}`);
      summary.push(
        `routerWires=${router.result.wires?.size ?? '?'} failed=${
          router.result.failures?.length ?? 0
        }`
      );
      if (router.result.cost)
        summary.push(
          `routerCost=total:${router.result.cost.total} length:${router.result.cost.length} turns:${router.result.cost.turns} failed:${router.result.cost.failed}`
        );
    }
    if (typeof (ctx.state as any).debugDirtySetSize === 'number')
      summary.push(`dirtySetSize=${(ctx.state as any).debugDirtySetSize}`);
    if ((ctx.state as any).debugRouteMode)
      summary.push(`routeMode=${(ctx.state as any).debugRouteMode}`);
    if ((ctx.state as any).debugFallbackFullRoute) {
      summary.push('fallbackFullRoute=true');
      if ((ctx.state as any).debugFallbackReason)
        summary.push(`fallbackReason=${(ctx.state as any).debugFallbackReason}`);
    }
    const debugObstacles = buildDebugObstacles();
    const debugSettings = {
      lengthCost: router?.settings?.lengthCost ?? 1,
      turnCost: router?.settings?.turnCost ?? 6,
      hopCost: router?.settings?.hopCost ?? 20,
      nearWirePenalty1: router?.settings?.nearWirePenalty1 ?? 6,
      nearWirePenalty2: router?.settings?.nearWirePenalty2 ?? 2,
      nearObstaclePenalty1: router?.settings?.nearObstaclePenalty1 ?? 10,
      nearObstaclePenalty2: router?.settings?.nearObstaclePenalty2 ?? 4,
    };
    const wireCosts = new Map();
    if (router?.result?.wires)
      router.result.wires.forEach((wire: any, key: any) => wireCosts.set(key, wire.cost || {}));
    const wireLines: string[] = [];
    ctx.state.connections.forEach((conn: any) => {
      const fromIndex = conn.fromIndex ?? 0;
      const toIndex = conn.toIndex ?? 0;
      const key = `${conn.from}:${fromIndex}->${conn.to}:${toIndex}`;
      const points = conn.points || [];
      const gridPoints = points
        .map((pt: any) => `(${Math.round(pt.x / GRID_SIZE)},${Math.round(pt.y / GRID_SIZE)})`)
        .join(' ');
      const cost = wireCosts.get(key);
      const obsNear = computeDebugObsNear(points, debugObstacles);
      const obs1 = obsNear.obs1;
      const obs2 = obsNear.obs2;
      const total =
        (cost?.length ?? points.length - 1) * debugSettings.lengthCost +
        (cost?.turns ?? 0) * debugSettings.turnCost +
        obs1 * debugSettings.nearObstaclePenalty1 +
        obs2 * debugSettings.nearObstaclePenalty2;
      const basePoints = points.length ? points : ctx.buildFallbackPathFromPorts?.(conn) ?? [];
      const rendered = ctx.applyWireOffsets?.(conn, basePoints) ?? [];
      wireLines.push(
        `${conn.from} port:out${fromIndex} -> ${conn.to} port:in${toIndex} cost=${total} route=${gridPoints}`
      );
      wireLines.push(
        `  base=${!basePoints.length ? 'empty' : `count=${basePoints.length}`} render=${
          !rendered.length ? 'empty' : `count=${rendered.length}`
        } badRaw=${hasInvalidPoint(points)} badRender=${hasInvalidPoint(rendered)}`
      );
      if (conn.debugIssues && conn.debugIssues.length)
        wireLines.push(`  issues=${conn.debugIssues.join('|')}`);
    });
    return ['summary:', ...summary, 'nodes:', ...nodeLines, 'wires:', ...wireLines].join('\n');
  }

  return { buildDebugSnapshot, buildDebugObstacles, computeDebugObsNear };
}
//=============================================================================
