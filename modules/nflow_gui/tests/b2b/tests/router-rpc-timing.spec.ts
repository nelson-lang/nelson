//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// End-to-end timing of the native router RPC (serialization + webui bridge +
// routing under the live-drag 60 ms budget) at several diagram sizes. The
// printed numbers calibrate LIVE_ROUTE_MAX_CONNECTIONS in
// renderer-routing.ts; assertions are deliberately loose.
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
//=============================================================================
async function timedRouteRpc(
  page: Page,
  snapshot: unknown,
  timeLimitMs: number
): Promise<{ ms: number; ok: boolean }> {
  return page.evaluate(
    async ([snap, budget]) => {
      const webui = (window as any).webui;
      const payload = JSON.stringify({
        snapshot: snap,
        width: 4000,
        height: 3000,
        timeLimitMs: budget,
      });
      const t0 = performance.now();
      const raw = await webui.call(
        'rpc',
        JSON.stringify({
          id: 1,
          method: 'nflow.router.routeWires',
          params: { payload },
        })
      );
      const ms = performance.now() - t0;
      const parsed = JSON.parse(String(raw));
      const result =
        typeof parsed.result === 'string' ? JSON.parse(parsed.result) : parsed.result;
      return { ms, ok: Boolean(result?.ok) };
    },
    [snapshot, timeLimitMs] as const
  );
}
//=============================================================================
function makeSnapshot(cols: number, rows: number) {
  const blocks: unknown[] = [];
  const connections: unknown[] = [];
  const id = (r: number, c: number) => `g${r}_${c}`;
  for (let r = 0; r < rows; r += 1) {
    for (let c = 0; c < cols; c += 1) {
      blocks.push({
        id: id(r, c),
        x: 100 + c * 250,
        y: 100 + r * 150,
        width: 80,
        height: 40,
        rotation: 0,
        ports: [
          { type: 'in', index: 0, x: 0, y: 20 },
          { type: 'out', index: 0, x: 80, y: 20 },
        ],
      });
    }
  }
  let prev: string | null = null;
  for (let r = 0; r < rows; r += 1) {
    for (let c = 0; c < cols; c += 1) {
      if (prev) connections.push({ from: prev, to: id(r, c), fromIndex: 0, toIndex: 0 });
      prev = id(r, c);
    }
  }
  return { blocks, connections };
}
//=============================================================================
test.beforeEach(async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForFunction(() => typeof (window as any).webui !== 'undefined', undefined, {
    timeout: 20_000,
  });
  await page.waitForFunction(
    () => {
      const webui = (window as any).webui;
      return typeof webui?.isConnected !== 'function' || webui.isConnected();
    },
    undefined,
    { timeout: 20_000 }
  );
  await page.waitForSelector('#svgCanvas', { timeout: 20_000 });
});
//=============================================================================
test('router RPC round-trip cost at live-drag budget (calibration data)', async ({ page }) => {
  const sizes: Array<[string, number, number]> = [
    ['~50 wires', 10, 5],
    ['~150 wires', 15, 10],
    ['~300 wires', 20, 15],
  ];
  for (const [label, cols, rows] of sizes) {
    const snapshot = makeSnapshot(cols, rows);
    const runs: number[] = [];
    for (let i = 0; i < 3; i += 1) {
      const { ms, ok } = await timedRouteRpc(page, snapshot, 60);
      expect(ok).toBe(true);
      runs.push(Math.round(ms));
    }
    // eslint-disable-next-line no-console
    console.log(
      `[router rpc] ${label} (${(snapshot.connections as unknown[]).length} conns, budget 60ms): ${runs.join(
        ' / '
      )} ms`
    );
    // Loose ceiling: the 60 ms routing budget plus bridge overhead should
    // never approach a full second.
    expect(Math.min(...runs)).toBeLessThan(1000);
  }
});
//=============================================================================
