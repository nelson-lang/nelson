//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Reorganization harness - numeric simulation characterization.
// Each bundled example is loaded through the real UI (file input), simulated
// on the real C++ engine (Ctrl+T through the real pipeline) and the scope
// series statistics are compared to frozen golden values with tolerances.
//
// To regenerate the goldens intentionally:
//   NFLOW_UPDATE_GOLDEN=1 npm run test:b2b
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const examplesDir = path.resolve(__dirname, '..', '..', '..', '..', 'nflow_blocks', 'examples');
const goldenDir = path.resolve(__dirname, '..', 'golden');
const UPDATE = process.env.NFLOW_UPDATE_GOLDEN === '1';
const EXAMPLES = ['PID_Demo', 'complementary_filter', 'Logic_Gates_Demo'];
test.setTimeout(180_000);
interface SeriesStats {
  n: number;
  last: number;
  min: number;
  max: number;
  mean: number;
}
//=============================================================================
interface ScopeStats {
  id: string;
  type: string;
  tEnd: number;
  series: SeriesStats[];
}
//=============================================================================
function statsOf(values: number[]): SeriesStats {
  const n = values.length;
  let min = Infinity;
  let max = -Infinity;
  let sum = 0;
  for (const v of values) {
    min = Math.min(min, v);
    max = Math.max(max, v);
    sum += v;
  }
  return {
    n,
    last: n ? values[n - 1] : NaN,
    min,
    max,
    mean: n ? sum / n : NaN,
  };
}
//=============================================================================
async function loadExample(page: Page, file: string): Promise<void> {
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas', { timeout: 20_000 });
  await page.waitForFunction(() => typeof (window as any).webui?.call === 'function', {
    timeout: 20_000,
  });
  await page.evaluate(async () => {
    const p = (window as any).__nf_lib_ready_promise as Promise<unknown> | undefined;
    if (p && typeof p.then === 'function') {
      try {
        await p;
      } catch (_e) {
        /* surfaced later by loadDiagram */
      }
    }
  });
  await page.locator('#fileOpenInput').setInputFiles(file);
  await page.waitForSelector('#blockLayer > *', { timeout: 20_000 });
}
//=============================================================================
for (const name of EXAMPLES) {
  test(`sim characterization: ${name}`, async ({ page }) => {
    await loadExample(page, path.join(examplesDir, 'causal', `${name}.nflow`));

    // Interactive demos run without real-time pacing: the goldens check numeric
    // outputs, not playback speed.
    const disableRealTimePacing = name === 'Logic_Gates_Demo';
    await page.waitForFunction(
      () => typeof (window as any).__nf_runSimulationForTest === 'function',
      undefined,
      { timeout: 20_000 }
    );
    const resultText = await page.evaluate((forceFastSimulation) =>
      (window as any).__nf_runSimulationForTest({
        realTime: forceFastSimulation ? false : undefined,
      }),
      disableRealTimePacing
    );

    const result = JSON.parse(resultText);
    expect(result.status, result.reason).toBe('done');
    expect(Array.isArray(result.scopes)).toBe(true);
    expect(result.scopes.length).toBeGreaterThan(0);

    const actual: ScopeStats[] = (result.scopes as any[])
      .map((s) => ({
        id: String(s.id),
        type: String(s.type ?? 'scope'),
        tEnd: Array.isArray(s.times) && s.times.length ? Number(s.times[s.times.length - 1]) : NaN,
        series: (Array.isArray(s.series) ? s.series : [])
          .filter((serie: unknown) => Array.isArray(serie))
          .map((serie: number[]) => statsOf(serie)),
      }))
      .sort((a, b) => a.id.localeCompare(b.id));

    const goldenPath = path.join(goldenDir, `sim-${name}.json`);
    if (UPDATE) {
      fs.mkdirSync(goldenDir, { recursive: true });
      fs.writeFileSync(goldenPath, JSON.stringify(actual, null, 2) + '\n');
      console.log(`golden updated: ${goldenPath}`);
      return;
    }
    expect(fs.existsSync(goldenPath), `missing golden ${goldenPath}`).toBe(true);
    const golden: ScopeStats[] = JSON.parse(fs.readFileSync(goldenPath, 'utf8'));

    expect(actual.length, 'scope count').toBe(golden.length);
    for (let i = 0; i < golden.length; i++) {
      const g = golden[i];
      const a = actual[i];
      expect(a.id).toBe(g.id);
      expect(a.type).toBe(g.type);
      expect(a.tEnd, `${g.id} tEnd`).toBeCloseTo(g.tEnd, 6);
      expect(a.series.length, `${g.id} series count`).toBe(g.series.length);
      for (let s = 0; s < g.series.length; s++) {
        const gs = g.series[s];
        const as = a.series[s];
        expect(as.n, `${g.id}[${s}].n`).toBe(gs.n);
        const tol = Math.max(1e-9, Math.abs(gs.max - gs.min) * 1e-6);
        expect(Math.abs(as.last - gs.last), `${g.id}[${s}].last`).toBeLessThan(tol);
        expect(Math.abs(as.min - gs.min), `${g.id}[${s}].min`).toBeLessThan(tol);
        expect(Math.abs(as.max - gs.max), `${g.id}[${s}].max`).toBeLessThan(tol);
        expect(Math.abs(as.mean - gs.mean), `${g.id}[${s}].mean`).toBeLessThan(tol);
      }
    }
  });
}
//=============================================================================
