//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Reorganization harness - .nflow format round-trip.
// Each bundled example is loaded through the real UI pipeline and saved
// again; the saved document must be semantically identical to the source
// (blocks, connections, labels, variables, timing). Locks the file format
// during refactors.
//=============================================================================
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const blocksDir = path.resolve(__dirname, '..', '..', '..', '..', 'nflow_blocks');
const examplesDir = path.join(blocksDir, 'examples');
// Loading fills parameters added to a block after the example was saved with
// the library default: the round-trip must keep every source value and may
// only add those defaults.
const DEFAULT_PARAMS = new Map<string, AnyRec>();
for (const lib of fs.readdirSync(path.join(blocksDir, 'libraries'))) {
  const file = path.join(blocksDir, 'libraries', lib, 'library.json');
  if (!fs.existsSync(file)) continue;
  for (const block of JSON.parse(fs.readFileSync(file, 'utf8')).blocks ?? []) {
    if (block?.type && block.defaultParams) DEFAULT_PARAMS.set(block.type, block.defaultParams);
  }
}
//=============================================================================
const EXAMPLES = [
  'causal/PID_Demo',
  'causal/complementary_filter',
  'causal/Logic_Gates_Demo',
  'dashboard/Dashboard_Gallery_Demo',
  'dashboard/Dashboard_Runtime_Demo',
];
//=============================================================================
type AnyRec = Record<string, any>;
function blockKey(b: AnyRec): string {
  return String(b.id);
}
//=============================================================================
function connKey(c: AnyRec): string {
  return `${c.from}:${c.fromIndex ?? 0}->${c.to}:${c.toIndex ?? 0}`;
}
//=============================================================================
async function loadExample(page: Page, file: string, expectedBlocks: number): Promise<void> {
  await page.evaluate(async () => {
    const p = (window as any).__nf_lib_ready_promise as Promise<unknown> | undefined;
    if (p && typeof p.then === 'function') {
      try {
        await p;
      } catch (_e) {
        /* surfaced later */
      }
    }
  });
  await page.locator('#fileOpenInput').setInputFiles(file);
  await expect(page.locator('#blockLayer > *')).toHaveCount(expectedBlocks, { timeout: 15_000 });
}
//=============================================================================
for (const name of EXAMPLES) {
  test(`round-trip: ${name}`, async ({ mockPage: page }) => {
    const sourceText = fs.readFileSync(path.join(examplesDir, `${name}.nflow`), 'utf8');
    const source = JSON.parse(sourceText) as AnyRec;

    const app = new AppPage(page);
    await app.open();
    await loadExample(page, path.join(examplesDir, `${name}.nflow`), source.blocks.length);

    // Capture the serialized document through the real save path.
    await page.evaluate(() => {
      (window as any).__captured_save = null;
      (window as any).__e2eFilePickerPath = 'C:/tmp/roundtrip.nflow';
      (window as any).__e2eWriteTextBackend = async (content: string, path: string) => {
        if (path.endsWith('/roundtrip.nflow')) (window as any).__captured_save = content;
        return JSON.stringify({ success: true });
      };
    });
    await page.locator('.menubar-item[data-menu="file"]').click();
    await page.locator('.menubar-item[data-action="saveAs"]').click();
    await page.waitForFunction(
      (expectedBlocks) => {
        try {
          return (
            JSON.parse((window as any).__captured_save || '').blocks?.length === expectedBlocks
          );
        } catch (_error) {
          return false;
        }
      },
      source.blocks.length,
      { timeout: 10_000 }
    );
    const savedText = await page.evaluate(() => (window as any).__captured_save as string);
    const saved = JSON.parse(savedText) as AnyRec;

    // ---- Blocks: same set, same type/position/rotation/params -------------
    const srcBlocks = new Map((source.blocks as AnyRec[]).map((b) => [blockKey(b), b]));
    const savBlocks = new Map((saved.blocks as AnyRec[]).map((b) => [blockKey(b), b]));
    expect([...savBlocks.keys()].sort()).toEqual([...srcBlocks.keys()].sort());
    for (const [id, src] of srcBlocks) {
      const sav = savBlocks.get(id)!;
      expect(sav.type, `${id}.type`).toBe(src.type);
      // Loading snaps positions to the 20px grid: tolerate half a grid step.
      expect(Math.abs(Number(sav.x) - Number(src.x)), `${id}.x`).toBeLessThanOrEqual(10);
      expect(Math.abs(Number(sav.y) - Number(src.y)), `${id}.y`).toBeLessThanOrEqual(10);
      expect(Number(sav.rotation ?? 0), `${id}.rotation`).toBe(Number(src.rotation ?? 0));
      const expected = { ...(DEFAULT_PARAMS.get(src.type) ?? {}), ...(src.params ?? {}) };
      expect(sav.params ?? {}, `${id}.params`).toEqual(expected);
    }

    // ---- Connections: same set (labels included) ---------------------------
    const srcConns = new Map((source.connections as AnyRec[]).map((c) => [connKey(c), c]));
    const savConns = new Map((saved.connections as AnyRec[]).map((c) => [connKey(c), c]));
    expect([...savConns.keys()].sort()).toEqual([...srcConns.keys()].sort());
    for (const [key, src] of srcConns) {
      const sav = savConns.get(key)!;
      expect(sav.label ?? undefined, `${key}.label`).toEqual(src.label ?? undefined);
    }

    expect(saved.name).toBe(source.name);
    expect(String(saved.variables ?? '')).toBe(String(source.variables ?? ''));
    expect(Number(saved.sampleTime)).toBeCloseTo(Number(source.sampleTime), 9);
    expect(Number(saved.runtime)).toBeCloseTo(Number(source.runtime), 9);
  });
}
//=============================================================================
