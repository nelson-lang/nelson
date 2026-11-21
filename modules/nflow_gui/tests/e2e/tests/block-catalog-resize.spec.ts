//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

test('every library block can be resized and reloaded', async ({ mockPage: page }) => {
  test.setTimeout(90_000);
  const modules = path.resolve(
    path.dirname(fileURLToPath(import.meta.url)),
    '..',
    '..',
    '..',
    '..'
  );
  const root = path.join(modules, 'nflow_blocks', 'libraries');
  const blocks: { id: string; type: string; x: number; y: number; params: unknown }[] = [];
  for (const dir of fs.readdirSync(root)) {
    const file = path.join(root, dir, 'library.json');
    if (!fs.existsSync(file)) continue;
    const manifest = JSON.parse(fs.readFileSync(file, 'utf8'));
    for (const item of manifest.blocks || []) {
      if (!item.type) continue;
      blocks.push({
        id: `audit_${blocks.length}`,
        type: item.type,
        x: (blocks.length % 20) * 180,
        y: Math.floor(blocks.length / 20) * 160,
        params: item.defaultParams || {},
      });
    }
  }
  await new AppPage(page).open();
  await page
    .locator('#fileOpenInput')
    .setInputFiles({
      name: 'audit.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(
        JSON.stringify({
          format: 'nflow-2',
          name: 'Resize audit',
          autoRoute: false,
          blocks,
          connections: [],
        })
      ),
    });
  const nodes = page.locator('#blockLayer > [data-block-id]');
  await expect(nodes).toHaveCount(blocks.length, { timeout: 45_000 });
  const failures = await nodes.evaluateAll((elements) =>
    elements.flatMap((node) => {
      const handle = node.querySelector<SVGElement>(':scope > .resize-handle');
      if (!handle) return [node.getAttribute('data-block-id')];
      handle.setPointerCapture = () => {};
      const oldX = Number(handle.getAttribute('x'));
      for (const [name, x, y] of [
        ['pointerdown', 100, 100],
        ['pointermove', 180, 160],
        ['pointerup', 180, 160],
      ] as const)
        handle.dispatchEvent(
          new PointerEvent(name, { pointerId: 1, clientX: x, clientY: y, bubbles: true })
        );
      if (Number(handle.getAttribute('x')) <= oldX) return [node.getAttribute('data-block-id')];
      for (const port of node.querySelectorAll('.port-group')) {
        const hit = port.querySelector('.port-hit'),
          shape = port.querySelector('.port');
        if (!hit || !shape) continue;
        const x = Number(hit.getAttribute('cx')),
          y = Number(hit.getAttribute('cy'));
        if (shape.tagName.toLowerCase() === 'polygon') {
          const points = (shape.getAttribute('points') || '')
            .trim()
            .split(/\s+/)
            .map((p) => p.split(',').map(Number));
          if (Math.abs(points[0][0] - (x - 6)) > 0.001 || Math.abs(points[2][1] - y) > 0.001)
            return [node.getAttribute('data-block-id')];
        } else if (
          shape.tagName.toLowerCase() === 'rect' &&
          (Math.abs(Number(shape.getAttribute('x')) - (x - 6)) > 0.001 ||
            Math.abs(Number(shape.getAttribute('y')) - (y - 6)) > 0.001)
        )
          return [node.getAttribute('data-block-id')];
      }
      return [];
    })
  );
  expect(failures.map((id) => blocks.find((block) => block.id === id)?.type)).toEqual([]);
  const serialize = () =>
    page.evaluate(() =>
      (window as unknown as { __nf_serializeDiagram: () => string }).__nf_serializeDiagram()
    );
  const saved = await serialize();
  await page
    .locator('#fileOpenInput')
    .setInputFiles({
      name: 'saved-resize.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(saved),
    });
  await expect(nodes).toHaveCount(blocks.length, { timeout: 45_000 });
  const restored = JSON.parse(await serialize());
  const after = new Map<string, any>(restored.blocks.map((block: any) => [block.id, block]));
  const mismatches = JSON.parse(saved).blocks.filter((block: any) => {
    const other = after.get(block.id);
    return (
      !other ||
      Math.abs(Number(other.params?.width) - Number(block.params?.width)) > 0.001 ||
      Math.abs(Number(other.params?.height) - Number(block.params?.height)) > 0.001
    );
  });
  expect(mismatches.map((block: any) => block.type)).toEqual([]);
});
