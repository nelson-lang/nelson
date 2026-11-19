//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// N4 - Acausal editing experience. Acausal (physical) diagrams are nflow's
// differentiator over a plain causal block editor: components expose undirected
// physical pins and are wired node-to-node. This spec loads a shipped acausal
// circuit and asserts the editor renders that world distinctly -- physical pins
// with a domain, undirected physical wires, and component parameters in the
// inspector. Acausal *simulation* through the UI is already covered by the b2b
// examples walk-through (it recurses the acausal/ gallery), so this stays on the
// editor surface.
//=============================================================================
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { InspectorPanel } from '../pages/InspectorPanel.js';
//=============================================================================
const currentDir = path.dirname(fileURLToPath(import.meta.url));
const acausalDir = path.resolve(
  currentDir,
  '..',
  '..',
  '..',
  '..',
  'nflow_blocks',
  'examples',
  'acausal'
);
//=============================================================================
const RLC = path.join(acausalDir, 'Acausal_RLC_Circuit_Demo.nflow');
test.describe('N4 - acausal editor', () => {
  test('an acausal circuit loads and renders its blocks', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await page.locator('#fileOpenInput').setInputFiles(RLC);
    await expect(page.locator('#blockLayer > *')).toHaveCount(9, { timeout: 15_000 });
  });

  test('components expose undirected physical pins with a domain', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await page.locator('#fileOpenInput').setInputFiles(RLC);
    await expect(page.locator('#blockLayer > *')).toHaveCount(9, { timeout: 15_000 });

    const pins = page.locator('#blockLayer circle.physical-pin');
    await expect(pins.first()).toBeAttached({ timeout: 5000 });
    expect(await pins.count()).toBeGreaterThan(0);
    // A physical pin carries its physical domain (electrical here), which is what
    // makes acausal pins visually and semantically distinct from causal ports.
    await expect(pins.first()).toHaveAttribute('data-domain', /.+/);
  });

  test('physical wires render as undirected connections', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await page.locator('#fileOpenInput').setInputFiles(RLC);
    await expect(page.locator('#blockLayer > *')).toHaveCount(9, { timeout: 15_000 });

    // The RLC demo stores physical (node-to-node) wires; they render with the
    // physical-wire class and, unlike causal wires, without a direction arrow.
    const physicalWires = page.locator('#wireLayer path.physical-wire');
    await expect(physicalWires.first()).toBeAttached({ timeout: 5000 });
    expect(await physicalWires.count()).toBeGreaterThan(0);
  });

  test('dragging between two physical pins creates a new physical wire', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    await page.locator('#fileOpenInput').setInputFiles(RLC);
    await expect(page.locator('#blockLayer > *')).toHaveCount(9, { timeout: 15_000 });
    await expect(page.locator('#wireLayer path.physical-wire').first()).toBeAttached({
      timeout: 5000,
    });

    const before = await page.locator('#wireLayer path.physical-wire').count();

    // V1.p and C1.p are on different components, same (electrical) domain, and not
    // yet directly wired, so connecting them adds one undirected physical wire.
    const srcPin = page.locator(
      '#blockLayer [data-block-id="V1"] circle.physical-pin[data-pin="p"]'
    );
    const dstPin = page.locator(
      '#blockLayer [data-block-id="C1"] circle.physical-pin[data-pin="p"]'
    );
    const a = await srcPin.boundingBox();
    const b = await dstPin.boundingBox();
    if (!a || !b) throw new Error('missing physical pins');

    await page.mouse.move(a.x + a.width / 2, a.y + a.height / 2);
    await page.mouse.down();
    await page.mouse.move((a.x + b.x) / 2, (a.y + b.y) / 2 - 20, { steps: 6 });
    await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2, { steps: 10 });
    await page.mouse.up();

    await expect
      .poll(() => page.locator('#wireLayer path.physical-wire').count(), { timeout: 3000 })
      .toBe(before + 1);
  });

  test('selecting a component shows its parameters in the inspector', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    await page.locator('#fileOpenInput').setInputFiles(RLC);
    await expect(page.locator('#blockLayer > *')).toHaveCount(9, { timeout: 15_000 });

    const inspector = new InspectorPanel(page);
    await inspector.open();
    await page.locator('#blockLayer [data-block-id="R1"]').click();
    await expect(inspector.inspectorBody.locator('[data-edit]').first()).toBeVisible({
      timeout: 5000,
    });
  });
});
//=============================================================================
