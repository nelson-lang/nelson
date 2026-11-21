//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, type Page } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';

async function resize(page: Page, id: string, dx: number, dy: number) {
  const block = page.locator(`#blockLayer > [data-block-id="${id}"]`);
  await block.click();
  const handle = block.locator('.resize-handle');
  await expect(handle).toBeVisible();
  const box = await handle.boundingBox();
  if (!box) throw new Error('Resize handle has no bounding box');
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + dx, box.y + box.height / 2 + dy, { steps: 5 });
  await page.mouse.up();
  return block;
}

test('resized blocks keep their input and output ports on their edges', async ({
  mockPage: page,
}) => {
  await new AppPage(page).open();
  const model = {
    format: 'nflow-2',
    name: 'Resize ports',
    autoRoute: false,
    blocks: [
      { id: 'source', type: 'constant', x: 100, y: 130, params: { Value: 1 } },
      {
        id: 'bus',
        type: 'busCreator',
        x: 350,
        y: 130,
        params: { Inputs: 2, MemberNames: ['a', 'b'] },
      },
      { id: 'scope', type: 'scope', x: 650, y: 130, params: {} },
    ],
    connections: [
      { from: 'source', to: 'bus', fromIndex: 0, toIndex: 0 },
      { from: 'bus', to: 'scope', fromIndex: 0, toIndex: 0 },
    ],
  };
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'resize.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify(model)),
  });
  const wire = page.locator('#wireLayer .wire').first();
  const oldPath = await wire.getAttribute('d');
  const source = await resize(page, 'source', 80, 30);
  const sourceGeometry = await source.evaluate((block) => {
    const body = block.querySelector('rect.nf-block')!;
    const port = block.querySelector('.port-group[data-port-type="out"]')!;
    return {
      width: Number(body.getAttribute('width')),
      hit: Number(port.querySelector('.port-hit')!.getAttribute('cx')),
      points: port.querySelector('polygon')!.getAttribute('points')!,
    };
  });
  expect(sourceGeometry.hit).toBe(sourceGeometry.width);
  expect(Number(sourceGeometry.points.split(',')[0])).toBe(sourceGeometry.width - 6);
  await expect.poll(() => wire.getAttribute('d')).not.toBe(oldPath);
  const bus = await resize(page, 'bus', 70, 40);
  const geometry = await bus.evaluate((block) => {
    const body = block.querySelector('rect.nf-block')!;
    const ports = [...block.querySelectorAll('.port-group')];
    return {
      width: Number(body.getAttribute('width')),
      height: Number(body.getAttribute('height')),
      ports: ports.map((p) => ({
        type: p.getAttribute('data-port-type'),
        x: Number(p.querySelector('.port-hit')!.getAttribute('cx')),
        y: Number(p.querySelector('.port-hit')!.getAttribute('cy')),
        polygon: p.querySelector('polygon')?.getAttribute('points'),
      })),
    };
  });
  expect(geometry.width).toBeGreaterThan(80);
  expect(geometry.height).toBeGreaterThan(70);
  for (const port of geometry.ports) {
    expect(port.x).toBe(port.type === 'in' ? 0 : geometry.width);
    if (port.polygon) expect(Number(port.polygon.split(',')[0])).toBe(geometry.width - 6);
  }
  expect(geometry.ports.filter((p) => p.type === 'in').map((p) => p.y)).toEqual([
    geometry.height / 4,
    (geometry.height * 3) / 4,
  ]);
});

test('resizing a physical block moves its pins and preserves them on reload', async ({
  mockPage: page,
}) => {
  await new AppPage(page).open();
  const model = {
    format: 'nflow-2',
    name: 'Physical resize',
    autoRoute: false,
    blocks: [{ id: 'resistor', type: 'Resistor', x: 180, y: 160, params: {} }],
    connections: [],
  };
  await page
    .locator('#fileOpenInput')
    .setInputFiles({
      name: 'physical.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(JSON.stringify(model)),
    });
  const block = page.locator('#blockLayer > [data-block-id="resistor"]');
  const coords = async () =>
    block.locator('.physical-pin').evaluateAll((pins) =>
      pins.map((pin) => ({
        name: pin.getAttribute('data-pin'),
        x: Number(pin.getAttribute('cx')),
        y: Number(pin.getAttribute('cy')),
      }))
    );
  const before = await coords();
  expect(before.length).toBeGreaterThan(0);
  await resize(page, 'resistor', 70, 40);
  const after = await coords();
  expect(after).not.toEqual(before);
  const saved = await page.evaluate(() =>
    (window as unknown as { __nf_serializeDiagram: () => string }).__nf_serializeDiagram()
  );
  await page
    .locator('#fileOpenInput')
    .setInputFiles({
      name: 'saved-physical.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(saved),
    });
  expect(await coords()).toEqual(after);
});
