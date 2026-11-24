//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, type Locator, type Page } from '@playwright/test';
//=============================================================================
// Selects an object in the browser of the figure properties panel, opening the
// panel first when needed. An axes is listed under its title, as "axes (Surface)".
export async function selectInspectorObject(scope: Page | Locator, name: string) {
  const properties = scope.getByRole('button', { name: 'Properties', exact: true }).first();
  if ((await properties.getAttribute('aria-expanded')) !== 'true') await properties.click();
  const node = scope
    .getByRole('tree', { name: 'Object browser' })
    .first()
    .getByRole('button', { name, exact: true });
  await node.click();
  await expect(node).toHaveClass(/is-selected/);
  // the property grid loads the model of the new object: wait for it, not for the previous one
  const handle = await node.getAttribute('data-handle');
  await expect(scope.locator('.figure-inspector-list').first()).toHaveAttribute(
    'data-object',
    handle ?? ''
  );
}
//=============================================================================
// Runs an entry of the "More figure actions" menu of the figure toolbar.
export async function figureMenuAction(scope: Page | Locator, name: string) {
  await scope.getByRole('button', { name: 'More figure actions', exact: true }).first().click();
  await scope
    .getByRole('group', { name: 'More figure actions', exact: true })
    .getByRole('button', { name, exact: true })
    .click();
}
//=============================================================================
// Pixels that differ between two screenshots of the same size, with their bounding box.
export async function screenshotDiff(page: Page, before: Buffer, after: Buffer) {
  return page.evaluate(
    async ([a, b]) => {
      const load = (src: string) =>
        new Promise<HTMLImageElement>((resolve) => {
          const image = new Image();
          image.onload = () => resolve(image);
          image.src = src;
        });
      const [first, second] = await Promise.all([load(a), load(b)]);
      const canvas = document.createElement('canvas');
      canvas.width = first.width;
      canvas.height = first.height;
      const context = canvas.getContext('2d')!;
      context.drawImage(first, 0, 0);
      const left = context.getImageData(0, 0, canvas.width, canvas.height).data;
      context.drawImage(second, 0, 0);
      const right = context.getImageData(0, 0, canvas.width, canvas.height).data;
      const box = { count: 0, minX: canvas.width, minY: canvas.height, maxX: -1, maxY: -1 };
      for (let y = 0; y < canvas.height; y++)
        for (let x = 0; x < canvas.width; x++) {
          const k = (y * canvas.width + x) * 4;
          if (
            left[k] !== right[k] ||
            left[k + 1] !== right[k + 1] ||
            left[k + 2] !== right[k + 2]
          ) {
            box.count++;
            box.minX = Math.min(box.minX, x);
            box.minY = Math.min(box.minY, y);
            box.maxX = Math.max(box.maxX, x);
            box.maxY = Math.max(box.maxY, y);
          }
        }
      return box;
    },
    [
      `data:image/png;base64,${before.toString('base64')}`,
      `data:image/png;base64,${after.toString('base64')}`,
    ]
  );
}
//=============================================================================
