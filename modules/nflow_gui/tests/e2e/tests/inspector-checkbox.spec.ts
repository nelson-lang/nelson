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

for (const width of [180, 240, 340]) {
  test(`Bus Creator checkbox wraps and remains usable at ${width}px`, async ({
    mockPage: page,
  }) => {
    await new AppPage(page).open();
    await page.locator('#fileOpenInput').setInputFiles({
      name: 'checkbox.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(
        JSON.stringify({
          format: 'nflow-2',
          name: 'Checkbox layout',
          autoRoute: false,
          blocks: [
            {
              id: 'bus',
              type: 'busCreator',
              x: 150,
              y: 150,
              params: { Inputs: 2, MemberNames: ['x', 'y'], NonVirtual: true },
            },
          ],
          connections: [],
        })
      ),
    });
    await page.locator('#blockLayer > [data-block-id="bus"]').click();
    const checkbox = page.getByRole('checkbox', {
      name: 'Nonvirtual (struct at the generated interface)',
      exact: true,
    });
    await expect(checkbox).toBeVisible();
    await page.locator('.inspector-pane').evaluate((el, value) => {
      el.style.width = `${value}px`;
      el.style.minWidth = `${value}px`;
      el.style.maxWidth = `${value}px`;
    }, width);
    await expect
      .poll(async () => Math.round((await page.locator('.inspector-pane').boundingBox())!.width))
      .toBe(width);
    const overflow = await page.locator('.inspector-capability-row').evaluateAll((rows) =>
      rows.map((row) => {
        const r = row.getBoundingClientRect();
        const value = row.querySelector('.inspector-capability-value')!.getBoundingClientRect();
        return value.right - r.right;
      })
    );
    expect(overflow.length).toBeGreaterThan(0);
    expect(Math.max(...overflow)).toBeLessThanOrEqual(1);
    if (width === 180) {
      const nameInput = page.locator('.inspector-name-input');
      expect((await nameInput.boundingBox())!.width).toBeGreaterThan(130);
      await expect(page.locator('.inspector-capability-name').first()).toHaveCSS(
        'white-space',
        'normal'
      );
      const clipped = await page
        .locator('.inspector-capability-name')
        .evaluateAll((labels) =>
          labels.some(
            (label) =>
              label.scrollWidth > label.clientWidth || label.scrollHeight > label.clientHeight
          )
        );
      expect(clipped).toBe(false);
    }
    const label = checkbox.locator('..');
    const text = label.locator('.param-name');
    const textBox = await text.boundingBox();
    const checkBox = await checkbox.boundingBox();
    const rowBox = await label.boundingBox();
    expect(textBox).not.toBeNull();
    expect(checkBox).not.toBeNull();
    expect(rowBox).not.toBeNull();
    expect(textBox!.x + textBox!.width).toBeLessThanOrEqual(checkBox!.x - 4);
    expect(checkBox!.x + checkBox!.width).toBeLessThanOrEqual(rowBox!.x + rowBox!.width);
    await expect(text).toHaveCSS('white-space', 'normal');
    if (width === 180) expect(textBox!.height).toBeGreaterThan(20);
    await expect(checkbox).toBeChecked();
    await text.click();
    await expect(checkbox).not.toBeChecked();
    const saved = JSON.parse(
      await page.evaluate(() =>
        (window as unknown as { __nf_serializeDiagram: () => string }).__nf_serializeDiagram()
      )
    );
    expect(saved.blocks[0].params.NonVirtual).toBe(false);
    await page.screenshot({ path: `temp/inspector-checkbox-${width}.png` });
  });
}
