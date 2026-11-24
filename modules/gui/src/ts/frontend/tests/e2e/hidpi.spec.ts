//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test } from '@playwright/test';
//=============================================================================
test('SVG mask icons remain CSS-sized and aligned at the current DPR', async ({
  page,
  context,
}) => {
  await page.goto('/');
  await expect(page.getByRole('toolbar', { name: 'Main toolbar' })).toBeVisible();

  const icons = page.locator('.desktop-icon:visible');
  await expect(icons).not.toHaveCount(0);
  const icon = icons.first();
  const button = icon.locator('xpath=..');
  const metrics = await icon.evaluate((element) => {
    const style = getComputedStyle(element);
    const box = element.getBoundingClientRect();
    return {
      dpr: window.devicePixelRatio,
      width: box.width,
      height: box.height,
      mask: style.maskImage || style.webkitMaskImage,
    };
  });
  expect(metrics.width).toBe(20);
  expect(metrics.height).toBe(20);
  expect(metrics.mask).not.toBe('none');
  expect(metrics.dpr).toBeGreaterThanOrEqual(1);
  await expect
    .poll(() =>
      icon.evaluate(async (element) => {
        const mask = getComputedStyle(element).maskImage;
        const url = /^url\(["']?(.*?)["']?\)$/.exec(mask)?.[1];
        return url ? (await fetch(url)).ok : false;
      })
    )
    .toBe(true);

  const iconStates = await icons.evaluateAll(async (elements) =>
    Promise.all(
      elements.map(async (element) => {
        const style = getComputedStyle(element);
        const box = element.getBoundingClientRect();
        const url = /^url\(["']?(.*?)["']?\)$/.exec(style.maskImage)?.[1];
        return {
          width: box.width,
          height: box.height,
          tinted: style.backgroundColor === style.color,
          loaded: url ? (await fetch(url)).ok : false,
        };
      })
    )
  );
  expect(
    iconStates.every((state) => state.width === state.height && [16, 20].includes(state.width))
  ).toBe(true);
  expect(iconStates.every((state) => state.tinted && state.loaded)).toBe(true);
  expect(
    await icons.evaluateAll((elements) =>
      elements.every((element) => {
        const button = element.closest('button');
        if (!button) return true;
        const box = button.getBoundingClientRect();
        return box.width >= 32 && box.height >= 32;
      })
    )
  ).toBe(true);

  const scaleStates = await page.evaluate(() => {
    const source = document.querySelector<HTMLElement>('.desktop-icon');
    const shell = document.querySelector<HTMLElement>('.nelson-shell');
    if (!source || !shell) return [];
    const host = document.createElement('div');
    host.style.position = 'fixed';
    host.style.pointerEvents = 'none';
    host.style.opacity = '0.01';
    host.style.left = '0';
    host.style.bottom = '0';
    const url = source.style.getPropertyValue('--desktop-icon-url');
    shell.appendChild(host);
    const states = [16, 18, 20, 24].map((size) => {
      const sample = document.createElement('span');
      sample.className = 'desktop-icon';
      sample.style.setProperty('--desktop-icon-url', url);
      sample.style.width = `${size}px`;
      sample.style.height = `${size}px`;
      host.appendChild(sample);
      const box = sample.getBoundingClientRect();
      return {
        size,
        width: box.width,
        height: box.height,
        mask: getComputedStyle(sample).maskImage,
      };
    });
    host.remove();
    return states;
  });
  expect(scaleStates.map((state) => [state.width, state.height])).toEqual(
    [16, 18, 20, 24].map((size) => [size, size])
  );
  expect(scaleStates.every((state) => state.mask !== 'none')).toBe(true);

  const target = await button.boundingBox();
  expect(target).not.toBeNull();
  expect(target!.width).toBeGreaterThanOrEqual(32);
  expect(target!.height).toBeGreaterThanOrEqual(32);

  const railButton = page
    .getByRole('navigation', { name: 'Panels sidebar' })
    .getByRole('button', { name: /Workspace/ });
  const railTarget = await railButton.boundingBox();
  expect(railTarget!.width).toBeGreaterThanOrEqual(32);
  expect(railTarget!.height).toBeGreaterThanOrEqual(32);

  await page.getByRole('menuitem', { name: 'View' }).click();
  const menu = page.getByRole('menu').last();
  await expect(menu).toBeVisible();
  const menuBox = await menu.boundingBox();
  expect(menuBox!.x).toBeGreaterThanOrEqual(0);
  expect(menuBox!.y).toBeGreaterThanOrEqual(0);
  expect(menuBox!.x + menuBox!.width).toBeLessThanOrEqual(1000);
  expect(menuBox!.y + menuBox!.height).toBeLessThanOrEqual(700);
  await menu.getByRole('menuitemradio', { name: 'Dark' }).click();
  await expect(page.locator('.nelson-shell--dark')).toBeVisible();
  await expect(icon).toBeVisible();

  const filesGroup = page.locator('.dv-groupview').filter({ has: page.locator('.file-list') });
  await filesGroup.getByRole('button', { name: 'Panel actions' }).click();
  await page.getByRole('menu').last().getByRole('menuitem', { name: 'Float panel' }).click();
  const floating = page.locator('.dv-resize-container').filter({ has: page.locator('.file-list') });
  await expect(floating).toBeVisible();
  const floatingBox = await floating.boundingBox();
  expect(floatingBox!.width).toBeGreaterThan(300);
  expect(floatingBox!.height).toBeGreaterThan(250);

  await page.emulateMedia({ forcedColors: 'active', colorScheme: 'dark' });
  await expect(icon).toBeVisible();
  expect(await icon.evaluate((element) => getComputedStyle(element).backgroundColor)).not.toBe(
    'rgba(0, 0, 0, 0)'
  );

  const nextDpr = metrics.dpr === 2 ? 1 : 2;
  const session = await context.newCDPSession(page);
  await session.send('Emulation.setDeviceMetricsOverride', {
    width: 1000,
    height: 700,
    deviceScaleFactor: nextDpr,
    mobile: false,
  });
  await expect.poll(() => page.evaluate(() => window.devicePixelRatio)).toBe(nextDpr);
  const movedScreenIcon = await icon.boundingBox();
  expect(movedScreenIcon!.width).toBe(20);
  expect(movedScreenIcon!.height).toBe(20);

  await session.send('Emulation.setPageScaleFactor', { pageScaleFactor: 1.25 });
  await expect(icon).toBeVisible();
  const zoomedIcon = await icon.boundingBox();
  expect(zoomedIcon!.width / zoomedIcon!.height).toBeCloseTo(1, 5);
  expect(await icon.evaluate((element) => getComputedStyle(element).maskImage)).not.toBe('none');
});
//=============================================================================
