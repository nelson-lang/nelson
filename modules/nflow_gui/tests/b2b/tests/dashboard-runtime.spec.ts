//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import path from 'node:path';
import fs from 'node:fs';
import { fileURLToPath } from 'node:url';
import { expect, test } from '@playwright/test';
//=============================================================================
const here = path.dirname(fileURLToPath(import.meta.url));
const model = path.resolve(here, '..', '..', 'dashboard', 'dashboard-runtime.nflow');
const repoRoot = path.resolve(here, '..', '..', '..', '..', '..');
const galleryExample = path.join(
  repoRoot,
  'modules',
  'nflow_blocks',
  'examples',
  'dashboard',
  'Dashboard_Gallery_Demo.nflow'
);
//=============================================================================
const runtimeExample = path.join(
  repoRoot,
  'modules',
  'nflow_blocks',
  'examples',
  'dashboard',
  'Dashboard_Runtime_Demo.nflow'
);
//=============================================================================
const dashboardContract = JSON.parse(
  fs.readFileSync(
    path.join(repoRoot, 'modules', 'nflow_blocks', 'libraries', 'dashboard', 'contract.json'),
    'utf8'
  )
) as {
  blocks: Array<{
    type: string;
    width: number;
    height: number;
    properties: Record<string, { default: unknown }>;
  }>;
};
//=============================================================================
const observerTypes = new Set([
  'dashboardScope',
  'dashboardDisplay',
  'dashboardGauge',
  'dashboardHalfGauge',
  'dashboardQuarterGauge',
  'dashboardLinearGauge',
  'dashboardLamp',
  'dashboardMultiStateImage',
]);
//=============================================================================
function completeDashboardModel(): Record<string, unknown> {
  const signalBinding = {
    kind: 'signal',
    blockPath: ['gain'],
    outputPortIndex: 1,
    frameProcessingMode: 'sample',
  };
  const parameterBinding = {
    kind: 'parameter',
    blockPath: ['gain'],
    paramName: 'Gain',
    element: '',
  };
  const dashboards = dashboardContract.blocks.map((definition, index) => {
    const params = Object.fromEntries(
      Object.entries(definition.properties).map(([name, property]) => [name, property.default])
    );
    if (observerTypes.has(definition.type)) params.Binding = signalBinding;
    else if (definition.type !== 'dashboardCallbackButton') params.Binding = parameterBinding;
    if (definition.type === 'dashboardCallbackButton') {
      params.ClickFcn = 'dashboard_callback_count = 1;';
      params.PressFcn = 'dashboard_press_count = 1;';
      params.PressDelay = 0;
    }
    return {
      id: `dashboard-${definition.type}`,
      type: definition.type,
      x: 60 + (index % 5) * 240,
      y: 320 + Math.floor(index / 5) * 220,
      width: definition.width,
      height: definition.height,
      rotation: 0,
      params,
    };
  });
  return {
    version: '2.0.0',
    format: 'nflow-2',
    name: 'dashboard-complete-runtime',
    blocks: [
      { id: 'source', type: 'constant', x: 60, y: 100, params: { Value: 2 } },
      { id: 'gain', type: 'gain', x: 280, y: 100, params: { Gain: 1 } },
      { id: 'legacy', type: 'display', x: 500, y: 100, params: {} },
      ...dashboards,
    ],
    connections: [
      { from: 'source', to: 'gain', fromIndex: 0, toIndex: 0 },
      {
        from: 'gain',
        to: 'legacy',
        fromIndex: 0,
        toIndex: 0,
      },
    ],
    variables: '',
    sampleTime: 0.05,
    runtime: 2,
    solver: 'discrete',
  };
}
//=============================================================================
async function manipulateAllControls(page: import('@playwright/test').Page): Promise<void> {
  await page.evaluate(() => {
    const group = (type: string) =>
      document.querySelector(`[data-block-id="dashboard-${type}"]`) as SVGElement;
    for (const type of [
      'dashboardKnob',
      'dashboardRotarySwitch',
      'dashboardRadioButton',
      'dashboardCheckBox',
      'dashboardRockerSwitch',
      'dashboardSliderSwitch',
      'dashboardToggleSwitch',
    ]) {
      group(type).dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true }));
    }
    const edit = group('dashboardEdit').querySelector('input') as HTMLInputElement;
    edit.value = '7';
    edit.dispatchEvent(new Event('change', { bubbles: true }));
    const combo = group('dashboardComboBox').querySelector('select') as HTMLSelectElement;
    combo.selectedIndex = Math.min(1, combo.options.length - 1);
    combo.dispatchEvent(new Event('change', { bubbles: true }));
    for (const type of ['dashboardPushButton', 'dashboardCallbackButton']) {
      const button = group(type);
      button.dispatchEvent(new PointerEvent('pointerdown', { button: 0, bubbles: true }));
      window.dispatchEvent(new PointerEvent('pointerup', { button: 0, bubbles: true }));
    }
    group('dashboardSlider').dispatchEvent(
      new KeyboardEvent('keydown', { key: 'End', bubbles: true })
    );
  });
}
//=============================================================================
async function setSliderNearMaximum(page: import('@playwright/test').Page): Promise<void> {
  const track = page.locator(
    '[data-block-id="dashboard-dashboardSlider"] .dashboard-control-track'
  );
  const box = await track.boundingBox();
  expect(box).not.toBeNull();
  const clientX = (box?.x ?? 0) + (box?.width ?? 1) * 0.9;
  const clientY = (box?.y ?? 0) + (box?.height ?? 0) / 2;
  await track.dispatchEvent('pointerdown', { button: 0, clientX, clientY });
  await page.evaluate(
    ([x, y]) =>
      window.dispatchEvent(new PointerEvent('pointerup', { button: 0, clientX: x, clientY: y })),
    [clientX, clientY]
  );
}
//=============================================================================
// In the web host the application saves through the browser: the test turns the
// File System Access picker off so the download fallback runs, and stores the
// download where the test expects the model. The embedded WebView host keeps
// the in-app file dialog.
async function hostMode(page: import('@playwright/test').Page): Promise<string> {
  return page.evaluate(async () => {
    const raw = await (window as any).webui.call(
      'rpc',
      JSON.stringify({ id: 1, method: 'nflow.host.mode', params: {} })
    );
    const parsed = JSON.parse(String(raw));
    return String(parsed.result?.mode ?? 'web');
  });
}
//=============================================================================
// Returns the path of the saved model. The in-app dialog starts in the engine's
// current directory, so the test types the full target path as the file name.
async function saveToDashboardTemp(
  page: import('@playwright/test').Page,
  target: string
): Promise<string> {
  if ((await hostMode(page)) === 'web') {
    await page.evaluate(() => {
      (window as any).showSaveFilePicker = undefined;
    });
    const downloading = page.waitForEvent('download');
    await page.locator('.menubar-item[data-menu="file"]').click();
    await page.locator('.menubar-item[data-action="saveAs"]').click();
    const download = await downloading;
    await download.saveAs(target);
    await expect(page.locator('#statusBarInfo')).toContainText('Saved', { timeout: 10_000 });
    return target;
  }
  await page.locator('.menubar-item[data-menu="file"]').click();
  await page.locator('.menubar-item[data-action="saveAs"]').click();
  const dialog = page.getByRole('dialog');
  await expect(dialog).toBeVisible();
  await dialog.getByPlaceholder('File name').fill(target.replace(/\\/g, '/'));
  await dialog.getByRole('button', { name: 'Save', exact: true }).click();
  // a refused write reports "Save error: ..." in the status bar after the 30 s bridge timeout
  // at the latest: wait past it so the failure names its cause
  await expect(page.locator('#statusBarInfo')).toContainText(/Saved|error/, { timeout: 40_000 });
  await expect(page.locator('#statusBarInfo')).toContainText('Saved');
  return target;
}
//=============================================================================
test('a dashboard control updates its bound parameter at the next major step', async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(model);
  await expect(page.locator('.dashboard-control-track')).toHaveCount(1);

  const runButton = page.locator('#floatingToolbar [data-action="run"]');
  await runButton.click();
  await expect(page.locator('#floatingToolbar [data-action="pause"]')).toBeVisible();
  const slider = page.locator('[data-block-id="control"]');
  const track = slider.locator('.dashboard-control-track');
  const thumb = slider.locator('.dashboard-control-thumb');
  const transformBefore = await slider.getAttribute('transform');
  const box = await track.boundingBox();
  const thumbBox = await thumb.boundingBox();
  expect(box).not.toBeNull();
  expect(thumbBox).not.toBeNull();
  const clientX = (box?.x ?? 0) + (box?.width ?? 1) * 0.8;
  const clientY = (box?.y ?? 0) + (box?.height ?? 1) / 2;
  await page.mouse.move(
    (thumbBox?.x ?? 0) + (thumbBox?.width ?? 1) / 2,
    (thumbBox?.y ?? 0) + (thumbBox?.height ?? 1) / 2
  );
  await page.mouse.down();
  await page.mouse.move((box?.x ?? 0) + (box?.width ?? 1) * 0.6, clientY, { steps: 2 });
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 30_000 });
  await page.mouse.move(clientX, clientY, { steps: 2 });
  await page.mouse.up();

  await expect
    .poll(async () => Number(await slider.getAttribute('aria-valuenow')))
    .toBeGreaterThan(5);
  await expect(slider).toHaveAttribute('transform', transformBefore ?? '');

  const displayed = Number(await page.locator('.dashboard-display-value').textContent());
  expect(displayed).toBeGreaterThan(10);
});
//=============================================================================
test('the bundled slider responds to a real mouse at reduced zoom', async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(runtimeExample);
  await expect(page.locator('.dashboard-control-track')).toHaveCount(1);

  // Zoom out so the slider is rendered below unity scale. The geometry is read
  // live from boundingBox(), so the exact percentage does not matter: assert only
  // that the zoom is reduced.
  for (let index = 0; index < 7; index += 1) await page.locator('#zoomOutBtn').click();
  await expect
    .poll(async () =>
      Number((await page.locator('#statusBarZoom').textContent())?.replace('%', ''))
    )
    .toBeLessThan(60);
  const slider = page.locator('[data-block-id="gain_slider"]');
  const track = slider.locator('.dashboard-control-track');
  const thumb = slider.locator('.dashboard-control-thumb');
  const box = await track.boundingBox();
  const thumbBox = await thumb.boundingBox();
  expect(box).not.toBeNull();
  expect(thumbBox).not.toBeNull();
  const clickX = (box?.x ?? 0) + (box?.width ?? 1) * 0.8;
  const clickY = (box?.y ?? 0) + (box?.height ?? 1) / 2;
  const initialThumbHitClass = await page.evaluate(
    ([x, y]) => document.elementFromPoint(x, y)?.getAttribute('class') || '',
    [
      (thumbBox?.x ?? 0) + (thumbBox?.width ?? 1) / 2,
      (thumbBox?.y ?? 0) + (thumbBox?.height ?? 1) / 2,
    ]
  );
  expect(initialThumbHitClass).toContain('dashboard-control-thumb');
  const enlargedHitClass = await page.evaluate(
    ([x, y]) => document.elementFromPoint(x, y)?.getAttribute('class') || '',
    [clickX, clickY + 8]
  );
  expect(enlargedHitClass).toContain('dashboard-control-hit');
  await page.mouse.click(clickX, clickY + 8);
  await expect
    .poll(async () => Number(await slider.getAttribute('aria-valuenow')))
    .toBeGreaterThan(5);
  const movedThumbBox = await thumb.boundingBox();
  expect(movedThumbBox).not.toBeNull();
  await page.mouse.move(
    (movedThumbBox?.x ?? 0) + (movedThumbBox?.width ?? 1) / 2,
    (movedThumbBox?.y ?? 0) + (movedThumbBox?.height ?? 1) / 2
  );
  await page.mouse.down();
  await page.mouse.move((box?.x ?? 0) + (box?.width ?? 1) * 0.2, clickY, { steps: 4 });
  await page.mouse.up();

  await expect.poll(async () => Number(await slider.getAttribute('aria-valuenow'))).toBeLessThan(5);
});
//=============================================================================
test('the bundled knob follows a real mouse drag at gallery zoom', async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(galleryExample);
  const knob = page.locator('[data-block-id="dashboard_knob"]');
  const dial = knob.locator('.dashboard-knob');
  const box = await dial.boundingBox();
  expect(box).not.toBeNull();
  const centerX = (box?.x ?? 0) + (box?.width ?? 1) / 2;
  const centerY = (box?.y ?? 0) + (box?.height ?? 1) / 2;

  await page.mouse.move(centerX, (box?.y ?? 0) + 1);
  await page.mouse.down();
  await page.mouse.move((box?.x ?? 0) + (box?.width ?? 1) - 1, centerY, { steps: 5 });
  await page.mouse.up();

  await expect
    .poll(async () => Number(await knob.getAttribute('aria-valuenow')))
    .toBeGreaterThan(80);
});
//=============================================================================
test('the bundled dashboard gallery runs with all dashboard blocks', async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(galleryExample);
  await expect(page.locator('#blockLayer .dashboard-block')).toHaveCount(20);

  await page.locator('#floatingToolbar [data-action="run"]').click();
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 30_000 });
  const scopePath = page.locator('[data-block-id="dashboard_scope"] .dashboard-scope-path');
  await expect.poll(async () => (await scopePath.getAttribute('d')) ?? '').toMatch(/[ML]/);
  const scopeGeometry = await scopePath.evaluate((path) => ({
    clipPath: path.getAttribute('clip-path'),
    yValues: Array.from(path.getAttribute('d')?.matchAll(/[ML] [\d.]+ ([\d.]+)/g) ?? []).map(
      (match) => Number(match[1])
    ),
  }));
  expect(scopeGeometry.clipPath).toMatch(/^url\(#nflow-dashboard-scope-clip-/);
  expect(scopeGeometry.yValues.length).toBeGreaterThan(1);
  expect(scopeGeometry.yValues.every((value) => value >= 18 && value <= 143)).toBe(true);
});
//=============================================================================
test('all dashboard blocks persist and interact during real simulations', async ({ page }) => {
  const tempDir = path.join(repoRoot, 'temp', 'nflow-dashboard-b2b');
  fs.mkdirSync(tempDir, { recursive: true });
  const completeModel = path.join(tempDir, 'dashboard-complete-runtime.nflow');
  fs.writeFileSync(completeModel, JSON.stringify(completeDashboardModel(), null, 2));

  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(completeModel);
  await expect(page.locator('#blockLayer .dashboard-block')).toHaveCount(20);

  const playPause = page.locator('#playPauseBtn');
  await playPause.click();
  await expect(playPause).toHaveAttribute('data-action', 'pause');
  await playPause.click();
  await expect(playPause).toHaveAttribute('data-action', 'run');
  await manipulateAllControls(page);
  await page.waitForTimeout(300);
  await playPause.click();
  await expect(playPause).toHaveAttribute('data-action', 'pause');
  await setSliderNearMaximum(page);
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 30_000 });
  expect(Number(await page.locator('.dashboard-display-value').textContent())).toBeGreaterThan(100);

  const savedModel = await saveToDashboardTemp(page, completeModel);
  await expect
    .poll(() => {
      const saved = JSON.parse(fs.readFileSync(savedModel, 'utf8')) as any;
      return saved.blocks.find((block: any) => block.id === 'gain')?.params?.Gain;
    })
    .toBeGreaterThan(50);

  await page.reload();
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(savedModel);
  await expect(page.locator('#blockLayer .dashboard-block')).toHaveCount(20);
  await page.locator('#floatingToolbar [data-action="run"]').click();
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 30_000 });
  expect(Number(await page.locator('.dashboard-display-value').textContent())).toBeGreaterThan(100);
});
//=============================================================================
