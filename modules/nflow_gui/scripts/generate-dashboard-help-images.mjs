// ============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================

import { chromium } from '@playwright/test';
import { spawn } from 'node:child_process';
import fs from 'node:fs/promises';
import http from 'node:http';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const guiRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const repoRoot = path.resolve(guiRoot, '..', '..');
const port = 1987;
const url = `http://127.0.0.1:${port}/nflow.html`;
const runtimeModel = path.join(
  repoRoot,
  'modules',
  'nflow_blocks',
  'examples',
  'dashboard',
  'Dashboard_Runtime_Demo.nflow'
);

function probe() {
  return new Promise((resolve) => {
    const request = http.get(url, (response) => {
      response.resume();
      resolve(response.statusCode === 200);
    });
    request.on('error', () => resolve(false));
    request.setTimeout(500, () => {
      request.destroy();
      resolve(false);
    });
  });
}

async function waitForServer() {
  const deadline = Date.now() + 15_000;
  while (Date.now() < deadline) {
    if (await probe()) return;
    await new Promise((resolve) => setTimeout(resolve, 150));
  }
  throw new Error(`NFlow static server did not start at ${url}`);
}

async function preparePage(browser, locale, viewport) {
  const context = await browser.newContext({ viewport });
  const page = await context.newPage();
  await page.addInitScript((selectedLocale) => {
    localStorage.setItem('nf-i18n-locale', selectedLocale);
    sessionStorage.setItem('nf-i18n-locale', selectedLocale);
    window.webui = {
      call: async (_name, rawEnvelope) => {
        let envelope = {};
        try {
          envelope = JSON.parse(String(rawEnvelope || '{}'));
        } catch (_error) {}
        const method = String(envelope.method || '');
        let result = {};
        if (method === 'nflow.file.loadInitial') result = { success: false, canceled: true };
        if (method === 'nflow.library.list') result = { manifests: [], locations: [] };
        return JSON.stringify({ id: envelope.id, result });
      },
    };
  }, locale);
  await page.goto(url);
  await page.waitForFunction(() => !document.documentElement.classList.contains('nf-loading'));
  await page.waitForSelector('#svgCanvas');
  await page.waitForFunction(async () => {
    const ready = window.__nf_lib_ready_promise;
    if (ready && typeof ready.then === 'function') await ready;
    return document.querySelectorAll('.tool[data-type^="dashboard"]').length === 20;
  });
  return { context, page };
}

async function captureLibrary(browser, locale, outputPath) {
  const { context, page } = await preparePage(browser, locale, { width: 1000, height: 900 });
  await page.locator('#blockLibrarySearch').fill('dashboard');
  const group = page.locator('details.tool-group', {
    has: page.locator('.tool[data-type="dashboardScope"]'),
  });
  await page.waitForFunction(() =>
    Array.from(document.querySelectorAll('.tool[data-type^="dashboard"]')).every(
      (tool) => (tool.querySelector('svg[data-library-icon]')?.childElementCount || 0) > 0
    )
  );
  await group.evaluate((element) => {
    element.open = true;
    const capture = document.createElement('section');
    capture.id = 'helpDashboardLibrary';
    capture.style.cssText =
      'position:fixed;inset:0 auto auto 0;width:650px;background:#fff;color:#17191c;z-index:10000';
    const heading = document.createElement('header');
    heading.style.cssText =
      'height:34px;padding:0 12px;display:flex;align-items:center;justify-content:space-between;border-bottom:1px solid #d7dce1;font-size:13px';
    heading.innerHTML = '<strong>Dashboard</strong><span style="color:#60656c">20</span>';
    capture.appendChild(heading);
    const grid = document.createElement('div');
    grid.style.cssText = 'display:grid;grid-template-columns:1fr 1fr';
    element.querySelectorAll('.tool').forEach((tool) => {
      const clone = tool.cloneNode(true);
      clone.style.cssText = 'min-height:58px;border-bottom:1px solid #e1e4e8;border-radius:0';
      grid.appendChild(clone);
    });
    capture.appendChild(grid);
    document.body.appendChild(capture);
  });
  await page.locator('#helpDashboardLibrary').screenshot({
    path: outputPath,
    animations: 'disabled',
  });
  await context.close();
}

async function captureBinding(browser, locale, outputPath) {
  const { context, page } = await preparePage(browser, locale, { width: 1440, height: 900 });
  await page.locator('#fileOpenInput').setInputFiles(runtimeModel);
  const slider = page.locator('[data-block-id="gain_slider"]');
  await slider.waitFor();
  const bounds = await slider.boundingBox();
  if (!bounds) throw new Error('Dashboard Slider is not visible');
  await page.mouse.click(bounds.x + 6, bounds.y + 6);
  await page.waitForSelector('.dashboard-binding-status.valid');
  await page.evaluate(() => {
    document.dispatchEvent(
      new CustomEvent('nf-dashboard-update', {
        detail: { id: 'dashboard_display', connected: true, valid: true, value: 6.2 },
      })
    );
    const bottom = document.getElementById('bottomPanel');
    if (bottom) bottom.style.display = 'none';
  });
  await page.screenshot({ path: outputPath, animations: 'disabled' });
  await context.close();
}

async function main() {
  const server = spawn(process.execPath, ['tests/e2e/static-server.mjs'], {
    cwd: guiRoot,
    env: {
      ...process.env,
      STATIC_SERVER_PORT: String(port),
      STATIC_SERVER_ROOT: path.join(guiRoot, 'dist'),
    },
    stdio: 'ignore',
  });
  let browser;
  try {
    await waitForServer();
    browser = await chromium.launch({ headless: true });
    for (const locale of ['en_US', 'fr_FR']) {
      const imageDir = path.join(guiRoot, 'help', locale, 'images');
      await fs.mkdir(imageDir, { recursive: true });
      await captureLibrary(browser, locale, path.join(imageDir, 'nflow_dashboard_library.png'));
      await captureBinding(browser, locale, path.join(imageDir, 'nflow_dashboard_binding.png'));
    }
  } finally {
    if (browser) await browser.close();
    server.kill();
  }
}

await main();
