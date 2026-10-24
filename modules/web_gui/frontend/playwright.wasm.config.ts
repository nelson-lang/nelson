//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { defineConfig, devices } from '@playwright/test';
//=============================================================================
const port = Number(process.env.NELSON_WASM_TEST_PORT ?? '5210');
const browserDirectory =
  process.env.NELSON_WASM_BROWSER_DIR ?? '../../../temp/build-wasm/bin/wasm/browser';
export default defineConfig({
  testDir: './tests/wasm',
  timeout: 180_000,
  fullyParallel: false,
  workers: 1,
  retries: 0,
  reporter: [['list']],
  use: {
    baseURL: `http://127.0.0.1:${port}`,
    headless: true,
    trace: 'retain-on-failure',
  },
  webServer: {
    command: `node ../../../tools/wasm/serve-browser.mjs ${JSON.stringify(
      browserDirectory
    )} ${port}`,
    port,
    reuseExistingServer: false,
    timeout: 30_000,
  },
  projects: [
    {
      name: 'chromium-wasm',
      use: { ...devices['Desktop Chrome'] },
    },
    {
      name: 'firefox-wasm',
      use: { ...devices['Desktop Firefox'] },
    },
    {
      name: 'webkit-wasm',
      use: { ...devices['Desktop Safari'] },
    },
  ],
});
//=============================================================================
