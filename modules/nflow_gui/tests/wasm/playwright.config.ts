//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { defineConfig, devices } from '@playwright/test';
//=============================================================================
const port = Number(process.env.NFLOW_WASM_TEST_PORT || '5224');
const directory = process.env.NFLOW_WASM_BROWSER_DIR || '../../temp/build-wasm/bin/wasm/browser';
export default defineConfig({
  testDir: '.',
  timeout: 180000,
  workers: 1,
  use: { baseURL: `http://127.0.0.1:${port}`, trace: 'retain-on-failure' },
  webServer: {
    command: `node ../../tools/wasm/serve-browser.mjs ${JSON.stringify(directory)} ${port}`,
    cwd: '../..',
    port,
    reuseExistingServer: false,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
  outputDir: '../../../../temp/fmi-wasm-browser-results',
});
//=============================================================================
