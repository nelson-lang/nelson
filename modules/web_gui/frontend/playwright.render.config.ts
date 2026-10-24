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
const baseURL = process.env.NELSON_RENDER_TEST_URL;
if (!baseURL) throw new Error('Set NELSON_RENDER_TEST_URL to a running WASM desktop.');
export default defineConfig({
  testDir: './tests/render_compare',
  testMatch: 'published.spec.ts',
  timeout: 180_000,
  workers: 1,
  reporter: [['list']],
  use: {
    baseURL,
    headless: true,
    serviceWorkers: process.env.NELSON_RENDER_FRONTEND_DIR ? 'block' : 'allow',
  },
  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
    { name: 'firefox', use: { ...devices['Desktop Firefox'] } },
  ],
});
//=============================================================================
