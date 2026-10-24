//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/// <reference types="node" />
import { defineConfig, devices } from '@playwright/test';
//=============================================================================
/**
 * nflow Playwright E2E configuration.
 *
 * Requires a running nflow instance.  Set NFLOW_URL to point at it, e.g.:
 *
 *   NFLOW_URL=http://localhost:12345/nflow.html npm run test:e2e
 */
const nflowUrl = process.env['NFLOW_URL'] ?? 'http://localhost:1973/nflow.html';
export default defineConfig({
  // Ensure the NFLOW server is reachable before starting tests.
  globalSetup: './global-setup.js',
  globalTeardown: './global-teardown.js',
  // Serve the built dist/ automatically unless NFLOW_URL points elsewhere.
  webServer: process.env['NFLOW_URL']
    ? undefined
    : {
        command: 'node tests/e2e/static-server.mjs',
        url: 'http://localhost:1973/nflow.html',
        cwd: '../..',
        reuseExistingServer: true,
        timeout: 30_000,
      },
  testDir: './tests',
  forbidOnly: !!process.env['CI'],
  retries: process.env['CI'] ? 1 : 0,
  /* Single worker: tests share one nflow instance, run serially to avoid state conflicts. */
  workers: 1,
  reporter: process.env['CI'] ? 'dot' : 'list',

  use: {
    baseURL: nflowUrl,
    navigationTimeout: 30_000,
    actionTimeout: 10_000,
    screenshot: 'only-on-failure',
    video: 'on-first-retry',
    headless: true,
  },

  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],

  outputDir: 'test-results',
});
//=============================================================================
