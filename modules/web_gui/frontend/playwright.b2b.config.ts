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
// End-to-end smoke against the REAL engine: Playwright launches a platform
// specific nelson-webview executable and drives the served desktop at
// http://localhost:22333. Requirements: the native build must be up to date
// (nlsWeb_gui + deployed dist) and no other nelson-webview may hold port 22333.
//
//   npm run test:b2b
//
// Scenarios share one interpreter, so run them serially to avoid concurrent
// commands and figure mutations. Keep each interaction sequence in one test;
// the engine watchdog stops it after the last browser has disconnected.
export default defineConfig({
  testDir: './tests/b2b',
  timeout: 120_000,
  fullyParallel: false,
  workers: 1,
  retries: 0,
  reporter: [['list']],
  use: {
    baseURL: 'http://localhost:22333',
    headless: true,
    trace: 'off',
  },
  webServer: {
    command: 'node tests/b2b/start-nelson.mjs',
    port: 22333,
    reuseExistingServer: false,
    timeout: 90_000,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});
//=============================================================================
