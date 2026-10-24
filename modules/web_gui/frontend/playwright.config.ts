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
// E2E against the dev server, which runs the app with the in-process mock engine
// (no native backend needed), so these are stable in CI.
export default defineConfig({
  testDir: './tests/e2e',
  timeout: 30_000,
  fullyParallel: false,
  retries: 0,
  reporter: [['list']],
  use: {
    baseURL: 'http://localhost:5199',
    headless: true,
    trace: 'off',
  },
  webServer: {
    command: 'npm run dev',
    port: 5199,
    reuseExistingServer: true,
    timeout: 60_000,
  },
  projects: [
    {
      name: 'chromium',
      testIgnore: /hidpi\.spec\.ts/,
      use: { ...devices['Desktop Chrome'] },
    },
    ...[1, 1.25, 1.5, 2, 3].map((deviceScaleFactor) => ({
      name: `hidpi-${deviceScaleFactor}`,
      testMatch: /hidpi\.spec\.ts/,
      use: {
        ...devices['Desktop Chrome'],
        viewport: { width: 1000, height: 700 },
        deviceScaleFactor,
      },
    })),
  ],
});
//=============================================================================
