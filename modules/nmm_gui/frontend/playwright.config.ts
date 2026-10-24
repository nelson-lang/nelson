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
// The built bundle (dist) is served by `vite preview` and talks to a mocked
// nmmGuiRpc (tests/e2e/fixtures.ts), so no Nelson engine is needed.
export default defineConfig({
  testDir: './tests/e2e',
  timeout: 30_000,
  fullyParallel: true,
  retries: 0,
  reporter: [['list']],
  use: {
    baseURL: 'http://127.0.0.1:5211',
    headless: true,
    trace: 'off',
  },
  webServer: {
    command: 'npm run build && npx vite preview --host 127.0.0.1',
    port: 5211,
    reuseExistingServer: true,
    timeout: 120_000,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'], viewport: { width: 1000, height: 720 } } }],
});
//=============================================================================
