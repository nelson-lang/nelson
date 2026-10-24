//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/// <reference types="node" />
//
// Back-to-back configuration: drives the real NFlow UI served by a real
// Nelson process (real webui bridge, real C++ simulation engine, real native
// router), no mocks. `npm run test:b2b`.
//=============================================================================
import { defineConfig, devices } from '@playwright/test';
//=============================================================================
const port = process.env['NFLOW_B2B_PORT'] ?? '17973';
const nflowUrl = `http://localhost:${port}/nflow.html`;
export default defineConfig({
  testDir: './tests',
  forbidOnly: !!process.env['CI'],
  /* Under an instrumented (ASAN) host a long serial run can leave the shared
     Nelson process briefly unresponsive, so the supervisor recycles it mid-run
     (see start-nelson.mjs); a test caught during that window fails on boot or
     mid-simulation though it passes cleanly on its own. One retry absorbs that
     host-recycle flake without masking a genuine failure, which still fails the
     retry. */
  retries: 1,
  /* One Nelson instance, shared engine state: run serially. */
  workers: 1,
  reporter: process.env['CI'] ? 'dot' : 'list',
  webServer: {
    command: 'node tests/b2b/start-nelson.mjs',
    url: nflowUrl,
    cwd: '../..',
    /* A host left over from a previous session serves stale engine state (and
       may predate the current build), so default to a fresh supervised host;
       the npm script reaps any stale listener first. Set NFLOW_B2B_REUSE=1 to
       run against a host you started yourself. */
    reuseExistingServer: !!process.env['NFLOW_B2B_REUSE'],
    timeout: 120_000,
  },
  use: {
    baseURL: nflowUrl,
    navigationTimeout: 30_000,
    actionTimeout: 15_000,
    screenshot: 'only-on-failure',
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
