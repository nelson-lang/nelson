//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test } from '@playwright/test';
//=============================================================================
test('keeps a connected desktop alive when rendering delays its heartbeats', async ({ page }) => {
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  const response = await page.evaluate(async () => {
    const bridge = window.webui!;
    const original = bridge.call.bind(bridge);
    bridge.call = (name, payload) => {
      const request = JSON.parse(payload);
      return request.method === 'app.heartbeat'
        ? Promise.resolve(JSON.stringify({ id: request.id, result: { ok: true } }))
        : original(name, payload);
    };
    let pong: unknown;
    try {
      await new Promise((resolve) => setTimeout(resolve, 5500));
      pong = JSON.parse(
        await original('rpc', JSON.stringify({ id: 'watchdog-check', method: 'ping' }))
      );
    } finally {
      bridge.call = original;
    }
    // feed the watchdog before the page closes, so the next test does not race its budget
    await original('rpc', JSON.stringify({ id: 'watchdog-beat', method: 'app.heartbeat' }));
    return pong as { error?: unknown; result?: unknown };
  });
  expect(response.error).toBeUndefined();
  expect(response.result).toBeDefined();
});
//=============================================================================
