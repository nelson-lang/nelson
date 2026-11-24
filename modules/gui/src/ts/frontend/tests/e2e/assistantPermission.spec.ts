//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
//=============================================================================
// E2E against the dev server and its in-process mock engine (no AI model). The
// permission prompt is driven by pushing the engine's 'permission' event through
// the frontend push entry, the same path the ACP backend uses, so the Allow/Deny
// modal and its reply are testable in CI.
async function pushAssistantEvent(page: Page, channel: string, payload: unknown): Promise<void> {
  await page.evaluate(
    ({ channel, payload }) => {
      const push = (window as unknown as { __nelsonPush?: (c: string, p: string) => void })
        .__nelsonPush;
      push?.('tool.event', JSON.stringify({ tool: 'assistant', channel, payload }));
    },
    { channel, payload: JSON.stringify(payload) }
  );
}
//=============================================================================
test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toBeVisible();
});
//=============================================================================
test('assistant: a permission request opens an Allow/Deny prompt', async ({ page }) => {
  await pushAssistantEvent(page, 'permission', {
    id: 7,
    tool: 'read_file',
    path: 'd:/demo.m',
    title: 'Claude wants to read d:/demo.m',
  });
  const dialog = page.getByRole('dialog');
  await expect(dialog).toBeVisible();
  await expect(dialog).toContainText('d:/demo.m');
  await expect(dialog.getByRole('button', { name: 'Allow once' })).toBeVisible();
  await expect(dialog.getByRole('button', { name: 'Always for this file' })).toBeVisible();
  await expect(dialog.getByRole('button', { name: 'Deny' })).toBeVisible();
  // Choosing a decision closes the prompt (the reply goes over the RPC).
  await dialog.getByRole('button', { name: 'Allow once' }).click();
  await expect(page.getByRole('dialog')).toHaveCount(0);
});
//=============================================================================
test('assistant: the permission prompt closes if the run ends without a click', async ({
  page,
}) => {
  await pushAssistantEvent(page, 'permission', {
    id: 8,
    tool: 'read_file',
    path: 'd:/demo.m',
    title: '',
  });
  await expect(page.getByRole('dialog')).toBeVisible();
  // The run finished (answer) — the prompt must not be left orphaned.
  await pushAssistantEvent(page, 'answer', { text: 'done' });
  await expect(page.getByRole('dialog')).toHaveCount(0);
});
//=============================================================================
