//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Back-to-back coverage of the embedded AI assistant pane against a real Nelson
// process. CI has no AI model, so this never sends a prompt (which would call
// the Claude/Ollama backend): it checks the wiring that does not need a model —
// the activity-bar button opens the pane, the live model list round-trips
// through the engine (ACP is always offered), and an assistant event pushed
// through the real __nelsonPush channel renders in the pane. The streamed
// answer path is covered without a model in the unit tests (assistant-pane and
// assistant-backend specs).
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
//=============================================================================
async function bootApp(page: Page): Promise<void> {
  // The assistant pane is opt-in: its activity-bar icon renders only when
  // isAssistantAiEnabled() finds the flag. In production the seed page persists
  // 'nf-assistant-ai' to storage (window.__nf_bootstrap is lost on its redirect
  // to nflow.html), so seed the same key the host would — the real delivery path.
  // This mirrors NELSON_ASSISTANT_AI=1, which start-nelson sets on the live
  // engine for the engine-side gate.
  await page.addInitScript(() => {
    try {
      localStorage.setItem('nf-assistant-ai', '1');
    } catch (_e) {
      /* storage unavailable */
    }
  });
  await page.goto('/nflow.html');
  await page.waitForFunction(() => typeof (window as any).webui !== 'undefined', undefined, {
    timeout: 20_000,
  });
  await page.waitForFunction(
    () => {
      const webui = (window as any).webui;
      return typeof webui?.isConnected !== 'function' || webui.isConnected();
    },
    undefined,
    { timeout: 20_000 }
  );
  await page.waitForSelector('#svgCanvas', { timeout: 20_000 });
  await page.waitForTimeout(1500);
}
//=============================================================================
test('the activity-bar button opens the embedded assistant pane', async ({ page }) => {
  await bootApp(page);

  await page.click('.activity-icon[data-panel="assistant"]');

  const panel = page.locator('#panel-assistant');
  await expect(panel).toHaveClass(/active/, { timeout: 10_000 });
  // The shared pane rendered its chat surface.
  await expect(panel.locator('.assistant-panel')).toBeVisible();
  await expect(panel.locator('textarea')).toBeVisible();
});
//=============================================================================
test('the pane always offers the Claude (ACP) backend from the live engine', async ({ page }) => {
  await bootApp(page);
  await page.click('.activity-icon[data-panel="assistant"]');

  // listModels round-trips through nelson.ai.models(); the pane always prepends
  // the ACP backend, so it must appear whether or not a provider is configured.
  const acpOption = page.locator('#panel-assistant .assistant-panel__model option', {
    hasText: 'Claude (ACP)',
  });
  await expect(acpOption).toHaveCount(1, { timeout: 10_000 });
});
//=============================================================================
test('an assistant answer pushed over the real channel renders in the pane', async ({ page }) => {
  await bootApp(page);
  await page.click('.activity-icon[data-panel="assistant"]');
  await expect(page.locator('#panel-assistant')).toHaveClass(/active/, { timeout: 10_000 });

  // Simulate the engine sink delivering a final answer, without invoking any AI
  // model: push straight onto the same __nelsonPush('assistant', ...) channel
  // the NFlow assistant sink uses.
  await page.evaluate(() => {
    const push = (window as unknown as { __nelsonPush?: (c: string, p: string) => void })
      .__nelsonPush;
    push?.('assistant', JSON.stringify({ channel: 'answer', payload: '{"text":"b2b answer ok"}' }));
  });

  await expect(page.locator('#panel-assistant .assistant-panel__bubble')).toContainText(
    'b2b answer ok',
    { timeout: 10_000 }
  );
});
//=============================================================================
