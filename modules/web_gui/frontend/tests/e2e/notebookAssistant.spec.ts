//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, type Page, type Locator } from '@playwright/test';
//=============================================================================
// E2E against the dev server and its in-process mock engine, so no native build
// and no AI provider are required. The notebookInsert channel is driven directly
// through the frontend push entry (window.__nelsonPush), exactly as the engine's
// __ai_notify__ builtin drives it, which keeps the assistant wiring testable in
// CI where no model is available.
//=============================================================================
async function openNotebook(page: Page): Promise<Locator> {
  await page.getByRole('button', { name: 'New Notebook' }).first().click();
  const notebook = page.locator('.nls-panel').first();
  await expect(notebook).toBeVisible();
  return notebook;
}
//=============================================================================
// Push an assistant tool event the way the engine does, without any AI model.
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
test('notebook: the embedded assistant pane toggles open and closed', async ({ page }) => {
  const notebook = await openNotebook(page);
  const toggle = notebook.getByRole('button', { name: 'Assistant' });
  await expect(notebook.locator('.nls-assistant')).toHaveCount(0);
  await toggle.click();
  await expect(notebook.locator('.nls-assistant .assistant-panel')).toBeVisible();
  // The header shows where generated code lands, so the target is explicit.
  await expect(notebook.locator('.assistant-panel__target')).toContainText('→');
  await toggle.click();
  await expect(notebook.locator('.nls-assistant')).toHaveCount(0);
});
//=============================================================================
test('notebook: the assistant shows the session permission-mode selector', async ({ page }) => {
  const notebook = await openNotebook(page);
  await notebook.getByRole('button', { name: 'Assistant' }).click();
  const select = notebook.locator('.nls-assistant .assistant-mode-bar select');
  await expect(select).toBeVisible();
  // Default is "always_ask"; picking another mode is reflected immediately.
  await expect(select).toHaveValue('always_ask');
  await select.selectOption('full');
  await expect(select).toHaveValue('full');
});
//=============================================================================
test('notebook: a chat code block offers surface actions that add a cell', async ({ page }) => {
  const notebook = await openNotebook(page);
  await expect(notebook.locator('.nls-codecell').first()).toBeVisible();
  await notebook.getByRole('button', { name: 'Assistant' }).click();
  const pane = notebook.locator('.nls-assistant .assistant-panel');
  await expect(pane).toBeVisible();
  // Deliver an assistant answer with a code fence (no AI model needed), so the
  // pane renders a code block with the host's surface-aware actions.
  const marker = 'assistantBlockMarker';
  await pushAssistantEvent(page, 'answer', { text: '```\n' + marker + ' = 9\n```' });
  // Scope to the first rendered code block (React StrictMode double-renders the
  // dev server, so the same answer can appear twice; prod renders it once).
  const codeBlock = pane.locator('.assistant-panel__code').first();
  const addCell = codeBlock.getByRole('button', { name: 'Add cell' });
  await expect(addCell).toBeVisible();
  // The console-only "Run" button is replaced by the notebook actions.
  await expect(codeBlock.getByRole('button', { name: 'Run', exact: true })).toHaveCount(0);
  await addCell.click();
  await expect
    .poll(async () =>
      notebook
        .locator('.nls-codecell .view-lines')
        .last()
        .evaluate((el) => el.textContent ?? '')
    )
    .toContain(marker);
});
//=============================================================================
test('notebook: notebookInsert adds a code cell holding the generated code', async ({ page }) => {
  const notebook = await openNotebook(page);
  // Let the sample document finish mounting its own cells first.
  await expect(notebook.locator('.nls-codecell').first()).toBeVisible();
  // The marker is unique to the generated cell, so finding it in a cell proves a
  // new cell was appended with exactly that code (not an edit of an existing one).
  const marker = 'assistantNotebookMarker';
  await pushAssistantEvent(page, 'notebookInsert', { text: `${marker} = 7`, run: false });
  await expect
    .poll(async () =>
      notebook
        .locator('.nls-codecell .view-lines')
        .last()
        .evaluate((el) => el.textContent ?? '')
    )
    .toContain(marker);
});
//=============================================================================
