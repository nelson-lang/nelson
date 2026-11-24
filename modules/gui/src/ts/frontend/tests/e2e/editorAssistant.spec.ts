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
// E2E against the dev server and its in-process mock engine (no native build,
// no AI provider). The editor's embedded assistant is reached like the notebook
// one, and its code-block action inserts at the editor cursor. The assistant
// answer is delivered through the frontend push entry, the same path the engine
// drives, so no model is needed.
//=============================================================================
async function runLine(page: Page, line: string): Promise<void> {
  await page.locator('.console-host').first().click();
  await page.keyboard.type(line);
  await page.keyboard.press('Enter');
}
//=============================================================================
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
// Click the editor's "Assistant" ribbon toggle whatever tier the ribbon is in
// (expanded button, a collapsed group dropdown, or the overflow menu).
async function clickAssistantToggle(editor: Locator): Promise<void> {
  const ribbon = editor.locator('.editor-ribbon');
  const direct = ribbon.getByRole('button', { name: 'Assistant', exact: true });
  if ((await direct.count()) > 0 && (await direct.first().isVisible())) {
    await direct.first().click();
    return;
  }
  const analyze = ribbon.locator('.editor-ribbon__collapsed-btn', { hasText: 'ANALYZE' });
  if ((await analyze.count()) > 0) {
    await analyze.first().click();
  } else {
    await ribbon.locator('.editor-ribbon__overflow .editor-ribbon__collapsed-btn').click();
  }
  await ribbon.locator('.editor-ribbon__popup-item', { hasText: 'Assistant' }).click();
}
//=============================================================================
test.use({ viewport: { width: 1680, height: 900 } });
//=============================================================================
test('editor: embedded assistant toggles, shows its target and inserts at the cursor', async ({
  page,
}) => {
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toBeVisible();
  await runLine(page, "editor('assistant_demo.m')");
  const editor = page.locator('.editor-panel').first();
  await expect(editor).toBeVisible();

  // The ribbon toggle opens an embedded pane, consistent with the notebook.
  await expect(editor.locator('.editor-assistant')).toHaveCount(0);
  await clickAssistantToggle(editor);
  const pane = editor.locator('.editor-assistant .assistant-panel');
  await expect(pane).toBeVisible();
  // The header names the target file, so it is clear where code is inserted.
  await expect(editor.locator('.editor-assistant .assistant-panel__target')).toContainText('→');

  // Deliver an assistant answer with a code fence (no AI model needed).
  const marker = 'editorAssistantMarker';
  await pushAssistantEvent(page, 'answer', { text: '```\n' + marker + ' = 5\n```' });
  const codeBlock = pane.locator('.assistant-panel__code').first();
  const insert = codeBlock.getByRole('button', { name: 'Insert at cursor' });
  await expect(insert).toBeVisible();
  // The notebook-only "Add cell" action is not offered in the editor.
  await expect(codeBlock.getByRole('button', { name: 'Add cell' })).toHaveCount(0);

  // Place the cursor in the document, then insert the generated code there.
  await editor.locator('.editor-panel__main .monaco-editor .view-lines').click();
  await insert.click();
  await expect
    .poll(async () =>
      editor
        .locator('.editor-panel__main .view-lines')
        .first()
        .evaluate((el) => el.textContent ?? '')
    )
    .toContain(marker);
});
//=============================================================================
test('editor: a filename-addressed insert opens the file and appends the code', async ({
  page,
}) => {
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toBeVisible();
  // No editor is open yet: the startup router (insert_into_nelson_file) must open
  // the target file itself, then append the code once the panel is ready.
  const marker = 'routedFileInsertMarker';
  await pushAssistantEvent(page, 'editorInsert', {
    filename: 'routed_demo.m',
    text: marker + ' = 42',
  });
  const editor = page.locator('.editor-panel').first();
  await expect(editor).toBeVisible({ timeout: 15000 });
  await expect
    .poll(async () =>
      editor
        .locator('.editor-panel__main .view-lines')
        .first()
        .evaluate((el) => el.textContent ?? '')
    )
    .toContain(marker);
});
//=============================================================================
