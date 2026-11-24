//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';
import { runLine, showTab } from './helpers';
//=============================================================================
// Full-stack check against the real nelson-gui.exe: the assistant adds a
// code cell to the open notebook through the real notebookInsert channel. The
// engine's __ai_notify__ builtin pushes the tool event itself, so this needs no
// AI provider, yet exercises the whole path (C++ push -> WebUI -> front -> cell)
// and runs the cell on the real interpreter.
//=============================================================================
test('notebook assistant: notebookInsert adds and runs a cell on the real engine', async ({
  page,
}) => {
  await page.goto('/');
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });

  // Open a Notebook from the desktop toolbar.
  await page.getByRole('button', { name: 'New Notebook' }).first().click();
  const notebook = page.locator('.nls-panel').first();
  await expect(notebook).toBeVisible({ timeout: 30_000 });

  // The embedded assistant pane toggles open and offers a model (the ACP backend
  // is always present, even with no provider configured).
  await notebook.getByRole('button', { name: 'Assistant' }).click();
  await expect(notebook.locator('.nls-assistant .assistant-panel')).toBeVisible();
  await expect(notebook.locator('.assistant-panel__brand')).toBeVisible();

  // Drive the real notebookInsert channel end to end with run:true.
  await runLine(
    page,
    "__ai_notify__('notebookInsert', jsonencode(struct('text', 'disp(1234 * 5678)', 'run', true)));"
  );

  // The generated cell ran on the real interpreter; its output is rendered.
  await showTab(page, 'Notebook');
  await expect(notebook.locator('.nls-output').filter({ hasText: '7006652' })).toBeVisible({
    timeout: 60_000,
  });
});
//=============================================================================
