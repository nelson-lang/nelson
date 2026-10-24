//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test, type Locator, type Page } from '@playwright/test';
//=============================================================================
async function waitForDesktop(page: Page): Promise<void> {
  await page.goto('/');
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect(page.getByRole('status')).toHaveText('Ready');
}
//=============================================================================
function panelGroup(page: Page, child: string): Locator {
  return page.locator('.dv-groupview').filter({ has: page.locator(child) });
}
//=============================================================================
async function runViewAction(page: Page, name: string): Promise<void> {
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.locator('.menu').getByRole('menuitem', { name, exact: true }).click();
}
//=============================================================================
test.beforeEach(async ({ page }) => waitForDesktop(page));
test('default layout is a compact two-column scientific workspace', async ({ page }) => {
  const files = panelGroup(page, '.file-list');
  const workspace = panelGroup(page, '.browser__table');
  const command = panelGroup(page, '.console-panel');

  await expect(files).toBeVisible();
  await expect(workspace).toBeVisible();
  await expect(command).toBeVisible();
  await expect(page.locator('.history-list')).toHaveCount(0);
  await expect(page.locator('.profiler')).toHaveCount(0);
  await expect(page.locator('.variables-host')).toHaveCount(0);

  const [filesBox, workspaceBox, commandBox] = await Promise.all([
    files.boundingBox(),
    workspace.boundingBox(),
    command.boundingBox(),
  ]);
  expect(filesBox).not.toBeNull();
  expect(workspaceBox).not.toBeNull();
  expect(commandBox).not.toBeNull();
  expect(Math.abs(filesBox!.x - workspaceBox!.x)).toBeLessThan(3);
  expect(Math.abs(filesBox!.width - workspaceBox!.width)).toBeLessThan(3);
  expect(filesBox!.width).toBeGreaterThanOrEqual(260);
  expect(filesBox!.width).toBeLessThanOrEqual(330);
  expect(workspaceBox!.y).toBeGreaterThan(filesBox!.y);
  expect(commandBox!.x).toBeGreaterThan(filesBox!.x + filesBox!.width - 3);
  await page.setViewportSize({ width: 800, height: 600 });
  await page.reload();
  await expect(page.getByRole('toolbar', { name: 'Main toolbar' })).toBeVisible();
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect
    .poll(() =>
      page.evaluate(
        () => document.documentElement.scrollWidth <= document.documentElement.clientWidth
      )
    )
    .toBe(true);
});
//=============================================================================
test('rail distinguishes visibility from focus and applies the three-state click behavior', async ({
  page,
}) => {
  const history = page.getByRole('button', { name: /Command History/ });
  const workspace = page
    .getByRole('navigation', { name: 'Panels sidebar' })
    .getByRole('button', { name: /Workspace/ });

  await expect(history).toHaveAttribute('aria-pressed', 'false');
  await history.click();
  await expect(history).toHaveAttribute('aria-pressed', 'true');
  await expect(history).toHaveAttribute('aria-current', 'page');
  const secondaryGroup = panelGroup(page, '.history-list');
  await expect(secondaryGroup.locator('.dv-default-tab', { hasText: 'Workspace' })).toBeVisible();
  await expect(
    secondaryGroup.locator('.dv-default-tab', { hasText: 'Command History' })
  ).toBeVisible();

  await workspace.click();
  await expect(workspace).toHaveAttribute('aria-pressed', 'true');
  await expect(workspace).toHaveAttribute('aria-current', 'page');
  await expect(history).not.toHaveAttribute('aria-current', 'page');

  await workspace.click();
  await expect(workspace).toHaveAttribute('aria-pressed', 'false');
  await expect(page.locator('.browser__table')).toHaveCount(0);
});
//=============================================================================
test('rail Terminal is a stable panel while Window can open additional terminals', async ({
  page,
}) => {
  const rail = page.getByRole('navigation', { name: 'Panels sidebar' });
  const terminal = rail.getByRole('button', { name: 'Terminal', exact: true });
  const workspace = rail.getByRole('button', { name: /Workspace/ });

  await expect(terminal).toHaveAttribute('aria-pressed', 'false');
  await terminal.click();
  await expect(terminal).toHaveAttribute('aria-pressed', 'true');
  await expect(terminal).toHaveAttribute('aria-current', 'page');
  await expect(page.getByRole('tab', { name: 'Terminal', exact: true })).toBeVisible();

  await workspace.click();
  await expect(terminal).not.toHaveAttribute('aria-current', 'page');
  await terminal.click();
  await expect(terminal).toHaveAttribute('aria-current', 'page');
  await terminal.click();
  await expect(terminal).toHaveAttribute('aria-pressed', 'false');
  await expect(page.getByRole('tab', { name: 'Terminal', exact: true })).toHaveCount(0);

  await page.getByRole('menuitem', { name: 'Window' }).click();
  await page.locator('.menu').getByRole('menuitem', { name: 'Terminal', exact: true }).click();
  await expect(page.getByRole('tab', { name: /^Terminal 1$/ })).toBeVisible();
});
//=============================================================================
test('Dockview groups expose screen-reader tab semantics', async ({ page }) => {
  await expect(page.getByRole('main', { name: 'Desktop panels' })).toBeVisible();
  await expect(page.getByRole('tablist', { name: 'Panel tabs' })).toHaveCount(3);
  for (const name of ['Files', 'Workspace', 'Command Window']) {
    await expect(page.getByRole('tab', { name, exact: true })).toBeVisible();
    await expect(page.getByRole('tabpanel', { name, exact: true })).toBeVisible();
  }

  await page.getByRole('button', { name: /Command History/ }).click();
  const history = page.getByRole('tab', { name: 'Command History', exact: true });
  const workspace = page.getByRole('tab', { name: 'Workspace', exact: true });
  await expect(history).toHaveAttribute('aria-selected', 'true');
  await expect(workspace).toHaveAttribute('aria-selected', 'false');
  await expect(page.getByRole('tabpanel', { name: 'Command History' })).toBeVisible();

  await history.focus();
  await page.keyboard.press('ArrowLeft');
  await expect(workspace).toBeFocused();
  await expect(workspace).toHaveAttribute('aria-selected', 'true');
  await expect(history).toHaveAttribute('aria-selected', 'false');
  await expect(page.getByRole('tabpanel', { name: 'Workspace' })).toBeVisible();
});
//=============================================================================
test('browser accessibility tree exposes the shell landmarks and panel tabs', async ({
  page,
  context,
}) => {
  const session = await context.newCDPSession(page);
  await session.send('Accessibility.enable');
  const { nodes } = await session.send('Accessibility.getFullAXTree');
  const entries = nodes.map((node) => ({
    role: String(node.role?.value ?? ''),
    name: String(node.name?.value ?? ''),
  }));
  for (const expected of [
    { role: 'main', name: 'Desktop panels' },
    { role: 'navigation', name: 'Panels sidebar' },
    { role: 'toolbar', name: 'Main toolbar' },
    { role: 'menubar', name: 'Application menu' },
    { role: 'tab', name: 'Files' },
    { role: 'tab', name: 'Workspace' },
    { role: 'tab', name: 'Command Window' },
  ]) {
    expect(entries).toContainEqual(expected);
  }
});
//=============================================================================
test('desktop chrome and context menus are not text-selectable', async ({ page }) => {
  const userSelect = (selector: string) =>
    page
      .locator(selector)
      .first()
      .evaluate((element) => getComputedStyle(element).userSelect);

  for (const selector of ['.nelson-shell', '.menubar', '.desktop-toolbar', '.dv-default-tab']) {
    await expect.poll(() => userSelect(selector)).toBe('none');
  }

  await page.locator('.browser__table tbody tr').first().click({ button: 'right' });
  await expect(page.locator('.ctx-menu')).toBeVisible();
  await expect.poll(() => userSelect('.ctx-menu')).toBe('none');

  await expect.poll(() => userSelect('.console-host')).toBe('text');
  await expect.poll(() => userSelect('.console-input')).toBe('text');
});
//=============================================================================
test('History, Variables and Profiler open as tabs in the Workspace group', async ({ page }) => {
  const cases = [
    { button: /Command History/, content: '.history-list', title: 'Command History' },
    { button: /Variables Editor/, content: '.variables-host', title: 'Variables Editor' },
    { button: /Profiler/, content: '.profiler', title: 'Profiler' },
  ];
  for (const item of cases) {
    const button = page.getByRole('button', { name: item.button });
    await button.click();
    const group = panelGroup(page, item.content);
    await expect(group.locator('.dv-default-tab', { hasText: 'Workspace' })).toBeVisible();
    await expect(group.locator('.dv-default-tab', { hasText: item.title })).toBeVisible();
    await expect(button).toHaveAttribute('aria-current', 'page');
    await button.click();
    await expect(page.locator(item.content)).toHaveCount(0);
  }
  await page.getByRole('button', { name: 'More panels' }).click();
  const more = page.getByRole('menu').last();
  await expect(more.getByRole('menuitemcheckbox', { name: 'Documentation' })).toBeVisible();
});
//=============================================================================
test('Variables Editor keeps one stable dock panel with internal variable tabs', async ({
  page,
}) => {
  await page
    .locator('.browser__table tbody tr')
    .filter({ has: page.locator('td', { hasText: /^A$/ }) })
    .dblclick();
  await expect(page.getByRole('tab', { name: /A \[double 3×3\]/ })).toBeVisible();

  const variablesGroup = panelGroup(page, '.variables-host');
  await variablesGroup.locator('.dv-default-tab', { hasText: 'Workspace' }).click();
  await page
    .locator('.browser__table tbody tr')
    .filter({ has: page.locator('td', { hasText: /^L$/ }) })
    .dblclick();
  await expect(page.getByRole('tab', { name: /A \[double 3×3\]/ })).toBeVisible();
  await expect(page.getByRole('tab', { name: /L \[logical 2×2\]/ })).toBeVisible();
  await expect(page.locator('.dv-default-tab', { hasText: 'Variables Editor' })).toHaveCount(1);
});
//=============================================================================
test('toolbar validates and reuses shared recent directories', async ({ page }) => {
  const field = page.getByRole('combobox', { name: 'Current directory' });
  await field.fill('/workspace/first');
  await field.press('Enter');
  await expect(field).toHaveValue('/workspace/first');

  await field.fill('/workspace/second');
  await field.press('Enter');
  await page.getByRole('button', { name: 'Recent directories' }).click();
  const options = page.getByRole('listbox').getByRole('option');
  await expect(options.nth(0)).toHaveText('/workspace/second');
  await expect(options.nth(1)).toHaveText('/workspace/first');
  await options.nth(1).click();
  await expect(field).toHaveValue('/workspace/first');

  await field.press('ArrowDown');
  await field.press('ArrowDown');
  await field.press('Enter');
  await expect(field).toHaveValue('/workspace/second');

  await field.fill('relative/path');
  await field.press('Enter');
  await expect(field).toHaveValue('/workspace/second');
  await expect(page.getByRole('status')).toContainText('Directory does not exist');

  const consoleInput = page.locator('.console-input').first();
  await consoleInput.fill("cd('/workspace/from-command')");
  await consoleInput.press('Enter');
  await expect(field).toHaveValue('/workspace/from-command');
});
//=============================================================================
test('toolbar Execute filters scripts and responsive overflow keeps secondary actions', async ({
  page,
}) => {
  await expect(page.locator('.desktop-toolbar__secondary')).toBeVisible();
  await page.getByRole('button', { name: 'Execute a Nelson file' }).click();
  const picker = page.getByRole('dialog', { name: 'Execute a Nelson file' });
  const script = picker.getByRole('option', { name: /demo\.m/ });
  await expect(script).toBeVisible();
  await expect(picker.getByRole('option', { name: /notes\.txt/ })).toHaveCount(0);
  await script.dblclick();
  await expect(picker).toBeHidden();

  const directory = page.getByRole('combobox', { name: 'Current directory' });
  await directory.fill('/workspace/other');
  await directory.press('Enter');
  await page.getByRole('button', { name: 'Execute a Nelson file' }).click();
  const reopened = page.getByRole('dialog', { name: 'Execute a Nelson file' });
  await expect(reopened.getByRole('option', { name: /demo\.m/ })).toBeVisible();
  await reopened.getByRole('button', { name: 'Cancel' }).click();

  await page.setViewportSize({ width: 800, height: 600 });
  await expect(page.locator('.desktop-toolbar__secondary')).toBeHidden();
  await expect(page.locator('.desktop-toolbar__label').first()).toBeHidden();
  await page.getByRole('button', { name: 'More actions' }).click();
  const overflow = page.getByRole('menu').last();
  await expect(overflow).toContainText('Load Workspace');
  await expect(overflow).toContainText('Save Workspace');
  await expect(overflow).toContainText('Import Data');
});
//=============================================================================
test('Open in Editor falls back to the file picker outside the embedded WebView', async ({
  page,
}) => {
  await page.getByRole('menuitem', { name: 'File' }).click();
  await page.getByRole('menuitem', { name: 'Open in Editor…' }).click();
  const picker = page.getByRole('dialog', { name: 'Open in Editor' });
  await expect(picker).toBeVisible();
  await picker.getByRole('option', { name: /demo\.m/ }).dblclick();
  await expect(picker).toBeHidden();
  await expect(page.locator('.editor-panel').first()).toBeVisible();
});
//=============================================================================
test('View controls shell visibility and persists it across reloads', async ({ page }) => {
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.getByRole('menuitemcheckbox', { name: /Main toolbar/ }).click();
  await expect(page.getByRole('toolbar', { name: 'Main toolbar' })).toHaveCount(0);
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.getByRole('menuitemcheckbox', { name: /Panels sidebar/ }).click();
  await expect(page.getByRole('navigation', { name: 'Panels sidebar' })).toHaveCount(0);
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.getByRole('menuitemcheckbox', { name: /Status bar/ }).click();
  await expect(page.getByRole('status')).toHaveCount(0);
  await page.reload();
  await expect(page.getByRole('toolbar', { name: 'Main toolbar' })).toHaveCount(0);
  await expect(page.getByRole('navigation', { name: 'Panels sidebar' })).toHaveCount(0);
  await expect(page.getByRole('status')).toHaveCount(0);

  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.getByRole('menuitemcheckbox', { name: /Main toolbar/ }).click();
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.getByRole('menuitemcheckbox', { name: /Panels sidebar/ }).click();
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.getByRole('menuitemcheckbox', { name: /Status bar/ }).click();
  await expect(page.getByRole('toolbar', { name: 'Main toolbar' })).toBeVisible();
  await expect(page.getByRole('navigation', { name: 'Panels sidebar' })).toBeVisible();
  await expect(page.getByRole('status')).toHaveText('Ready');
});
//=============================================================================
test('System theme follows live operating-system color-scheme changes', async ({ page }) => {
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.getByRole('menuitemradio', { name: 'System' }).click();
  await page.emulateMedia({ colorScheme: 'dark' });
  await expect(page.locator('.nelson-shell--dark')).toBeVisible();
  await page.emulateMedia({ colorScheme: 'light' });
  await expect(page.locator('.nelson-shell--light')).toBeVisible();
});
//=============================================================================
test('panel action menus protect Command Window and expose movable dock actions', async ({
  page,
}) => {
  await expect(page.locator('.dv-default-tab-action')).toHaveCount(0);
  const command = panelGroup(page, '.console-panel');
  const commandActions = command.getByRole('button', { name: 'Panel actions' });
  await commandActions.click();
  const commandMenu = page.getByRole('menu').last();
  await expect(commandMenu.getByRole('menuitem', { name: 'Collapse panel' })).toBeFocused();
  await page.keyboard.press('ArrowDown');
  await expect(commandMenu.getByRole('menuitem', { name: 'Float panel' })).toBeFocused();
  await expect(commandMenu.getByRole('menuitem', { name: 'Float panel' })).toBeVisible();
  await expect(commandMenu.getByRole('menuitem', { name: 'Close panel' })).toHaveCount(0);
  await page.keyboard.press('Escape');
  await expect(commandActions).toBeFocused();

  const files = panelGroup(page, '.file-list');
  await files.getByRole('button', { name: 'Panel actions' }).click();
  const filesMenu = page.getByRole('menu').last();
  await expect(filesMenu.getByRole('menuitem', { name: 'Move Left' })).toBeVisible();
  await expect(filesMenu.getByRole('menuitem', { name: 'Move Right' })).toBeVisible();
  await expect(filesMenu.getByRole('menuitem', { name: 'Move Bottom' })).toBeVisible();
  await expect(filesMenu.getByRole('menuitem', { name: 'Close panel' })).toBeVisible();
});
//=============================================================================
test('double-click floats and re-docks a collapsed panel in expanded form', async ({ page }) => {
  const filesGroup = panelGroup(page, '.file-list');
  await filesGroup.getByRole('button', { name: 'Collapse panel' }).click();
  await filesGroup.locator('.dv-default-tab', { hasText: 'Files' }).dblclick();
  const floating = page.locator('.dv-resize-container').filter({ has: page.locator('.file-list') });
  await expect(floating).toBeVisible();
  expect((await floating.boundingBox())!.height).toBeGreaterThan(250);

  await floating.locator('.dv-default-tab', { hasText: 'Files' }).dblclick();
  await expect(
    page.locator('.dv-resize-container').filter({ has: page.locator('.file-list') })
  ).toHaveCount(0);
  await expect(page.locator('.file-list')).toBeVisible();
});
//=============================================================================
for (const direction of ['Left', 'Right', 'Bottom'] as const) {
  test(`dock Move ${direction} action changes the panel placement`, async ({ page }) => {
    const filesGroup = panelGroup(page, '.file-list');
    const before = await filesGroup.boundingBox();
    await filesGroup.getByRole('button', { name: 'Panel actions' }).click();
    await page
      .getByRole('menu')
      .last()
      .getByRole('menuitem', { name: `Move ${direction}` })
      .click();
    const movedGroup = panelGroup(page, '.file-list');
    await expect(movedGroup).toBeVisible();
    await expect
      .poll(async () => {
        const after = await movedGroup.boundingBox();
        return (
          Math.abs(after!.x - before!.x) +
          Math.abs(after!.y - before!.y) +
          Math.abs(after!.width - before!.width) +
          Math.abs(after!.height - before!.height)
        );
      })
      .toBeGreaterThan(10);
  });
}
//=============================================================================
test('command palette supports its shortcut, ranking and Escape', async ({ page }) => {
  await page.keyboard.press('Control+Shift+P');
  const dialog = page.getByRole('dialog', { name: 'Command palette' });
  await expect(dialog).toBeVisible();
  await dialog.getByRole('textbox').fill('prof');
  await expect(dialog.getByRole('option').first()).toContainText('Profiler');
  await page.keyboard.press('Escape');
  await expect(dialog).toHaveCount(0);

  await page.keyboard.press('Control+Shift+P');
  const themePalette = page.getByRole('dialog', { name: 'Command palette' });
  await themePalette.getByRole('textbox').fill('Dark');
  await expect(themePalette.getByRole('option').first()).toContainText('Dark');
  await page.keyboard.press('Enter');
  await expect(page.locator('.nelson-shell--dark')).toBeVisible();
});
//=============================================================================
test('layout presets restore their documented panel sets', async ({ page }) => {
  await runViewAction(page, 'Command Window only');
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect(page.locator('.file-list')).toHaveCount(0);
  await expect(page.locator('.browser__table')).toHaveCount(0);

  await runViewAction(page, 'Two columns (Default)');
  await expect(page.locator('.file-list')).toBeVisible();
  await expect(page.locator('.browser__table')).toBeVisible();
  await expect(page.locator('.history-list')).toHaveCount(0);

  await runViewAction(page, 'Three columns');
  await expect(page.locator('.file-list')).toBeVisible();
  await expect(page.locator('.browser__table')).toBeVisible();
  await expect(page.locator('.history-list')).toBeVisible();

  await runViewAction(page, 'Restore default layout');
  await expect(page.locator('.file-list')).toBeVisible();
  await expect(page.locator('.browser__table')).toBeVisible();
  await expect(page.locator('.history-list')).toHaveCount(0);
});
//=============================================================================
test('named layouts save and restore the complete v3 Dockview state', async ({ page }) => {
  await page.getByRole('menuitem', { name: 'View' }).click();
  await page.locator('.menu').getByRole('menuitem', { name: 'Save current layout…' }).click();
  const prompt = page.getByRole('dialog', { name: 'Layout name:' });
  await prompt.getByRole('textbox').fill('Analysis layout');
  await prompt.getByRole('textbox').press('Enter');
  await expect(prompt).toHaveCount(0);

  const workspace = page
    .getByRole('navigation', { name: 'Panels sidebar' })
    .getByRole('button', { name: /Workspace/ });
  await workspace.click();
  await workspace.click();
  await expect(page.locator('.browser__table')).toHaveCount(0);

  await runViewAction(page, 'Analysis layout');
  await expect(page.locator('.browser__table')).toBeVisible();
  await expect(page.locator('.console-panel')).toBeVisible();
});
//=============================================================================
test('collapsed panels keep their expanded height across reload', async ({ page }) => {
  const filesGroup = page
    .locator('.dv-groupview')
    .filter({ has: page.locator('.dv-default-tab', { hasText: 'Files' }) });
  const collapse = filesGroup.getByRole('button', { name: 'Collapse panel' });
  const expanded = (await filesGroup.boundingBox())!.height;
  expect(expanded).toBeGreaterThan(100);
  await collapse.click();
  await expect(filesGroup.getByRole('button', { name: 'Expand panel' })).toBeVisible();
  await expect.poll(async () => (await filesGroup.boundingBox())!.height).toBeLessThan(45);

  await page.reload();
  const restoredGroup = page
    .locator('.dv-groupview')
    .filter({ has: page.locator('.dv-default-tab', { hasText: 'Files' }) });
  await expect(restoredGroup.getByRole('button', { name: 'Expand panel' })).toBeVisible();
  await expect.poll(async () => (await restoredGroup.boundingBox())!.height).toBeLessThan(45);
  await restoredGroup.getByRole('button', { name: 'Expand panel' }).click();
  await expect.poll(async () => (await restoredGroup.boundingBox())!.height).toBeGreaterThan(100);
});
//=============================================================================
test('menus and focus zones support keyboard-only navigation', async ({ page }) => {
  const view = page.getByRole('menuitem', { name: 'View' });
  await view.focus();
  await page.keyboard.press('ArrowDown');
  await expect(page.locator('.menu button').first()).toBeFocused();
  await page.keyboard.press('ArrowDown');
  await expect(page.locator('.menu button').nth(1)).toBeFocused();
  await page.keyboard.press('Escape');
  await expect(page.locator('.menu')).toHaveCount(0);
  await expect(view).toBeFocused();

  await page.getByRole('button', { name: 'More actions' }).click();
  const overflowButtons = page.locator('.desktop-toolbar__menu button');
  await expect(overflowButtons).toHaveCount(3);
  await overflowButtons.first().focus();
  await page.keyboard.press('ArrowDown');
  await expect(overflowButtons.nth(1)).toBeFocused();
  await page.keyboard.press('Escape');
  await expect(page.locator('.desktop-toolbar__menu')).toHaveCount(0);
  await expect(page.getByRole('button', { name: 'More actions' })).toBeFocused();

  await page.getByRole('button', { name: 'More panels' }).click();
  const morePanelButtons = page.locator('.sidebar__menu button');
  await expect(morePanelButtons).toHaveCount(9);
  await morePanelButtons.first().focus();
  await page.keyboard.press('ArrowDown');
  await expect(morePanelButtons.nth(1)).toBeFocused();
  await page.keyboard.press('Escape');
  await expect(page.locator('.sidebar__menu')).toHaveCount(0);
  await expect(page.getByRole('button', { name: 'More panels' })).toBeFocused();

  await page.getByRole('menuitem', { name: 'File' }).focus();
  await page.keyboard.press('Control+F6');
  await expect(
    page.getByRole('toolbar', { name: 'Main toolbar' }).getByRole('button').first()
  ).toBeFocused();
  await page.keyboard.press('Control+Shift+F6');
  await expect(page.getByRole('menuitem', { name: 'File' })).toBeFocused();
});
//=============================================================================
test('shell icon controls expose independent accessible names and visible focus', async ({
  page,
}) => {
  const unnamed = await page
    .locator(
      '.desktop-toolbar button, .sidebar button, .menubar button, .panel-actions, .group-collapse'
    )
    .evaluateAll((controls) =>
      controls
        .filter((control) => {
          const element = control as HTMLElement;
          return !(
            element.getAttribute('aria-label') ||
            element.getAttribute('title') ||
            element.textContent?.trim()
          );
        })
        .map((control) => control.outerHTML)
    );
  expect(unnamed).toEqual([]);
  await expect(page.locator('.desktop-icon:not([aria-hidden="true"])')).toHaveCount(0);

  const editor = page.getByRole('button', { name: 'Text Editor' });
  await editor.focus();
  await page.keyboard.press('Tab');
  const execute = page.getByRole('button', { name: 'Execute a Nelson file' });
  await expect(execute).toBeFocused();
  const outline = await execute.evaluate((element) => {
    const style = getComputedStyle(element);
    return { style: style.outlineStyle, width: parseFloat(style.outlineWidth) };
  });
  expect(outline.style).not.toBe('none');
  expect(outline.width).toBeGreaterThanOrEqual(2);
});
//=============================================================================
