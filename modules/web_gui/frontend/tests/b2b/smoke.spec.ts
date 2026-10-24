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
import { readFileSync, statSync } from 'node:fs';
//=============================================================================
// Full-stack smoke against the real nelson-webview.exe (see playwright.b2b.config):
// real interpreter, real WebUI transport, real render path. One scenario, one
// page; the engine's heartbeat watchdog would stop it between separate tests.
async function runLine(page: Page, line: string, waitForReady = true) {
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });
  // A figure (or another panel) may have taken the active tab of the console's
  // dockview group: bring the Command Window back to front before typing.
  const input = page.locator('.console-input');
  if (!(await input.isVisible())) {
    await page.getByText('Command Window', { exact: true }).first().click();
  }
  await input.click();
  await input.fill(line);
  await input.press('Enter');
  if (waitForReady) {
    await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });
  }
}
//=============================================================================
// The console still pads matrix rows with non-breaking spaces; normalise them
// so plain-text matching behaves as a reader would expect.
async function terminalText(page: Page) {
  return page
    .locator('.console-host')
    .first()
    .evaluate((element) => {
      const copy = element.cloneNode(true) as HTMLElement;
      copy.querySelectorAll('.console-input-stack').forEach((prompt) => prompt.remove());
      return copy.innerText.replace(/ /g, ' ');
    });
}
//=============================================================================
test('real engine: REPL, history, uicontrol callback, PNG export', async ({ page }) => {
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect.poll(() => terminalText(page), { timeout: 60_000 }).toContain('connected: engine');

  await runLine(page, 'smoke_a = 21 * 2');
  await expect.poll(() => terminalText(page)).toContain('42');

  // After output without a trailing newline the prompt sits on the same row
  // (Hello>> ): the input is that row, not a separate element that would make the
  // line jump on Enter.
  await runLine(page, "cprintf('red','inline-echo')");
  await expect.poll(() => terminalText(page)).toContain('inline-echo');
  const onSameRow = await page.evaluate(() => {
    const inp = document.querySelector('.console-input')!.getBoundingClientRect();
    const lines = [...document.querySelectorAll('.console-line')];
    const last = lines[lines.length - 1].getBoundingClientRect();
    return Math.abs(inp.top - last.top) < 8;
  });
  expect(onSameRow).toBe(true);

  await runLine(page, "cprintf('red','RedText')");
  await expect
    .poll(() =>
      page.evaluate(() => {
        const s = [...document.querySelectorAll('.console-line span')].find(
          (e) => e.textContent === 'RedText'
        );
        return s ? getComputedStyle(s).color : 'none';
      })
    )
    .toBe('rgb(255, 0, 0)');

  const pasteInput = page.locator('.console-input');
  await pasteInput.click();
  await pasteInput.evaluate((el: HTMLInputElement) => {
    const dt = new DataTransfer();
    dt.setData('text', 'pa_smoke = 11\npb_smoke = 22\npc_smoke = 3');
    el.dispatchEvent(
      new ClipboardEvent('paste', { clipboardData: dt, bubbles: true, cancelable: true })
    );
  });
  await expect(pasteInput).toHaveValue('pa_smoke = 11\npb_smoke = 22\npc_smoke = 3');
  await expect(pasteInput).toHaveAttribute('rows', '3');
  expect(await terminalText(page)).not.toContain('pa_smoke = 11');
  await pasteInput.press('Enter');
  await expect(page.getByRole('status')).toHaveText('Ready');
  await runLine(page, 'pa_smoke + pb_smoke + pc_smoke');
  await expect.poll(() => terminalText(page)).toContain('36');

  await runLine(
    page,
    't_int = tic; while toc(t_int) < 10; a = 1; end; printf("INT_DONE\\n")',
    false
  );
  await page.waitForTimeout(1000);
  const tInt = Date.now();
  await page.getByText('Command Window', { exact: true }).first().click();
  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect.poll(() => terminalText(page), { timeout: 15_000 }).toContain('INT_DONE');
  expect(Date.now() - tInt).toBeLessThan(4000);

  const input = page.locator('.console-input');
  await input.click();
  await input.fill('magi');
  await input.press('Tab');
  await expect.poll(() => input.inputValue()).toBe('magic');
  await input.fill('');
  // A multi-match prefix opens the popup; it must render above the input line
  // (it once docked to the panel bottom, overlapping the prompt).
  await input.fill('disp(pl');
  await input.press('Tab');
  const popup = page.locator('.console-popup');
  await expect(popup).toBeVisible();
  const placement = await page.evaluate(() => {
    const p = document.querySelector('.console-popup')!.getBoundingClientRect();
    const i = document.querySelector('.console-input')!.getBoundingClientRect();
    const style = getComputedStyle(document.querySelector('.console-input')!);
    const canvas = document.createElement('canvas');
    const context = canvas.getContext('2d')!;
    context.font = style.font;
    const tokenStart =
      i.left + parseFloat(style.paddingLeft || '0') + context.measureText('disp(').width;
    return {
      above: p.bottom <= i.top + 1 && p.top >= 0,
      tokenAligned: Math.abs(p.left - tokenStart) <= 2,
    };
  });
  expect(placement.above).toBe(true);
  expect(placement.tokenAligned).toBe(true);
  await input.press('Escape');
  await input.fill('');

  await runLine(page, "disp('findme_a findme_b findme_c')");
  await page.locator('.console-input').focus();
  await page.keyboard.press('Control+f');
  await expect(page.locator('.console-find-bar')).toBeVisible();
  await page.locator('.console-find-bar__input').fill('findme');
  await expect.poll(() => page.locator('.console-find').count()).toBeGreaterThan(0);
  await expect(page.locator('.console-find--current')).toHaveCount(1);
  await expect(page.locator('.console-find-bar__count')).not.toHaveText('0/0');
  await page.locator('.console-find-bar__input').press('Escape');
  // Browser zoom stays locked, but the gesture is redirected to the console
  // font zoom when it originates from the console (accessibility). The zoom
  // lands on React's next async render, and re-rendering the accumulated
  // transcript can lose the race against a one-shot read: poll instead.
  const consoleFontSize = () =>
    page.evaluate(() =>
      parseInt(getComputedStyle(document.querySelector('.console-host')!).fontSize)
    );
  const fz0 = await consoleFontSize();
  await page.locator('.console-input').focus();
  await page.keyboard.press('Control+=');
  await expect.poll(consoleFontSize).toBe(fz0 + 1);
  await page.keyboard.press('Control+0');
  await expect.poll(consoleFontSize).toBe(14);

  type WebUiWindow = Window & {
    webui: {
      call: (name: string, payload: string) => string | Promise<string>;
    };
  };

  const cwdResult = JSON.parse(
    await page.evaluate(() =>
      (window as WebUiWindow).webui.call(
        'rpc',
        JSON.stringify({ id: 1, method: 'fs.cwd', params: {} })
      )
    )
  ).result as { cwd: string; separator: string; recentDirectories: string[] };
  const fsCwd = cwdResult.cwd;
  expect(cwdResult.separator).toMatch(/^[\\/]$/);
  expect(cwdResult.recentDirectories[0]).toBe(fsCwd);
  const fsRun = (method: string, params: object) =>
    page.evaluate(
      ({ m, p }) =>
        (window as WebUiWindow).webui.call('rpc', JSON.stringify({ id: 1, method: m, params: p })),
      { m: method, p: params }
    );
  const changedDirectory = JSON.parse(await fsRun('fs.chdir', { path: fsCwd }));
  expect(changedDirectory.result.cwd).toBe(fsCwd);
  expect(changedDirectory.result.separator).toBe(cwdResult.separator);
  expect(changedDirectory.result.recentDirectories[0]).toBe(fsCwd);
  await fsRun('fs.create', { path: fsCwd + '/b2b_fs.m', kind: 'file', content: '%% x' });
  let listed = JSON.parse(await fsRun('fs.list', { path: fsCwd }));
  expect(listed.result.entries.some((e: { name: string }) => e.name === 'b2b_fs.m')).toBe(true);
  await fsRun('fs.rename', { from: fsCwd + '/b2b_fs.m', to: fsCwd + '/b2b_fs2.m' });
  await fsRun('fs.delete', { path: fsCwd + '/b2b_fs2.m' });
  listed = JSON.parse(await fsRun('fs.list', { path: fsCwd }));
  expect(listed.result.entries.some((e: { name: string }) => e.name.startsWith('b2b_fs'))).toBe(
    false
  );
  await expect(page.locator('.files__filter')).toBeVisible();
  await page.locator('.file-list li').first().click({ button: 'right' });
  await expect(page.locator('.ctx-menu__item', { hasText: 'Rename' })).toBeVisible();
  await expect(page.locator('.ctx-menu__item', { hasText: 'Add to Path' })).toBeVisible();
  await page.keyboard.press('Escape');

  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await expect(page.locator('.menu button', { hasText: 'Command Window only' })).toBeVisible();
  await expect(page.locator('.menu button', { hasText: 'Two columns (Default)' })).toBeVisible();
  await page.keyboard.press('Escape');
  await runLine(page, 'ws_smoke = magic(3);');
  await expect
    .poll(() =>
      page.evaluate(() =>
        [...document.querySelectorAll('.browser__name')].some((e) => e.textContent === 'ws_smoke')
      )
    )
    .toBe(true);
  await page.getByRole('button', { name: /Command History/ }).click();
  await expect(page.locator('.history-list')).toBeVisible();
  await page.locator('.history-list li').last().click({ button: 'right' });
  await expect(page.locator('.ctx-menu__item', { hasText: 'Create Script' })).toBeVisible();
  await expect(page.locator('.ctx-menu__item', { hasText: 'Delete' }).first()).toBeVisible();
  await page.keyboard.press('Escape');

  await runLine(
    page,
    'progress_figure = gcf(); ' +
      "progress_dialog = uiprogressdlg(progress_figure, 'Title', 'Import', " +
      "'Message', 'Reading data', 'Value', 0.25);"
  );
  const progressCard = page.locator('.progress-dialog', { hasText: 'Import' });
  await expect(progressCard).toBeVisible();
  await expect(progressCard).toContainText('Reading data');
  await expect(progressCard).toContainText('25%');
  await runLine(page, "set(progress_dialog, 'value', 60); drawnow");
  await runLine(page, "set(progress_dialog, 'labelText', 'Writing data'); drawnow");
  await expect(progressCard).toContainText('Writing data');
  await expect(progressCard).toContainText('60%');
  await runLine(page, "set(progress_dialog, 'visible', false);");
  await expect(progressCard).toBeHidden();
  await runLine(page, "set(progress_dialog, 'visible', true);");
  await expect(progressCard).toBeVisible();
  await runLine(page, 'delete(progress_dialog);');
  await runLine(page, "disp(['PROGRESS_VALID_', int2str(isvalid(progress_dialog))]);");
  await expect.poll(() => terminalText(page)).toContain('PROGRESS_VALID_0');
  await expect(progressCard).toBeHidden();

  await runLine(
    page,
    "f = figure('Position', [100 100 420 260]); " +
      'global smoke_key_press smoke_key_release; ' +
      "set(f, 'WindowKeyPressFcn', 'global smoke_key_press; smoke_key_press = 1;', " +
      "'WindowKeyReleaseFcn', 'global smoke_key_release; smoke_key_release = 1;'); " +
      'smoke_key_press = 0; smoke_key_release = 0; ' +
      "ax = axes('Parent', f); axis(ax, [0 1 0 1]); axis(ax, 'off'); " +
      "text(ax, 0.5, 0.5, 'ExportText'); " +
      "uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'SmokeBtn', " +
      "'Position', [20 20 110 32], 'Callback', 'smoke_clicked = 7;');"
  );
  const btn = page.locator('.figure-panel').getByRole('button', { name: 'SmokeBtn' });
  await expect(btn).toBeVisible({ timeout: 30_000 });
  const figureCanvas = page.locator('.figure-panel__canvas');
  // a docked figure fills its panel, whatever its requested Position: the control keeps its
  // pixel size and its offset from the bottom left corner of the drawing area
  await expect
    .poll(async () => {
      const canvasBox = await figureCanvas.boundingBox();
      const buttonBox = await btn.boundingBox();
      if (!canvasBox || !buttonBox) return 1000;
      const measured = [
        buttonBox.width,
        buttonBox.height,
        buttonBox.x - canvasBox.x,
        canvasBox.y + canvasBox.height - (buttonBox.y + buttonBox.height),
      ];
      const expected = [110, 32, 20, 20];
      return Math.max(...measured.map((value, index) => Math.abs(value - expected[index])));
    })
    .toBeLessThanOrEqual(2);
  await btn.click();
  await runLine(page, 'disp(smoke_clicked)');
  await expect.poll(() => terminalText(page)).toContain('7');

  // Window callbacks observe keys even while a child control owns focus.
  await page
    .getByText(/^Figure \d+$/)
    .last()
    .click();
  await btn.focus();
  await page.keyboard.down('w');
  await page.keyboard.up('w');
  await runLine(
    page,
    "drawnow(); disp(['KEY_EVENTS_', int2str(smoke_key_press), " +
      "'_', int2str(smoke_key_release)]);"
  );
  await expect.poll(() => terminalText(page)).toContain('KEY_EVENTS_1_1');

  // Typing brought the Command Window tab back to front; surface the figure.
  await page
    .getByText(/^Figure \d+$/)
    .last()
    .click();
  const figurePanel = page.locator('.figure-panel');
  await figurePanel.getByRole('button', { name: 'More figure actions', exact: true }).click();
  await expect(
    figurePanel.getByRole('button', { name: 'Undock (separate window)', exact: true })
  ).toBeVisible();
  await page.keyboard.press('Escape');
  await figurePanel.getByRole('button', { name: 'Export', exact: true }).click();
  await figurePanel.getByLabel('Format', { exact: true }).selectOption('svg');
  const svgDownloadPromise = page.waitForEvent('download');
  await figurePanel.getByRole('button', { name: 'Download SVG', exact: true }).click();
  const svgDownload = await svgDownloadPromise;
  const svgFile = await svgDownload.path();
  expect(svgFile).toBeTruthy();
  // a figure holding uicontrols is exported as one raster page embedded in the SVG
  const svgText = readFileSync(svgFile!, 'utf8');
  expect(svgText).toContain('<svg');
  expect(svgText).toContain('<image');
  expect(svgText).toContain('data:image/png;base64');
  await figurePanel.getByRole('button', { name: 'Export', exact: true }).click();
  await figurePanel.getByLabel('Format', { exact: true }).selectOption('png');
  const exportButton = figurePanel.getByRole('button', { name: 'Download PNG', exact: true });
  // The export goes through the engine (render + rasterize + base64), which can
  // lag behind the click on a cold cache: give the download its own budget
  // instead of eating the whole test timeout.
  const downloadPromise = page.waitForEvent('download', { timeout: 60_000 });
  await exportButton.click();
  const download = await downloadPromise;
  expect(download.suggestedFilename()).toMatch(/^figure-\d+\.png$/);
  const file = await download.path();
  expect(file).toBeTruthy();
  expect(statSync(file!).size).toBeGreaterThan(1000);

  // Dark theme on: the menu is portalled out of its host, so it must still be
  // rendered inside the themed shell root (it once landed on document.body and
  // came out light-on-light).
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Dark', { exact: true }).click();
  // Normalize the reproduction to one current figure: earlier smoke sections
  // intentionally create more than one graphics object.
  await runLine(page, "close('all'); surf(peaks)");
  const figTab = page.locator('.dv-default-tab', { hasText: /^Figure \d+$/ }).first();
  await figTab.click({ button: 'right' });
  const floatItem = page.locator('.ctx-menu__item', { hasText: 'Detach as floating window' });
  await expect(floatItem).toBeVisible();
  // toBeVisible() only checks the box is non-empty: a popup clipped by an
  // overflow:hidden host (the tab strip) still passes it while being invisible
  // to the user. Assert the menu really is the element under its own centre.
  await expect
    .poll(() =>
      page.evaluate(() => {
        const item = document.querySelector('.ctx-menu__item');
        if (!item) return 'no-menu';
        const r = item.getBoundingClientRect();
        const hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
        return hit && hit.closest('.ctx-menu') ? 'on-top' : 'covered';
      })
    )
    .toBe('on-top');
  // The menu inherits the dark theme instead of falling back to the light defaults
  // outside the shell.
  expect(
    await page.evaluate(() => {
      const menu = document.querySelector('.ctx-menu') as HTMLElement;
      const inShell = !!menu.closest('.nelson-shell');
      const rgb = getComputedStyle(menu).backgroundColor.match(/\d+/g)!.map(Number);
      const luminance = (rgb[0] + rgb[1] + rgb[2]) / 3;
      return { inShell, dark: luminance < 128 };
    })
  ).toEqual({ inShell: true, dark: true });
  await expect(
    page.locator('.ctx-menu__item', { hasText: 'Undock (separate window)' })
  ).toBeVisible();
  await floatItem.click();
  const floating = page.locator('.dv-resize-container .figure-panel');
  await expect(floating).toBeVisible();
  await page.locator('.dv-resize-container .dv-default-tab').click({ button: 'right' });
  const redockItem = page.locator('.ctx-menu__item', { hasText: 'Re-dock into the desktop' });
  await expect(redockItem).toBeVisible();
  await redockItem.click();
  await expect(page.locator('.dv-resize-container .figure-panel')).toHaveCount(0);
  await expect(page.locator('.figure-panel')).toBeVisible();

  // Closing a web figure closes the engine object too. Nelson may reuse the
  // same graphics handle, so the exact user cycle must still create a panel.
  await page
    .locator('.dv-default-tab', { hasText: /^Figure \d+$/ })
    .first()
    .click({ button: 'right' });
  await page.locator('.ctx-menu__item', { hasText: 'Close panel' }).click();
  await expect(page.locator('.figure-panel')).toHaveCount(0);
  const closedFigures = JSON.parse(await fsRun('figure.list', {}));
  expect(closedFigures.result.figures).toHaveLength(0);
  await runLine(page, 'surf(peaks)');
  await expect(page.locator('.figure-panel')).toBeVisible({ timeout: 30_000 });

  // A close command sends figure.closed immediately, before the interpreter
  // reaches its next prompt. Keep the engine busy afterwards to prove that the
  // panel removal does not depend on prompt polling.
  await runLine(page, 't_close = tic; close(gcf()); while toc(t_close) < 2; end;', false);
  await expect(page.locator('.statusbar')).toHaveText(/Busy$/);
  await expect(page.locator('.figure-panel')).toHaveCount(0, { timeout: 1_500 });
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 10_000 });
  await runLine(page, 'surf(peaks)');
  await expect(page.locator('.figure-panel')).toBeVisible({ timeout: 30_000 });

  const folderTab = page.locator('.dv-default-tab', { hasText: 'Files' }).first();
  await folderTab.click({ button: 'right' });
  await expect(
    page.locator('.ctx-menu__item', { hasText: 'Detach as floating window' })
  ).toBeVisible();
  // Panel actions are offered for any panel; the undock entry is not (only
  // figures have a standalone window route).
  await expect(page.locator('.ctx-menu__item', { hasText: 'Close other tabs' })).toBeVisible();
  await expect(
    page.locator('.ctx-menu__item', { hasText: 'Undock (separate window)' })
  ).toHaveCount(0);
  await page.locator('.ctx-menu__item', { hasText: 'Detach as floating window' }).click();
  await expect(page.locator('.dv-resize-container .files__path')).toBeVisible();
  await folderTab.click({ button: 'right' });
  await page.locator('.ctx-menu__item', { hasText: 'Re-dock into the desktop' }).click();
  await expect(page.locator('.dv-resize-container .files__path')).toHaveCount(0);

  // inserthtml renders in the console flow; markdown produces the HTML and KaTeX
  // typesets $...$.
  await runLine(
    page,
    "inserthtml(markdown(sprintf('# Heading\\n\\nBody **bold** and math $E=mc^2$.')))"
  );
  await expect(page.locator('.console-rich h1')).toHaveText('Heading');
  await expect(page.locator('.console-rich strong')).toHaveText('bold');
  await expect(page.locator('.console-rich .katex').first()).toBeVisible();

  // In web mode markdowndisp emits raw markdown HTML for KaTeX, with no Qt MathJax
  // pre-render.
  await runLine(page, "markdowndisp(sprintf('## Sub\\n\\n$a+b$'))");
  await expect(page.locator('.console-rich h2')).toContainText('Sub');
  await expect(page.locator('.console-rich .katex').last()).toBeVisible();

  await runLine(page, 'trunc_m = magic(60);');
  await runLine(page, 'trunc_m');
  await expect.poll(() => terminalText(page), { timeout: 20_000 }).toContain('Display all 60 rows');
  const truncatedText = await terminalText(page);
  expect(truncatedText).toContain('⋮');
  // The label is a link (OSC 8): clicking it writes the full matrix.
  await page
    .getByText(/Display\s+all\s+60\s+rows/)
    .first()
    .click();
  await expect
    .poll(async () => (await terminalText(page)).length)
    .toBeGreaterThan(truncatedText.length);

  // Reopen Files through the rail so it is once again stacked above Workspace;
  // the preceding float/re-dock check intentionally allows Dockview to attach
  // it to any surviving group when its original group no longer exists.
  const filesRail = page.getByRole('button', { name: /Files/ });
  await filesRail.click();
  await filesRail.click();
  await filesRail.click();
  await expect(page.locator('.file-list')).toBeVisible();
  const collapsibleGroup = page
    .locator('.dv-groupview')
    .filter({ has: page.locator('.file-list') })
    .first();
  const collapseButton = collapsibleGroup.locator('.group-collapse');
  const groupHeight = () =>
    collapsibleGroup.evaluate((group) => {
      const btn = group.querySelector('.group-collapse') as HTMLElement;
      return {
        h: Math.round(group.getBoundingClientRect().height),
        label: btn.getAttribute('aria-label'),
      };
    });
  const expanded = await groupHeight();
  expect(expanded.h).toBeGreaterThan(60);
  await collapseButton.click();
  await expect.poll(async () => (await groupHeight()).h).toBeLessThan(45);
  expect((await groupHeight()).label).toBe('Expand panel');
  await collapseButton.click();
  await expect.poll(async () => (await groupHeight()).h).toBe(expanded.h);

  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Command History', { exact: true }).click();
  await expect(
    page.locator('.history-list li', { hasText: 'smoke_a = 21 * 2' }).last()
  ).toBeVisible();
  await expect(
    page.locator('.history-list li', { hasText: 'disp(smoke_clicked)' }).last()
  ).toBeVisible();
});
//=============================================================================
