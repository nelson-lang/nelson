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
import { showFigureBars, waitForFigureLayout } from '../figureBars';
//=============================================================================
// xterm reads real keyboard events from its hidden textarea.
async function focusTerminal(page: Page) {
  await page.locator('.console-host').first().click();
}
//=============================================================================
async function runLine(page: Page, line: string) {
  await focusTerminal(page);
  await page.keyboard.type(line);
  await page.keyboard.press('Enter');
}
//=============================================================================
function terminalText(page: Page) {
  return page
    .locator('.xterm-rows')
    .first()
    .evaluate((element) => {
      const copy = element.cloneNode(true) as HTMLElement;
      copy.querySelectorAll('.console-input-stack').forEach((prompt) => prompt.remove());
      return copy.innerText;
    });
}
//=============================================================================
// The editable prompt is a <textarea>, whose value is not part of the
// transcript's innerText; read it directly for buffer/completion assertions.
function inputText(page: Page) {
  return page.locator('.console-input').first().inputValue();
}
//=============================================================================
function postNelsonMessage(page: Page, message: Record<string, unknown>) {
  return page.evaluate(
    (payload) =>
      new Promise((resolve, reject) => {
        const id = String(payload.id ?? `post-message-${crypto.randomUUID()}`);
        const timer = window.setTimeout(() => {
          window.removeEventListener('message', onMessage);
          reject(new Error('Timed out waiting for Nelson postMessage reply'));
        }, 5000);
        function onMessage(event: MessageEvent) {
          if (
            event.data?.nelson !== 'web-desktop' ||
            event.data?.id !== id ||
            event.data?.requestType !== payload.type
          ) {
            return;
          }
          window.clearTimeout(timer);
          window.removeEventListener('message', onMessage);
          resolve(event.data);
        }
        window.addEventListener('message', onMessage);
        window.postMessage({ nelson: 'web-desktop', ...payload, id }, window.location.origin);
      }),
    message
  );
}
//=============================================================================
test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await expect(page.locator('.console-panel')).toBeVisible();
  // Wait for the boot banner (the mock prompt is "-->", the real one ">>").
  await expect.poll(() => terminalText(page)).toContain('connected');
});
//=============================================================================
test('boots against the mock backend', async ({ page }) => {
  await expect.poll(() => terminalText(page)).toContain('connected: mock');
});
//=============================================================================
test('runs a startup command from the URL query string', async ({ page }) => {
  await page.goto('/?run=42');
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect.poll(() => terminalText(page)).toContain('connected: mock');
  await expect.poll(() => terminalText(page)).toContain('42');
  await expect.poll(() => terminalText(page)).toContain('ans');
});
//=============================================================================
test('runs an embedded postMessage command after the desktop is ready', async ({ page }) => {
  const reply = postNelsonMessage(page, { type: 'run', command: '1+2' });
  await expect(reply).resolves.toMatchObject({ type: 'ack', requestType: 'run' });
  await expect.poll(() => terminalText(page)).toContain('3');
});
//=============================================================================
test('creates an embedded postMessage script that can be opened', async ({ page }) => {
  const reply = postNelsonMessage(page, {
    type: 'script',
    file: 'posted-script.m',
    content: "disp('from postMessage script')",
  });
  await expect(reply).resolves.toMatchObject({ type: 'ack', requestType: 'script' });
  await expect(
    postNelsonMessage(page, { type: 'open', path: '/workspace/posted-script.m' })
  ).resolves.toMatchObject({ type: 'ack', requestType: 'open' });
  await expect(page.locator('.editor-panel').first()).toBeVisible({ timeout: 15000 });
  await expect(page.locator('.editor-panel .view-lines').first()).toContainText(
    'from postMessage script',
    { timeout: 15000 }
  );
  await expect(page.locator('.dv-default-tab', { hasText: 'posted-script.m' })).toBeVisible();
});
//=============================================================================
test('creates and runs an embedded postMessage script', async ({ page }) => {
  const reply = postNelsonMessage(page, {
    type: 'script',
    file: 'posted-run.m',
    content: "disp('run me')",
    run: true,
  });
  await expect(reply).resolves.toMatchObject({ type: 'ack', requestType: 'script' });
  await page.locator('.dv-default-tab', { hasText: 'Command Window' }).click();
  await expect.poll(() => terminalText(page)).toContain('[run] /workspace/posted-run.m');
});
//=============================================================================
test('opens an embedded postMessage workspace path', async ({ page }) => {
  await expect(
    postNelsonMessage(page, {
      type: 'script',
      file: 'posted-open.m',
      content: "disp('open me')",
    })
  ).resolves.toMatchObject({ type: 'ack', requestType: 'script' });

  await expect(
    postNelsonMessage(page, { type: 'open', path: '/workspace/posted-open.m' })
  ).resolves.toMatchObject({ type: 'ack', requestType: 'open' });
  await expect(page.locator('.editor-panel').first()).toBeVisible({ timeout: 15000 });
  await expect(page.locator('.editor-panel .view-lines').first()).toContainText('open me', {
    timeout: 15000,
  });
});
//=============================================================================
test('multiline paste remains editable and preserves line breaks', async ({ page }) => {
  const input = page.locator('.console-input').first();
  await input.click();
  await input.evaluate((element: HTMLTextAreaElement) => {
    const clipboard = new DataTransfer();
    clipboard.setData('text', 'for k = 1:3\r\n  value = "text"\r\nend % loop');
    element.dispatchEvent(
      new ClipboardEvent('paste', { clipboardData: clipboard, bubbles: true, cancelable: true })
    );
  });
  await expect(input).toHaveValue('for k = 1:3\n  value = "text"\nend % loop');
  await expect(input).toHaveAttribute('rows', '3');
  await expect(page.locator('.console-input-highlight .console-syntax--keyword')).toHaveText([
    'for',
    'end',
  ]);
  await expect(page.locator('.console-input-highlight .console-syntax--string')).toHaveText(
    '"text"'
  );
  await expect(page.locator('.console-input-highlight .console-syntax--comment')).toHaveText(
    '% loop'
  );
  const promptLayers = await input.evaluate((element) => {
    const stack = element.parentElement;
    const highlight = stack?.querySelector<HTMLElement>('.console-input-highlight');
    if (!stack || !highlight) return null;
    return {
      isolation: getComputedStyle(stack).isolation,
      inputZIndex: Number(getComputedStyle(element).zIndex),
      highlightZIndex: Number(getComputedStyle(highlight).zIndex),
      highlightColor: getComputedStyle(highlight).color,
      highlightWidth: highlight.getBoundingClientRect().width,
      highlightHeight: highlight.getBoundingClientRect().height,
    };
  });
  expect(promptLayers).not.toBeNull();
  expect(promptLayers?.isolation).toBe('isolate');
  expect(promptLayers?.highlightZIndex).toBeGreaterThan(promptLayers?.inputZIndex ?? 0);
  expect(promptLayers?.highlightColor).not.toBe('rgba(0, 0, 0, 0)');
  expect(promptLayers?.highlightWidth).toBeGreaterThan(0);
  expect(promptLayers?.highlightHeight).toBeGreaterThan(0);
  await expect.poll(() => terminalText(page)).not.toContain('for k = 1:3');

  await input.press('Shift+Enter');
  await expect(input).toHaveValue('for k = 1:3\n  value = "text"\nend % loop\n');
});
//=============================================================================
test('evaluates a scalar into ans', async ({ page }) => {
  await runLine(page, '42');
  await expect.poll(() => terminalText(page)).toContain('42');
  await expect.poll(() => terminalText(page)).toContain('ans');
});
//=============================================================================
test('Tab opens a completion popup listing matches', async ({ page }) => {
  await focusTerminal(page);
  await page.keyboard.type('si');
  await page.keyboard.press('Tab');
  const popup = page.locator('.console-popup');
  await expect(popup).toBeVisible();
  await expect(popup).toContainText('sin');
});
//=============================================================================
test('evaluates arithmetic (1+1 -> 2)', async ({ page }) => {
  await runLine(page, '1+1');
  await expect.poll(() => terminalText(page)).toContain('2');
});
//=============================================================================
test('a duplicate prompt push is deduped (no ">> >>" at boot)', async ({ page }) => {
  const host = page.locator('.console-host');
  await expect.poll(() => host.innerText()).toContain('>>');
  // Boot race: the engine's first getLine pushes the prompt AND term.sync asks
  // it to resend; the second identical prompt must not double up.
  await page.evaluate(() =>
    (window as unknown as { __nelsonPush?: (c: string, p: string) => void }).__nelsonPush?.(
      'term.prompt',
      JSON.stringify({ prompt: '>> ' })
    )
  );
  const count = await host.evaluate((el) => (el.textContent?.match(/>>/g) ?? []).length);
  expect(count).toBe(1);
});
//=============================================================================
test('completes object properties after a dot (a.V<TAB>)', async ({ page }) => {
  await runLine(page, 'a = surf(peaks)');
  await focusTerminal(page);
  await page.keyboard.type('a.V');
  await page.keyboard.press('Tab');
  await expect.poll(() => inputText(page)).toContain('a.Visible');
});
//=============================================================================
test('right-click opens the context menu', async ({ page }) => {
  await page.locator('.console-panel').click({ button: 'right' });
  const menu = page.locator('.console-menu');
  await expect(menu).toBeVisible();
  await expect(menu).toContainText('Paste');
  await expect(menu).toContainText('Clear');
});
//=============================================================================
test('context-menu Clear leaves a single prompt (no ">> >>")', async ({ page }) => {
  await page.locator('.console-panel').click({ button: 'right' });
  await page.locator('.console-menu__item', { hasText: 'Clear' }).click();
  // the prompt lives in the input stack, which terminalText() strips: read the host
  const host = page.locator('.console-host');
  await expect.poll(() => host.innerText()).toContain('>>');
  await expect
    .poll(() => host.evaluate((el) => (el.textContent?.match(/>>/g) ?? []).length))
    .toBe(1);
});
//=============================================================================
test('Up recalls persistent history immediately (seeded from history.list)', async ({ page }) => {
  await focusTerminal(page);
  await page.waitForTimeout(300); // let the async history seed land
  await page.keyboard.press('ArrowUp');
  // The mock seeds history with 'plot(x)' as the most recent entry; it is
  // recalled into the input line (not echoed to the transcript).
  await expect.poll(() => inputText(page)).toContain('plot(x)');
});
//=============================================================================
test('Up cycles history filtered by the typed prefix', async ({ page }) => {
  // Assignments (no figures) so the Command Window stays the active panel.
  await runLine(page, 'aa = 1');
  await runLine(page, 'aa = 2');
  await runLine(page, 'bb = 3');
  await focusTerminal(page);
  await page.keyboard.type('aa');
  await page.keyboard.press('ArrowUp');
  await expect.poll(() => inputText(page)).toBe('aa = 2');
  await page.keyboard.press('ArrowUp');
  await expect.poll(() => inputText(page)).toBe('aa = 1');
});
//=============================================================================
test('surf renders a native 3D figure canvas', async ({ page }) => {
  await runLine(page, 'surf(peaks)');
  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible();
  const box = await canvas.boundingBox();
  expect(box && box.width).toBeGreaterThan(50);
  expect(box && box.height).toBeGreaterThan(50);
});
//=============================================================================
test('a newly created figure becomes the active dock tab', async ({ page }) => {
  await runLine(page, 'surf(peaks)');
  const figureTab = page.locator('.dv-tab', { hasText: /^Figure 1$/ }).first();
  const commandTab = page.locator('.dv-tab', { hasText: 'Command Window' }).first();
  await expect(figureTab).toHaveAttribute('aria-selected', 'true');
  await expect(commandTab).toHaveAttribute('aria-selected', 'false');
  await expect(page.locator('.figure-panel')).toBeVisible();
  await expect(page.locator('.console-panel')).toHaveCount(0);
});
//=============================================================================
test('figure window key events reach the backend from focused child controls', async ({ page }) => {
  await runLine(page, 'surf(peaks)');
  const exportButton = page
    .locator('.figure-panel')
    .getByRole('button', { name: 'Export', exact: true })
    .first();
  await expect(exportButton).toBeVisible();
  await exportButton.focus();
  await page.keyboard.down('w');
  await expect
    .poll(() =>
      page.evaluate(
        () =>
          (window as unknown as { __mockLastFigureKeyEvent?: { pressed: boolean; key: string } })
            .__mockLastFigureKeyEvent
      )
    )
    .toMatchObject({ pressed: true, key: 'W' });
  await page.keyboard.up('w');
  await expect
    .poll(() =>
      page.evaluate(
        () =>
          (window as unknown as { __mockLastFigureKeyEvent?: { pressed: boolean; key: string } })
            .__mockLastFigureKeyEvent
      )
    )
    .toMatchObject({ pressed: false, key: 'W' });
});
//=============================================================================
test('closing a figure destroys it and a later surf recreates its panel', async ({ page }) => {
  await runLine(page, 'surf(peaks)');
  await expect(page.locator('.figure-panel')).toBeVisible();

  const figureTab = page.locator('.dv-default-tab', { hasText: /^Figure \d+$/ }).first();
  await figureTab.click({ button: 'right' });
  await page.locator('.ctx-menu__item', { hasText: 'Close panel' }).click();
  await expect(page.locator('.figure-panel')).toHaveCount(0);

  await runLine(page, 'surf(peaks)');
  await expect(page.locator('.figure-panel')).toBeVisible();
});
//=============================================================================
test('rotating the 3D figure syncs the axes View to the backend', async ({ page }) => {
  await runLine(page, 'surf(peaks)');
  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible();
  const box = await canvas.boundingBox();
  if (!box) throw new Error('no figure canvas');
  const cx = box.x + box.width / 2;
  const cy = box.y + box.height / 2;
  // Drag to rotate: onView fires on mouse-up, sending figure.setView.
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + 90, cy + 30, { steps: 8 });
  await page.mouse.up();
  await expect
    .poll(() =>
      page.evaluate(
        () => (window as unknown as { __mockLastView?: { azimuth: number } }).__mockLastView
      )
    )
    .toMatchObject({ azimuth: expect.any(Number), elevation: expect.any(Number) });
});
//=============================================================================
test('figure toolbar exposes export and restore-view actions', async ({ page }) => {
  await runLine(page, 'surf(peaks)');
  const figure = page.locator('.figure-panel');
  await expect(figure.getByRole('button', { name: 'Export', exact: true })).toBeVisible();
  await figure.getByRole('button', { name: 'More figure actions', exact: true }).click();
  await expect(figure.getByRole('button', { name: 'Save as…', exact: true })).toBeVisible();
  await expect(figure.getByRole('button', { name: 'Print figure', exact: true })).toBeVisible();
  await page.keyboard.press('Escape');
  await showFigureBars(figure);
  await figure.getByRole('combobox', { name: 'View preset' }).selectOption('xz');
  await figure.getByRole('combobox', { name: 'View preset' }).selectOption('3d');
  await expect(page.locator('.figure-panel canvas').first()).toBeVisible();
});
//=============================================================================
test('2D figure Pan mode toggles and dragging pans the axes', async ({ page }) => {
  await runLine(page, 'plot(1:10)');
  await showFigureBars(page.locator('.figure-panel').first());
  const panBtn = page.locator('button[title^="Pan ("]');
  await expect(panBtn).toBeVisible();
  await panBtn.click();
  const overlay = page.locator('.figure-panel__pan');
  await expect(overlay).toBeVisible();
  const box = await overlay.boundingBox();
  if (!box) throw new Error('pan overlay has no box');
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 80, box.y + box.height / 2 + 20, { steps: 5 });
  await page.mouse.up();
  const lastPan = await page.evaluate(
    () => (window as unknown as { __mockLastPan?: { dxFrac: number } }).__mockLastPan
  );
  expect(lastPan).toBeTruthy();
  expect(Math.abs(lastPan!.dxFrac)).toBeGreaterThan(0);
});
//=============================================================================
test('object display "Show all properties" link expands on click', async ({ page }) => {
  // xterm's DOM renderer uses non-breaking spaces inside links, so normalize.
  const normText = async () => (await terminalText(page)).replace(/\s+/g, ' ');
  await runLine(page, 'a = surf(peaks)');
  await expect.poll(normText).toContain('Show all properties');
  await page
    .getByText(/Show.all.properties/)
    .first()
    .click();
  await expect.poll(normText).toContain('XData');
});
//=============================================================================
test('Files: uploading a file writes it into the current folder', async ({ page }) => {
  await expect(page.locator('.file-list')).toBeVisible();
  await page
    .locator('.files__path input[type="file"]')
    .setInputFiles({ name: 'hello.txt', mimeType: 'text/plain', buffer: Buffer.from('hi') });
  const up = await expect
    .poll(() =>
      page.evaluate(
        () => (window as unknown as { __mockLastUpload?: { path: string } }).__mockLastUpload
      )
    )
    .not.toBeUndefined();
  void up;
  const rec = await page.evaluate(
    () => (window as unknown as { __mockLastUpload?: { path: string } }).__mockLastUpload
  );
  expect(rec?.path).toContain('hello.txt');
});
//=============================================================================
test('prompts use a non-blocking modal (not a native dialog that stalls the heartbeat)', async ({
  page,
}) => {
  // A native window.prompt() would freeze the JS event loop and starve the
  // heartbeat. Playwright auto-dismisses a native dialog, so no .dialog-card
  // would appear.
  let nativeDialog = false;
  page.on('dialog', (d) => {
    nativeDialog = true;
    void d.dismiss();
  });
  await runLine(page, 'surf(peaks)');
  await page
    .locator('.figure-panel')
    .getByRole('button', { name: 'More figure actions', exact: true })
    .click();
  await page.getByRole('button', { name: 'Save as…', exact: true }).click();
  const card = page.locator('.dialog-card');
  await expect(card).toBeVisible();
  await expect(card).toContainText('Save figure to file');
  await card.locator('input[type="text"]').fill('from_modal.png');
  await card.getByRole('button', { name: 'OK' }).click();
  await expect(card).toBeHidden();
  expect(nativeDialog).toBe(false);
});
//=============================================================================
test('File > New > New Figure opens a figure panel', async ({ page }) => {
  await page.locator('.menubar').getByText('File', { exact: true }).click();
  await page.locator('.menu').getByText('New Figure', { exact: true }).click();
  await expect(page.locator('.figure-panel').first()).toBeVisible();
});
//=============================================================================
test('File > Set Path… lists the search path with remove/add/save', async ({ page }) => {
  await page.locator('.menubar').getByText('File', { exact: true }).click();
  await page.locator('.menu').getByText('Set Path…', { exact: true }).click();
  const card = page.locator('.setpath-card');
  await expect(card).toBeVisible();
  await expect(card.locator('.setpath-list__item')).toHaveCount(3);
  await expect(card).toContainText('/workspace');
  await expect(card.getByRole('button', { name: 'Add Folder…' })).toBeVisible();
  await expect(card.getByRole('button', { name: 'Save' })).toBeVisible();
  await card.getByRole('button', { name: 'Close' }).click();
  await expect(card).toBeHidden();
});
//=============================================================================
test('File > Import Data… previews the file and imports as a table', async ({ page }) => {
  await page.locator('.menubar').getByText('File', { exact: true }).click();
  await page.locator('.menu').getByText('Import Data…', { exact: true }).click();
  const card = page.locator('.import-card');
  await expect(card).toBeVisible();
  await card.getByRole('button', { name: 'Browse…' }).click();
  await expect(card.locator('.import-browse__list li').first()).toBeVisible();
  await card.locator('.import-input').first().fill('data.csv');
  // Preview loads via fs.download (mock returns "mock file contents").
  await expect(card.locator('.import-preview')).toContainText('mock file contents');
  await expect(card.locator('select')).toContainText('Table');
  await card.getByRole('button', { name: 'Import' }).click();
  await expect(card).toBeHidden();
});
//=============================================================================
test('Help > Documentation opens the doc panel with searchable help', async ({ page }) => {
  await page.locator('.menubar').getByText('Help', { exact: true }).click();
  await page.locator('.menu').getByText('Documentation', { exact: true }).click();
  const panel = page.locator('.help-panel');
  await expect(panel).toBeVisible();
  await panel.locator('.help-panel__search').fill('sin');
  await panel.locator('.help-panel__search').press('Enter');
  await expect(panel.locator('.help-panel__text')).toContainText('documentation for sin');
  const seeAlso = panel.locator('.help-link', { hasText: 'cos' });
  await expect(seeAlso).toBeVisible();
  await seeAlso.click();
  await expect(panel.locator('.help-panel__text')).toContainText('documentation for cos');
});
//=============================================================================
test('plots gallery offers plot types filtered by shape and runs one', async ({ page }) => {
  // A 1x9 vector: line/bar/pie apply, surf/mesh do not.
  await page.evaluate(() =>
    window.dispatchEvent(
      new CustomEvent('nelson-webview.open-plots', { detail: { name: 'v', rows: 1, cols: 9 } })
    )
  );
  const card = page.locator('.plots-card');
  await expect(card).toBeVisible();
  await expect(card.locator('.plots-grid__item', { hasText: 'Pie' })).toBeVisible();
  await expect(card.locator('.plots-grid__item', { hasText: 'Surface' })).toHaveCount(0);
  await card.locator('.plots-grid__item', { hasText: 'Line' }).click();
  await expect(card).toBeHidden();
  await expect(page.locator('.figure-panel').first()).toBeVisible();
});
//=============================================================================
test('Profiler panel shows the per-function report', async ({ page }) => {
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Profiler', { exact: true }).click();
  const prof = page.locator('.profiler');
  await expect(prof).toBeVisible();
  await expect(prof.locator('tbody tr')).toHaveCount(2);
  await expect(prof).toContainText('myfun');
  const toggle = prof.locator('.profiler__btn').first();
  const exportButton = prof.getByRole('button', { name: 'Export' }).first();
  await expect(toggle).toContainText('Start');
  await expect(exportButton).toBeEnabled();
  await toggle.click();
  await expect(toggle).toContainText('Stop');
  await expect(exportButton).toBeDisabled();
  await expect(prof.locator('.profiler__on')).toBeVisible();
  await toggle.click();
  await expect(toggle).toContainText('Start');
  await expect(exportButton).toBeEnabled();
  await page.evaluate(() => {
    window.open = (() => null) as typeof window.open;
  });
  await exportButton.click();
  const exportDialog = page.locator('.profiler__export-dialog');
  await expect(exportDialog).toBeVisible();
  await expect(exportDialog.locator('.dialog-file__cwd')).toContainText('/workspace');
  await exportDialog.locator('.dialog-btn--default').click();
  await expect(prof.locator('.profiler__hint')).toContainText('profile_result/index.html');
  await expect(prof.locator('.profiler__hint a')).toHaveAttribute(
    'href',
    /profile_result\/index\.html$/
  );
  await prof.locator('tbody tr.profiler__row--file').first().dblclick();
  await expect(page.locator('.editor-panel').first()).toBeVisible();
});
//=============================================================================
test('Profiler toggle syncs with profile on/off from the console', async ({ page }) => {
  await runLine(page, 'profile on');
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Profiler', { exact: true }).click();
  // The panel reads the state (via profile.info's `on`) and shows Stop.
  await expect(page.locator('.profiler .profiler__btn').first()).toContainText('Stop');
});
//=============================================================================
test('command palette (Ctrl+Shift+P) runs an action', async ({ page }) => {
  await page.keyboard.press('Control+Shift+P');
  const palette = page.locator('.palette');
  await expect(palette).toBeVisible();
  await palette.locator('.palette__input').fill('workspace');
  await expect(palette.locator('.palette__item').first()).toContainText('Workspace');
  await palette.locator('.palette__input').press('Enter');
  await expect(palette).toBeHidden();
  await expect(page.locator('.dv-tab', { hasText: 'Workspace' })).toBeVisible();
});
//=============================================================================
test('editor(...) opens the Monaco editor panel', async ({ page }) => {
  await runLine(page, "editor('demo.m')");
  await expect(page.locator('.editor-panel').first()).toBeVisible();
});
//=============================================================================
test('editor autocompletion shows engine suggestions', async ({ page }) => {
  await runLine(page, "editor('demo.m')");
  await expect(page.locator('.editor-panel').first()).toBeVisible();
  await page.locator('.editor-panel .monaco-editor .view-lines').click();
  await page.keyboard.type('si');
  // The mock returns sin/sind/sinh for the prefix "si".
  const suggest = page.locator('.suggest-widget').first();
  await expect(suggest).toBeVisible({ timeout: 4000 });
  await expect(suggest).toContainText('sin');
});
//=============================================================================
test('editor Go to Symbol lists functions and sections', async ({ page }) => {
  await runLine(page, "editor('outline.m')");
  await expect(page.locator('.editor-panel').first()).toBeVisible();
  await page.locator('.editor-panel .monaco-editor .view-lines').click();
  await page.keyboard.press('Control+Shift+O');
  const quick = page.locator('.quick-input-widget').first();
  await expect(quick).toBeVisible({ timeout: 4000 });
  await expect(quick).toContainText('myfun');
  await expect(quick).toContainText('Section A');
  await page.keyboard.press('Escape');
});
//=============================================================================
test('editor grouped ribbon toolbar shows its groups', async ({ page }) => {
  await runLine(page, "editor('demo.m')");
  const ribbon = page.locator('.editor-ribbon');
  await expect(ribbon).toBeVisible();
  for (const label of ['FILE', 'NAVIGATE', 'CODE', 'ANALYZE', 'SECTION', 'RUN']) {
    await expect(
      ribbon.locator(
        `.editor-ribbon__label:visible:text-is("${label}"), ` +
          `.editor-ribbon__collapsed-btn:visible:has-text("${label}")`
      )
    ).toBeVisible();
  }
});
//=============================================================================
test('editor ribbon collapses groups into dropdowns when narrow', async ({ page }) => {
  await page.setViewportSize({ width: 480, height: 800 });
  await runLine(page, "editor('demo.m')");
  const ribbon = page.locator('.editor-ribbon');
  await expect(ribbon).toBeVisible();
  const collapsed = ribbon.locator('.editor-ribbon__collapsed-btn').first();
  await expect(collapsed).toBeVisible();
  await collapsed.click();
  const popup = ribbon.locator('.editor-ribbon__popup');
  await expect(popup).toBeVisible();
  // The popup drops below the ribbon; a real click must reach an item (an
  // overflow-clipped popup would not receive pointer events). Clicking also
  // dismisses the popup.
  const item = popup.locator('.editor-ribbon__popup-item').first();
  await expect(item).toBeVisible();
  await item.click();
  await expect(popup).toBeHidden();
});
//=============================================================================
test('editor ribbon re-lays-out live as the panel is resized (follows shrink & grow)', async ({
  page,
}) => {
  await page.setViewportSize({ width: 1700, height: 900 });
  await runLine(page, "editor('demo.m')");
  const ribbon = page.locator('.editor-ribbon');
  await expect(ribbon).toBeVisible();
  const collapsedCount = () => ribbon.locator('.editor-ribbon__collapsed-btn').count();
  // The ribbon box must always track the panel width (never stay pinned to its
  // expanded content width, the flexbox min-width:auto trap), otherwise the
  // ResizeObserver stops firing and the groups don't collapse.
  const pinned = () =>
    ribbon.evaluate((el) => {
      const p = el.closest('.editor-panel') as HTMLElement;
      return (el as HTMLElement).clientWidth > (p?.clientWidth ?? 0);
    });

  await expect.poll(collapsedCount).toBeLessThanOrEqual(2);
  expect(await pinned()).toBe(false);
  const wideCount = await collapsedCount();

  // shrinking collapses more groups than the wide layout, growing restores it;
  // stay above MOBILE_MAX_WIDTH (760), the compact layout drops the editor panel
  await page.setViewportSize({ width: 800, height: 900 });
  await expect.poll(collapsedCount).toBeGreaterThan(wideCount);
  expect(await pinned()).toBe(false);

  await page.setViewportSize({ width: 1700, height: 900 });
  await expect.poll(collapsedCount).toBe(wideCount);
  expect(await pinned()).toBe(false);
});
//=============================================================================
test('editor ribbon folds groups into a "»" overflow menu when very narrow (nothing is cut off)', async ({
  page,
}) => {
  await page.setViewportSize({ width: 560, height: 800 });
  await runLine(page, "editor('demo.m')");
  const ribbon = page.locator('.editor-ribbon');
  await expect(ribbon).toBeVisible();
  // The bar must never spill past its box (which would hide groups behind the
  // neighbouring panel); instead a trailing "»" gathers what doesn't fit.
  const overflow = ribbon.locator('.editor-ribbon__overflow');
  await expect(overflow).toBeVisible();
  expect(
    await ribbon.evaluate(
      (el) => (el as HTMLElement).scrollWidth <= (el as HTMLElement).clientWidth + 2
    )
  ).toBe(true);
  await overflow.locator('.editor-ribbon__collapsed-btn').click();
  const popup = ribbon.locator('.editor-ribbon__overflow .editor-ribbon__popup');
  await expect(popup).toBeVisible();
  await expect(popup.locator('.editor-ribbon__popup-heading').first()).toBeVisible();
  const item = popup.locator('.editor-ribbon__popup-item').first();
  await expect(item).toBeVisible();
  await item.click(); // reachable; an overflow-clipped item would not receive this
  await expect(popup).toBeHidden();
});
//=============================================================================
test('editor right-click menu offers the Nelson actions', async ({ page }) => {
  await runLine(page, "editor('demo.m')");
  await expect(page.locator('.editor-panel').first()).toBeVisible();
  await page.locator('.editor-panel .monaco-editor .view-lines').click({ button: 'right' });
  const menu = page.locator('.monaco-menu');
  await expect(menu).toBeVisible();
  await expect(menu).toContainText('Run Section');
  await expect(menu).toContainText('Evaluate Selection');
  await expect(menu).toContainText('Help on Selection');
});
//=============================================================================
test('code analyzer surfaces issues (error + warning) in the editor toolbar', async ({ page }) => {
  await page.setViewportSize({ width: 2000, height: 1000 }); // keep the ribbon expanded
  await runLine(page, "editor('lint.m')");
  await expect(page.locator('.editor-panel').first()).toBeVisible();
  // The analyzer runs on open: line 2 `bug` → 1 error, line 3 (>80 chars) → 1 warning.
  const issues = page.locator('.editor-toolbar__issues');
  await expect(issues).toBeVisible();
  await expect(issues.locator('.editor-issues__err')).toContainText('1');
  await expect(issues.locator('.editor-issues__warn')).toContainText('1');
  await expect(page.locator('.editor-panel .squiggly-error').first()).toBeVisible();
  await issues.click();
  const problems = page.locator('.editor-problems');
  await expect(problems).toBeVisible();
  await expect(problems.locator('.editor-problems__item')).toHaveCount(2);
  await expect(problems).toContainText('Suspicious token');
  await expect(problems).toContainText('Line exceeds 80 characters');
  await problems.locator('.editor-problems__item').first().click();
  // The error diagnostic offers a Fix (replace 'bug' with 'fixed').
  const fix = problems.locator('.editor-problems__fix').first();
  await expect(fix).toBeVisible();
  await fix.click();
  await expect(page.locator('.editor-panel .view-lines')).toContainText('fixed');
});
//=============================================================================
test('switches to the light theme', async ({ page }) => {
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Light', { exact: true }).click();
  await expect(page.locator('.nelson-shell--light')).toHaveCount(1);
});
//=============================================================================
test('status bar shows Ready; toolbar owns current folder and Stop', async ({ page }) => {
  const status = page.locator('.statusbar');
  await expect(status).toBeVisible();
  await expect(status.locator('.statusbar__state')).toHaveText('Ready');
  await expect(page.getByRole('button', { name: 'Stop the current execution' })).toBeDisabled();
  await expect(page.getByRole('combobox', { name: 'Current directory' })).not.toHaveValue('');
});
//=============================================================================
test('File and Help menus expose desktop actions', async ({ page }) => {
  await page.locator('.menubar').getByText('File', { exact: true }).click();
  await expect(page.locator('.menu')).toContainText('Execute');
  await expect(page.locator('.menu')).toContainText('Change current directory');
  await page.locator('.menubar').getByText('Help', { exact: true }).click();
  await expect(page.locator('.menu')).toContainText('Documentation');
});
//=============================================================================
test('Command History right-click menu offers Evaluate and Clear', async ({ page }) => {
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Command History').click();
  await page.locator('.history-list li').first().click({ button: 'right' });
  const menu = page.locator('.ctx-menu');
  await expect(menu).toBeVisible();
  await expect(menu).toContainText('Evaluate');
  await expect(menu).toContainText('Clear history');
});
//=============================================================================
test('Workspace right-click menu offers Delete/Plot', async ({ page }) => {
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Workspace', { exact: true }).click();
  await page.locator('.browser__table tbody tr').first().click({ button: 'right' });
  const menu = page.locator('.ctx-menu');
  await expect(menu).toBeVisible();
  await expect(menu).toContainText('Delete');
  await expect(menu).toContainText('Open');
});
//=============================================================================
const lastAction = (page: Page) =>
  page.evaluate(() => (window as unknown as { __mockLastAction?: unknown }).__mockLastAction);
test('uidemo builds real interactive HTML controls', async ({ page }) => {
  await runLine(page, 'uidemo');
  const panel = page.locator('.figure-panel');
  await expect(panel.getByRole('button', { name: 'Push me' })).toBeVisible();
  await expect(panel.locator('input[type="checkbox"]')).toBeVisible();
  await expect(panel.locator('input[type="range"]')).toBeVisible();
  await expect(panel.locator('select')).toBeVisible();
  await expect(panel.locator('select option')).toHaveCount(2);
});
//=============================================================================
test('pixel uicontrols keep their authored layout when the figure panel resizes', async ({
  page,
}) => {
  await runLine(page, 'uidemo');
  const canvas = page.locator('.figure-panel__canvas');
  const button = page.locator('.figure-panel').getByRole('button', { name: 'Push me' });
  await expect(button).toBeVisible();
  // the figure takes the panel size (figure.setSize); pixel controls keep their
  // authored size and stay anchored to the bottom-left corner (Position units)
  await expect.poll(async () => (await canvas.boundingBox())?.width ?? 0).toBeGreaterThan(640);
  await expect
    .poll(async () => {
      const buttonBox = await button.boundingBox();
      return buttonBox?.width ?? 0;
    })
    .toBeCloseTo(110, 0);
  // authored at y = 20 from the top of a 400 px figure: 350 px above the bottom
  await expect
    .poll(async () => {
      const canvasBox = await canvas.boundingBox();
      const buttonBox = await button.boundingBox();
      if (!canvasBox || !buttonBox) return -1;
      return canvasBox.y + canvasBox.height - (buttonBox.y + buttonBox.height);
    })
    .toBeCloseTo(350, 0);
});
//=============================================================================
test('clicking a uicontrol reports the action to the backend', async ({ page }) => {
  await runLine(page, 'uidemo');
  await page.locator('.figure-panel').getByRole('button', { name: 'Push me' }).click();
  await expect.poll(() => lastAction(page)).toMatchObject({ handle: 9001 });
});
//=============================================================================
test('checkbox state round-trips through the backend', async ({ page }) => {
  await runLine(page, 'uidemo');
  const box = page.locator('.figure-panel input[type="checkbox"]');
  await box.click();
  await expect.poll(() => lastAction(page)).toMatchObject({ handle: 9002, value: 1 });
  // The mock stored the value; after the refresh round-trip it stays checked.
  await expect(box).toBeChecked();
});
//=============================================================================
test('figure menu bar opens and fires the selected uimenu', async ({ page }) => {
  await runLine(page, 'uidemo');
  const bar = page.locator('.figure-menubar');
  await expect(bar).toBeVisible();
  await bar.getByText('Actions', { exact: true }).click();
  await page.getByText('Do Thing', { exact: true }).click();
  await expect.poll(() => lastAction(page)).toMatchObject({ handle: 9201 });
});
//=============================================================================
test('right-click on a control opens its uicontextmenu and fires the item', async ({ page }) => {
  await runLine(page, 'uidemo');
  // the overlay hit-tests controls in stage coordinates: wait for the resize to settle
  await waitForFigureLayout(page.locator('.figure-panel'));
  await page
    .locator('.figure-panel')
    .getByRole('button', { name: 'Push me' })
    .click({ button: 'right' });
  const copy = page.getByText('Copy', { exact: true });
  await expect(copy).toBeVisible();
  await copy.click();
  await expect.poll(() => lastAction(page)).toMatchObject({ handle: 9101 });
});
//=============================================================================
test('figure.frame pushes apply live (animation streaming)', async ({ page }) => {
  await runLine(page, 'animate_demo');
  // The last of the three streamed frames shows without a refresh round-trip.
  await expect(page.locator('.figure-panel').getByText('frame-3')).toBeVisible();
});
//=============================================================================
test('Command History keeps its end in view after a command', async ({ page }) => {
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Command History', { exact: true }).click();
  await runLine(page, 'x_scroll = 1');
  const list = page.locator('.history-list');
  await expect(list.getByText('x_scroll = 1')).toBeVisible();
  const ok = await page.evaluate(() => {
    const el = document.querySelector('.history-list') as HTMLElement;
    if (!el) return false;
    const last = el.querySelector('li:last-child') as HTMLElement;
    const lr = last.getBoundingClientRect();
    const cr = el.getBoundingClientRect();
    return lr.bottom <= cr.bottom + 2 && lr.top >= cr.top - 2;
  });
  expect(ok).toBe(true);
});
//=============================================================================
test('inputdlg opens a modal and returns the typed value', async ({ page }) => {
  await runLine(page, 'inputdlg');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await expect(dialog.locator('.dialog-card__title')).toHaveText('Input');
  await dialog.locator('input[type="text"]').fill('Nelson');
  await dialog.getByRole('button', { name: 'OK' }).click();
  await expect(dialog).toBeHidden();
  await expect.poll(() => terminalText(page)).toContain('Nelson');
});
//=============================================================================
test('inputdlg Cancel returns nothing', async ({ page }) => {
  await runLine(page, 'inputdlg');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await dialog.getByRole('button', { name: 'Cancel' }).click();
  await expect(dialog).toBeHidden();
  await expect.poll(() => terminalText(page)).toContain('cancelled');
});
//=============================================================================
test('uialert shows a message with an OK button', async ({ page }) => {
  await runLine(page, 'uialert');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await expect(dialog.locator('.dialog-card__message')).toContainText('Something happened');
  await dialog.getByRole('button', { name: 'OK' }).click();
  await expect(dialog).toBeHidden();
});
//=============================================================================
test('uiconfirm returns the pressed button', async ({ page }) => {
  await runLine(page, 'uiconfirm');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await dialog.getByRole('button', { name: 'No' }).click();
  await expect(dialog).toBeHidden();
  await expect.poll(() => terminalText(page)).toContain('No');
});
//=============================================================================
test('uiprogressdlg creates, updates, stacks and closes non-modal progress cards', async ({
  page,
}) => {
  await runLine(
    page,
    "p1 = uiprogressdlg(1, 'Title', 'Import A', 'Message', 'Reading', 'Value', 0.25)"
  );
  const cards = page.locator('.progress-dialog');
  await expect(cards).toHaveCount(1);
  await expect(cards.first()).toContainText('Import A');
  await expect(cards.first()).toContainText('Reading');
  await expect(cards.first()).toContainText('25%');

  await runLine(
    page,
    "p2 = uiprogressdlg(1, 'Title', 'Import B', 'Message', 'Waiting', 'Indeterminate', true)"
  );
  await expect(cards).toHaveCount(2);
  await expect(cards.filter({ hasText: 'Import B' }).getByRole('progressbar')).toHaveAttribute(
    'aria-valuetext',
    'In progress'
  );

  await runLine(page, "set(p1, 'value', 60)");
  await runLine(page, "set(p1, 'labelText', 'Writing')");
  await expect(cards.filter({ hasText: 'Import A' })).toContainText('Writing');
  await expect(cards.filter({ hasText: 'Import A' })).toContainText('60%');

  await runLine(page, 'progress_probe = 7');
  await expect.poll(() => terminalText(page)).toContain('7');

  await runLine(page, 'delete(p1)');
  await expect(cards).toHaveCount(1);
  await expect(cards.first()).toContainText('Import B');
  await runLine(page, 'delete(p2)');
  await expect(cards).toHaveCount(0);
});
//=============================================================================
async function openVariableA(page: Page) {
  await page.locator('.browser__table tbody tr').first().dblclick();
  await expect(page.locator('.var-editor')).toBeVisible();
}
//=============================================================================
async function openVariableNamed(page: Page, varName: string) {
  await page
    .locator('.browser__table tbody tr')
    .filter({ has: page.locator('td', { hasText: new RegExp(`^${varName}$`) }) })
    .first()
    .dblclick();
  await expect(page.locator('.var-editor')).toBeVisible();
}
//=============================================================================
test('variable editor opens A as a 3x3 grid', async ({ page }) => {
  await openVariableA(page);
  await expect(page.locator('.var-editor__bar')).toContainText('3×3');
  await expect(page.locator('.grid tbody tr')).toHaveCount(3);
});
//=============================================================================
test('logical variable renders as true/false (Qt parity)', async ({ page }) => {
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Workspace', { exact: true }).click();
  const row = page
    .locator('.browser__table tbody tr')
    .filter({ has: page.locator('td', { hasText: /^L$/ }) });
  await row.first().dblclick();
  await expect(page.locator('.var-editor')).toBeVisible();
  const body = page.locator('.grid tbody');
  await expect(body).toContainText('true');
  await expect(body).toContainText('false');
  await expect(body).not.toContainText(/^[01]$/);
});
//=============================================================================
test('a <missing> string element renders in italic', async ({ page }) => {
  await runLine(page, 'str_demo');
  await openVariableNamed(page, 'sa');
  const missing = page.locator('.var-editor__missing');
  await expect(missing).toHaveText('<missing>');
  await expect(missing).toHaveCSS('font-style', 'italic');
});
//=============================================================================
test('a scalar struct field can be edited in place', async ({ page }) => {
  await runLine(page, 'struct_demo');
  await openVariableNamed(page, 'st');
  // Fields are columns: name | value | data. Edit the scalar "value" (42 → 99).
  const valueCell = page.locator('.grid tbody tr').first().locator('.grid__cell').nth(1);
  await expect(valueCell).toHaveText('42');
  await valueCell.dblclick();
  const input = page.locator('.grid__input');
  await input.fill('99');
  await input.press('Enter');
  await expect(valueCell).toHaveText('99');
});
//=============================================================================
test('Create-variable button is present in the editor toolbar', async ({ page }) => {
  await openVariableA(page);
  await expect(
    page.locator('.var-editor__btn[title="Create variable from selection"]')
  ).toBeVisible();
});
//=============================================================================
test('editing a cell writes the new value back', async ({ page }) => {
  await openVariableA(page);
  const cell = page.locator('.grid tbody tr').first().locator('.grid__cell').first();
  await cell.dblclick();
  const input = page.locator('.grid__input');
  await input.fill('7');
  await input.press('Enter');
  await expect(cell).toHaveText('7');
});
//=============================================================================
test('insert row grows the grid and undo reverts it', async ({ page }) => {
  await openVariableA(page);
  await page
    .locator('.grid tbody tr')
    .first()
    .locator('.grid__cell')
    .first()
    .click({ button: 'right' });
  await page.locator('.console-menu').getByText('Insert row below', { exact: true }).click();
  await expect(page.locator('.grid tbody tr')).toHaveCount(4);
  await expect(page.locator('.var-editor__bar')).toContainText('4×3');
  await page.locator('.var-editor__btn[title="Undo (Ctrl+Z)"]').click();
  await expect(page.locator('.grid tbody tr')).toHaveCount(3);
  await expect(page.locator('.var-editor__bar')).toContainText('3×3');
});
//=============================================================================
test('delete column shrinks the grid', async ({ page }) => {
  await openVariableA(page);
  await page
    .locator('.grid tbody tr')
    .first()
    .locator('.grid__cell')
    .first()
    .click({ button: 'right' });
  await page.locator('.console-menu').getByText('Delete column', { exact: true }).click();
  await expect(page.locator('.var-editor__bar')).toContainText('3×2');
});
//=============================================================================
test('editor toolbar exposes Run / Evaluate / Save', async ({ page }) => {
  await page.setViewportSize({ width: 1900, height: 900 });
  await runLine(page, "editor('demo.m')");
  const ribbon = page.locator('.editor-panel .editor-ribbon').first();
  // FILE and RUN collapse last, so at a wide width these are expanded.
  await expect(ribbon.locator('.ribbon-btn--run')).toBeVisible();
  await expect(ribbon.getByTitle('Save (Ctrl+S)')).toBeVisible();
});
//=============================================================================
test('desktop reload restores tools without reopening editor documents', async ({ page }) => {
  await runLine(page, "editor('demo.m')");
  await expect(page.locator('.editor-panel')).toBeVisible();

  await page.reload();
  await expect(page.locator('.editor-panel')).toHaveCount(0);
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect(page.locator('.file-list')).toBeVisible();
});
//=============================================================================
test('Run executes the file through the engine', async ({ page }) => {
  await runLine(page, "editor('demo.m')");
  const rpanel = page.locator('.editor-panel').first();
  await expect(rpanel.locator('.monaco-editor')).toBeVisible();
  await rpanel.locator('.view-lines').click();
  await rpanel.getByRole('button', { name: 'Run' }).click();
  // Opening the editor fronts its tab; bring the Command Window back to read it.
  await page.locator('.dv-tab').filter({ hasText: 'Command Window' }).click();
  await expect.poll(() => terminalText(page)).toContain('[run] demo.m');
});
//=============================================================================
test('Evaluate sends the current line to the engine', async ({ page }) => {
  await page.setViewportSize({ width: 2000, height: 1000 }); // keep the ribbon expanded
  await runLine(page, "editor('demo.m')");
  const epanel = page.locator('.editor-panel').first();
  await expect(epanel.locator('.monaco-editor')).toBeVisible();
  await epanel.locator('.view-lines').click();
  await page.keyboard.press('Control+A');
  await page.keyboard.type('y_eval = 42');
  await epanel.getByRole('button', { name: 'Evaluate' }).click();
  await page.locator('.dv-tab').filter({ hasText: 'Command Window' }).click();
  await expect.poll(() => terminalText(page)).toContain('y_eval');
  await expect.poll(() => terminalText(page)).toContain('42');
});
//=============================================================================
test('debugger pause shows controls and step advances the line', async ({ page }) => {
  await runLine(page, 'dbg_demo');
  await runLine(page, "editor('demo.m')");
  const bar = page.locator('.editor-debugbar');
  await expect(bar).toBeVisible();
  await expect(bar).toContainText('Paused');
  await expect(bar).toContainText('demo:2');
  await bar.getByRole('button', { name: 'Step' }).click();
  await expect(bar).toContainText('demo:3');
});
//=============================================================================
test('debugger continue clears the debug controls', async ({ page }) => {
  await runLine(page, 'dbg_demo');
  await runLine(page, "editor('demo.m')");
  const bar = page.locator('.editor-debugbar');
  await expect(bar).toBeVisible();
  await bar.getByRole('button', { name: 'Continue' }).click();
  await expect(bar).toBeHidden();
});
//=============================================================================
test('Preferences changes the console font size live', async ({ page }) => {
  await page.locator('.menubar').getByText('Edit', { exact: true }).click();
  await page.locator('.menu').getByText('Preferences…', { exact: true }).click();
  const dialog = page.locator('.prefs-card');
  await expect(dialog).toBeVisible();
  const sizeInput = dialog.locator('.prefs-row input[type="number"]').first();
  await sizeInput.fill('26');
  await dialog.getByRole('button', { name: 'Apply' }).click();
  await expect(dialog).toBeHidden();
  const fs = await page
    .locator('.console-host')
    .first()
    .evaluate((el) => getComputedStyle(el).fontSize);
  expect(fs).toBe('26px');
});
//=============================================================================
test('Preferences persists across reload', async ({ page }) => {
  await page.locator('.menubar').getByText('Edit', { exact: true }).click();
  await page.locator('.menu').getByText('Preferences…', { exact: true }).click();
  const dialog = page.locator('.prefs-card');
  await dialog.locator('.prefs-row input[type="number"]').first().fill('22');
  await dialog.getByRole('button', { name: 'Apply' }).click();
  await page.reload();
  await expect(page.locator('.console-panel')).toBeVisible();
  const fs = await page
    .locator('.console-host')
    .first()
    .evaluate((el) => getComputedStyle(el).fontSize);
  expect(fs).toBe('22px');
});
//=============================================================================
test('File menu lists recently opened files', async ({ page }) => {
  await runLine(page, "editor('demo.m')");
  await expect(page.locator('.editor-panel').first()).toBeVisible();
  await page.locator('.menubar').getByText('File', { exact: true }).click();
  const menu = page.locator('.menu');
  await expect(menu.getByText('Recent Files', { exact: true })).toBeVisible();
  await expect(menu.locator('.menu__recent', { hasText: 'demo.m' })).toBeVisible();
});
//=============================================================================
async function openVarByName(page: Page, name: string) {
  await page.locator('.menubar').getByText('View', { exact: true }).click();
  await page.locator('.menu').getByText('Workspace', { exact: true }).click();
  await page.locator('.browser__table tbody tr', { hasText: name }).first().dblclick();
  await expect(page.locator('.var-editor')).toBeVisible();
}
//=============================================================================
test('variable editor edits a char variable as a string', async ({ page }) => {
  await runLine(page, "s = 'hello'");
  await openVarByName(page, 's');
  await expect(page.locator('.var-editor__bar')).toContainText('char');
  const cell = page.locator('.grid tbody tr').first().locator('.grid__cell').first();
  await expect(cell).toHaveText('hello');
  await cell.dblclick();
  const input = page.locator('.grid__input');
  await input.fill('world');
  await input.press('Enter');
  await expect(cell).toHaveText('world');
});
//=============================================================================
test('variable editor edits a string array cell', async ({ page }) => {
  await runLine(page, 'str_demo');
  await openVarByName(page, 'sa');
  await expect(page.locator('.var-editor__bar')).toContainText('2×2 string');
  const cell = page.locator('.grid tbody tr').first().locator('.grid__cell').first();
  await expect(cell).toHaveText('a');
  await cell.dblclick();
  const input = page.locator('.grid__input');
  await input.fill('zzz');
  await input.press('Enter');
  await expect(cell).toHaveText('zzz');
});
//=============================================================================
test('variable editor shows a scalar struct as a one-row fields grid', async ({ page }) => {
  await runLine(page, 'struct_demo');
  await openVarByName(page, 'st');
  await expect(page.locator('.var-editor__bar')).toContainText('struct');
  const grid = page.locator('.grid');
  await expect(grid.locator('thead')).toContainText('name');
  await expect(grid.locator('thead')).toContainText('value');
  await expect(page.locator('.grid tbody tr')).toHaveCount(1);
  const firstRow = page.locator('.grid tbody tr').first();
  await expect(firstRow).toContainText('Nelson');
  // A non-scalar field ("data" = 1x3 double) stays read-only: no editor input.
  await firstRow.locator('.grid__cell').nth(2).dblclick();
  await expect(page.locator('.grid__input')).toHaveCount(0);
  await firstRow.locator('.grid__cell').nth(1).dblclick();
  await expect(page.locator('.grid__input')).toHaveCount(1);
  await page.locator('.grid__input').press('Escape');
});
//=============================================================================
test('variable editor shows a table as a rows x variables grid', async ({ page }) => {
  await runLine(page, 'table_demo');
  await openVarByName(page, 'tb');
  await expect(page.locator('.var-editor__bar')).toContainText('3×2 table');
  const grid = page.locator('.grid');
  await expect(grid.locator('thead')).toContainText('Age');
  await expect(grid.locator('thead')).toContainText('Name');
  await expect(page.locator('.grid tbody tr')).toHaveCount(3);
  const firstRow = page.locator('.grid tbody tr').first();
  await expect(firstRow).toContainText('30');
  await expect(firstRow).toContainText('Alice');
});
//=============================================================================
test('variable editor reloads when the variable is reassigned in the console', async ({ page }) => {
  await openVariableA(page);
  await expect(page.locator('.var-editor__bar')).toContainText('3×3');
  await page.locator('.dv-tab').filter({ hasText: 'Command Window' }).click();
  await runLine(page, 'A = 7');
  await expect(page.locator('.var-editor__bar')).toContainText('1×1');
  await expect(page.locator('.grid tbody tr').first().locator('.grid__cell').first()).toHaveText(
    '7'
  );
});
//=============================================================================
test('variable editor shows a struct array as an elements x fields grid', async ({ page }) => {
  await runLine(page, 'structarr_demo');
  await openVarByName(page, 'da');
  await expect(page.locator('.var-editor__bar')).toContainText('3×2 struct');
  const grid = page.locator('.grid');
  await expect(grid.locator('thead')).toContainText('name');
  await expect(grid.locator('thead')).toContainText('isdir');
  await expect(page.locator('.grid tbody tr')).toHaveCount(3);
  const firstRow = page.locator('.grid tbody tr').first();
  await expect(firstRow).toContainText('.git');
  await expect(firstRow).toContainText('true');
});
//=============================================================================
test('variable editor toolbar exposes Refresh/Copy; edit buttons only when editable', async ({
  page,
}) => {
  await openVariableA(page);
  const bar = page.locator('.var-editor__bar');
  await expect(bar.locator('button[title="Refresh"]')).toBeVisible();
  await expect(bar.locator('button[title="Copy (Ctrl+C)"]')).toBeVisible();
  await expect(bar.locator('button[title="Paste (Ctrl+V)"]')).toBeVisible();
  await expect(bar.locator('button[title="Undo (Ctrl+Z)"]')).toBeVisible();
  await page.locator('.dv-tab').filter({ hasText: 'Command Window' }).click();
  await runLine(page, 'struct_demo');
  await openVarByName(page, 'st');
  const stBar = page.locator('.var-editor').filter({ hasText: 'st' }).locator('.var-editor__bar');
  await expect(stBar.locator('button[title="Refresh"]')).toBeVisible();
  await expect(stBar.locator('button[title="Paste (Ctrl+V)"]')).toHaveCount(0);
  await expect(stBar.locator('button[title="Undo (Ctrl+Z)"]')).toHaveCount(0);
});
//=============================================================================
test('variable editor shows a read-only text dump for a function handle', async ({ page }) => {
  await runLine(page, 'fh_demo');
  await openVarByName(page, 'fh');
  await expect(page.locator('.var-editor__bar')).toContainText('function_handle');
  const text = page.locator('.var-editor__text');
  await expect(text).toBeVisible();
  await expect(text).toContainText('@(x) x + 1');
  await expect(page.locator('.grid')).toHaveCount(0);
});
//=============================================================================
test('variable editor tab title shows class and dimensions', async ({ page }) => {
  await openVariableA(page);
  await expect(page.getByRole('tab', { name: 'A [double 3×3]' })).toBeVisible();
  await page.locator('.dv-tab').filter({ hasText: 'Command Window' }).click();
  await runLine(page, 'struct_demo');
  await openVarByName(page, 'st');
  await expect(page.getByRole('tab', { name: /st .*\(read-only\)/ })).toBeVisible();
});
//=============================================================================
test('Help > About opens a native About modal with the version', async ({ page }) => {
  await page.locator('.menubar').getByText('Help', { exact: true }).click();
  await page.locator('.menu').getByText('About Nelson', { exact: true }).click();
  const about = page.locator('.about-card');
  await expect(about).toBeVisible();
  await expect(about.locator('.about-card__title')).toHaveText('Nelson');
  await expect(about.locator('.about-card__version')).toContainText('Version');
  await about.getByRole('button', { name: 'OK' }).click();
  await expect(about).toBeHidden();
});
//=============================================================================
test('listdlg shows a selectable list modal and returns the chosen indices', async ({ page }) => {
  await runLine(page, 'listdlg');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await expect(dialog.locator('.dialog-list__item')).toHaveCount(3);
  await expect(dialog.locator('.dialog-list__item--sel')).toHaveText('Alpha');
  await dialog.locator('.dialog-list__item', { hasText: 'Gamma' }).click();
  await dialog.getByRole('button', { name: 'OK' }).click();
  await expect(dialog).toBeHidden();
  await expect.poll(() => terminalText(page)).toContain('[1 3]');
});
//=============================================================================
test('uisetcolor shows a color picker modal', async ({ page }) => {
  await runLine(page, 'uisetcolor');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await expect(dialog.locator('input[type="color"]')).toBeVisible();
  await dialog.getByRole('button', { name: 'OK' }).click();
  await expect(dialog).toBeHidden();
  await expect.poll(() => terminalText(page)).toContain('#');
});
//=============================================================================
test('uigetfile shows a file browser modal and returns the chosen file', async ({ page }) => {
  await runLine(page, 'uigetfile');
  const dialog = page.getByRole('dialog', { name: 'Open File' });
  await expect(dialog).toBeVisible();
  // The mock filesystem lists demo.m / hello.m under /workspace.
  const file = dialog.getByRole('option', { name: /demo\.m/ });
  await expect(file).toBeVisible();
  await file.click();
  await dialog.getByRole('button', { name: 'Open' }).click();
  await expect(dialog).toBeHidden();
  await expect.poll(() => terminalText(page)).toContain('demo.m');
});
//=============================================================================
test('errordlg shows a fire-and-forget message modal (msgbox family)', async ({ page }) => {
  await runLine(page, 'errordlg');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await expect(dialog.locator('.dialog-card__message')).toContainText('This is a message');
  // Fire-and-forget: a single OK dismisses it locally, no reply needed.
  await dialog.getByRole('button', { name: 'OK' }).click();
  await expect(dialog).toBeHidden();
});
//=============================================================================
test('warndlg shows the vector warning icon with its message in the web dialog', async ({
  page,
}) => {
  await runLine(page, 'warndlg');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  const icon = dialog.locator('.dialog-card__icon .desktop-icon');
  await expect(icon).toBeVisible();
  expect(await icon.evaluate((element) => getComputedStyle(element).maskImage)).not.toBe('none');
  await expect(dialog.locator('.dialog-card__message')).toContainText('This is a message');
  await dialog.getByRole('button', { name: 'OK' }).click();
  await expect(dialog).toBeHidden();
});
//=============================================================================
test('uisetfont shows a font picker modal', async ({ page }) => {
  await runLine(page, 'uisetfont');
  const dialog = page.locator('.dialog-card');
  await expect(dialog).toBeVisible();
  await expect(dialog.locator('select')).toBeVisible();
  await expect(dialog.locator('.dialog-font__preview')).toBeVisible();
  await dialog.locator('input[type="checkbox"]').first().check(); // Bold
  await dialog.getByRole('button', { name: 'OK' }).click();
  await expect(dialog).toBeHidden();
  await expect.poll(() => terminalText(page)).toContain('bold');
});
//=============================================================================
test('Terminal (xterm) follows the desktop theme, not a fixed dark palette', async ({ page }) => {
  await page.locator('.menubar').getByText('Window', { exact: true }).click();
  await page.locator('.menu').getByText('Terminal', { exact: true }).click();
  // In the default light theme the xterm viewport must be white; regression
  // guard against the old hardcoded #141414 dark background.
  const viewport = page.locator('.xterm-viewport').first();
  await expect(viewport).toBeVisible();
  await expect
    .poll(() => viewport.evaluate((el) => getComputedStyle(el).backgroundColor))
    .toBe('rgb(255, 255, 255)');

  // The now-unselected "Command Window" tab must stay readable (dimmed grey,
  // not dockview's near-white default) on the light theme.
  const inactiveTab = page.locator('.nelson-dock .dv-tab', { hasText: 'Command Window' }).first();
  await expect
    .poll(() => inactiveTab.evaluate((el) => getComputedStyle(el).color))
    .toBe('rgb(106, 106, 106)');
});
//=============================================================================
