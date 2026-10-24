//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Clipboard cross-window test
//=============================================================================
import {
  test,
  expect,
  SIMPLE_DIAGRAM_CONTENT,
  SIMPLE_DIAGRAM_PATH,
} from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
//=============================================================================
test('nflow clipboard copy/paste between two windows (host clipboard)', async ({
  mockPage: page,
}) => {
  const tmpFile = path.join(os.tmpdir(), 'nflow_clipboard_data.json');
  try {
    fs.unlinkSync(tmpFile);
  } catch (_e) {}

  await page.exposeFunction('__e2eSetClipboardBackend', async (text: string) => {
    fs.writeFileSync(tmpFile, text, 'utf8');
    return '{"success":true}';
  });
  await page.exposeFunction('__e2eGetClipboardBackend', async () => {
    try {
      return fs.readFileSync(tmpFile, 'utf8');
    } catch (_e) {
      return '';
    }
  });

  const app1 = new AppPage(page);
  await app1.open();

  await app1.openMenu('file');
  await app1.clickMenuAction('open');
  await page.waitForSelector('#blockLayer > *', { timeout: 10_000 });
  const beforeCount = await page.locator('#blockLayer > *').count();
  expect(beforeCount).toBeGreaterThan(0);

  const page2 = await page.context().newPage();

  // Minimal RPC stubs so file dialog calls (if used) resolve sensibly.
  await page2.addInitScript(
    ({ openContent, openFileName, openFilePath, savePath, saveFileName }: any) => {
      (function installProxy() {
        try {
          (window as any).webui = (window as any).webui || {};
          (window as any).webui.call = async (fn: string, payload: string) => {
            if (fn !== 'rpc' && fn !== 'nflow_rpc') return '';
            const envelope = JSON.parse(String(payload || '{}'));
            const ok = (result: unknown) => JSON.stringify({ id: envelope.id, result });
            if (envelope.method === 'nflow.file.openDialog') {
              return ok(
                JSON.stringify({
                  success: true,
                  fileName: openFileName,
                  filePath: openFilePath,
                  content: openContent,
                })
              );
            }
            if (envelope.method === 'nflow.file.saveDialog') {
              return ok(
                JSON.stringify({ success: true, fileName: saveFileName, filePath: savePath })
              );
            }
            if (envelope.method === 'nflow.fs.list') {
              return ok(JSON.stringify({ cwd: '.', entries: [] }));
            }
            if (envelope.method === 'nflow.file.writeText') {
              return ok(JSON.stringify({ success: true, filePath: savePath }));
            }
            if (envelope.method === 'nflow.file.loadInitial') {
              return ok(JSON.stringify({ success: false, canceled: true }));
            }
            if (envelope.method === 'nflow.clipboard.set') {
              return ok(
                await (window as any).__e2eSetClipboardBackend(envelope.params?.data || '')
              );
            }
            if (envelope.method === 'nflow.clipboard.get') {
              return ok(await (window as any).__e2eGetClipboardBackend());
            }
            return '';
          };
        } catch (_e) {
          // ignore
        }
      })();
    },
    {
      openContent: SIMPLE_DIAGRAM_CONTENT,
      openFileName: path.basename(SIMPLE_DIAGRAM_PATH),
      openFilePath: SIMPLE_DIAGRAM_PATH,
      savePath: path.join(os.tmpdir(), 'nflow-e2e-save.nflow'),
      saveFileName: 'nflow-e2e-save.nflow',
    }
  );

  await page2.exposeFunction('__e2eSetClipboardBackend', async (text: string) => {
    fs.writeFileSync(tmpFile, text, 'utf8');
    return '{"success":true}';
  });
  await page2.exposeFunction('__e2eGetClipboardBackend', async () => {
    try {
      return fs.readFileSync(tmpFile, 'utf8');
    } catch (_e) {
      return '';
    }
  });

  const app2 = new AppPage(page2);
  await app2.open();

  await app2.openMenu('file');
  await app2.clickMenuAction('new');
  await expect(page2.locator('#blockLayer > *')).toHaveCount(0, { timeout: 5_000 });

  await page.locator('#blockLayer > *').first().click();
  await page.keyboard.press('Control+c');
  // give host write a moment
  await new Promise((r) => setTimeout(r, 150));

  await page2.keyboard.press('Control+v');

  await page2.waitForSelector('#blockLayer > *', { timeout: 5_000 });
  const afterCount = await page2.locator('#blockLayer > *').count();
  expect(afterCount).toBeGreaterThan(0);

  try {
    fs.unlinkSync(tmpFile);
  } catch (_e) {}
});
//=============================================================================
