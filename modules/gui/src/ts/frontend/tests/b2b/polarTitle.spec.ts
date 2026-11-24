//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { writeFileSync } from 'node:fs';
import { expect, test, type Page } from '@playwright/test';
import { figureCommands } from '../figureCommands';
import { waitForFigureLayout } from '../figureBars';
//=============================================================================
interface DrawnText {
  text: string;
  x: number;
  y: number;
  font: string;
  align: string;
}
//=============================================================================
// records every text the figure canvases draw, so a check reads the live frame
async function recordCanvasText(page: Page) {
  await page.addInitScript(() => {
    const log: DrawnText[] = [];
    (window as unknown as { __polarTitleTexts: DrawnText[] }).__polarTitleTexts = log;
    const original = CanvasRenderingContext2D.prototype.fillText;
    CanvasRenderingContext2D.prototype.fillText = function (text, x, y, maxWidth) {
      const m = this.getTransform();
      // x is the horizontal centre of the drawn text, whatever its alignment
      const width = this.measureText(String(text)).width;
      const shift = { left: 0.5, start: 0.5, center: 0, right: -0.5, end: -0.5 }[this.textAlign];
      const cx = x + width * shift;
      log.push({
        text: String(text),
        x: m.a * cx + m.c * y + m.e,
        y: m.b * x + m.d * y + m.f,
        font: this.font,
        align: this.textAlign,
      });
      return original.call(this, text, x, y, maxWidth);
    };
  });
}
//=============================================================================
async function clearTexts(page: Page) {
  await page.evaluate(() => {
    (window as unknown as { __polarTitleTexts: DrawnText[] }).__polarTitleTexts.length = 0;
  });
}
//=============================================================================
async function drawnTexts(page: Page): Promise<DrawnText[]> {
  return page.evaluate(() => [
    ...(window as unknown as { __polarTitleTexts: DrawnText[] }).__polarTitleTexts,
  ]);
}
//=============================================================================
let textDumps = 0;
// last draw of each wanted text, once all of them showed up
async function waitForTexts(page: Page, wanted: string[]): Promise<Map<string, DrawnText>> {
  let found = new Map<string, DrawnText>();
  await expect
    .poll(
      async () => {
        found = new Map();
        for (const t of await drawnTexts(page)) {
          if (wanted.includes(t.text)) found.set(t.text, t);
        }
        return wanted.filter((w) => !found.has(w));
      },
      { timeout: 30_000 }
    )
    .toEqual([]);
  // the drawn texts are kept next to the screenshots for a failing run
  writeFileSync(
    test.info().outputPath(`texts-${++textDumps}.json`),
    JSON.stringify(await drawnTexts(page), null, 1)
  );
  return found;
}
//=============================================================================
// the title sits centred above its top theta label, bold over a normal subtitle
function expectTitleAbove(title: DrawnText, top: DrawnText, subtitle?: DrawnText) {
  expect(title.font).toMatch(/bold/);
  expect(Math.abs(title.x - top.x)).toBeLessThan(2);
  expect(title.y).toBeGreaterThan(8);
  if (subtitle) {
    expect(subtitle.font).not.toMatch(/bold/);
    expect(Math.abs(subtitle.x - top.x)).toBeLessThan(2);
    expect(title.y).toBeLessThan(subtitle.y - 8);
    expect(subtitle.y).toBeLessThan(top.y - 8);
  } else {
    expect(title.y).toBeLessThan(top.y - 8);
  }
}
//=============================================================================
test('shows polaraxes titles and subtitles in the live view', async ({ page }) => {
  const run = figureCommands(page, 'polar-title');
  const show = async (name: string) => {
    await page.getByRole('tab', { name: /Polar title/ }).click();
    await waitForFigureLayout(page);
    await page
      .locator('.figure-panel__canvas')
      .first()
      .screenshot({ path: test.info().outputPath(`${name}.png`) });
  };
  await recordCanvasText(page);
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Polar title'); pax=polaraxes(f,'Position',[0.15 0.1 0.6 0.6]); title(pax,'Title text'); subtitle(pax,'sub text'); drawnow()"
  );
  await clearTexts(page);
  await show('single');
  let texts = await waitForTexts(page, ['Title text', 'sub text', '90°']);
  expectTitleAbove(texts.get('Title text')!, texts.get('90°')!, texts.get('sub text')!);
  const before = texts.get('Title text')!;
  // a smaller view repaints the figure: the title follows the new top label
  await page.setViewportSize({ width: 900, height: 560 });
  await clearTexts(page);
  await show('single-resized');
  await expect
    .poll(async () => {
      const latest = (await drawnTexts(page)).filter((t) => t.text === 'Title text').pop();
      return latest ? Math.round(latest.x - before.x) : 0;
    })
    .not.toBe(0);
  texts = await waitForTexts(page, ['Title text', 'sub text', '90°']);
  expectTitleAbove(texts.get('Title text')!, texts.get('90°')!, texts.get('sub text')!);
  // Visible 'off' hides the grid, labels, title and subtitle
  await run("pax.Visible='off'; drawnow()");
  await clearTexts(page);
  await show('single-hidden');
  await run('pax.Color=[1 1 1]; drawnow()');
  await page.waitForTimeout(1000);
  const hidden = (await drawnTexts(page)).map((t) => t.text);
  expect(hidden).not.toContain('Title text');
  expect(hidden).not.toContain('sub text');
  expect(hidden).not.toContain('90°');
  // 'axis off' hides the grid and labels but keeps the title and subtitle
  await run("pax.Visible='on'; axis off; drawnow()");
  await clearTexts(page);
  await show('single-axis-off');
  texts = await waitForTexts(page, ['Title text', 'sub text']);
  expect(texts.get('Title text')!.font).toMatch(/bold/);
  expect(texts.get('Title text')!.y).toBeLessThan(texts.get('sub text')!.y);
  expect((await drawnTexts(page)).map((t) => t.text)).not.toContain('90°');
  // the 2x4 layout gets back the default view size
  await page.setViewportSize({ width: 1280, height: 720 });
  await run(
    "close('all'); f=figure('Name','Polar title','Position',[100 100 900 500]); locs={'right','top','left','bottom'}; dirs={'counterclockwise','clockwise'}; for d=1:2, for l=1:4, p=polaraxes(f,'Position',[0.03+(l-1)*0.245, 0.56-(d-1)*0.5, 0.19, 0.34]); p.ThetaZeroLocation=locs{l}; p.ThetaDir=dirs{d}; title(p,[locs{l} ' ' dirs{d}]); end, end; drawnow()"
  );
  await clearTexts(page);
  await show('grid');
  // top label of each layout, read from the reference
  const tops: Record<string, string> = {
    'right counterclockwise': '90°',
    'top counterclockwise': '0°',
    'left counterclockwise': '270°',
    'bottom counterclockwise': '180°',
    'right clockwise': '270°',
    'top clockwise': '0°',
    'left clockwise': '90°',
    'bottom clockwise': '180°',
  };
  await waitForTexts(page, Object.keys(tops));
  const all = await drawnTexts(page);
  const names = all.map((t) => t.text);
  for (const [name, top] of Object.entries(tops)) {
    // an axes draws its labels right before its title: both come from the same frame
    const titleIndex = names.lastIndexOf(name);
    const labelIndex = names.lastIndexOf(top, titleIndex);
    expect(labelIndex, `top label under ${name}`).toBeGreaterThanOrEqual(0);
    expectTitleAbove(all[titleIndex], all[labelIndex]);
  }
  await run("close('all')");
});
//=============================================================================
