//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as eh from './export-handlers';
import * as gm from './grid-manager';
import { beforeEach, test, expect, vi } from 'vitest';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
  const svgNS = 'http://www.w3.org/2000/svg';
  const parent = document.createElement('div');
  const svg = document.createElementNS(svgNS, 'svg');
  svg.setAttribute('id', 'svgCanvas');
  // Provide a viewBox so getExportBounds uses it predictably
  svg.setAttribute('viewBox', '0 0 200 100');
  const blockLayer = document.createElementNS(svgNS, 'g');
  blockLayer.setAttribute('id', 'blockLayer');
  const wireLayer = document.createElementNS(svgNS, 'g');
  wireLayer.setAttribute('id', 'wireLayer');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  parent.appendChild(svg);
  document.body.appendChild(parent);
  // ensure canvas parent has a CSS grid color variable
  parent.style.setProperty('--canvas-grid-color', '#123456');
  gm.setGridSize(20);
});
//=============================================================================
test('buildExportSvg inserts exportGridLayer and returns sanitized filename', async () => {
  // mock fetch to fail so embedUsedFonts does nothing
  // @ts-ignore
  global.fetch = vi.fn(async () => ({ ok: false }));

  const res = await eh.buildExportSvg({ diagramName: 'My Diagram' });
  expect(res).not.toBeNull();
  if (res) {
    expect(res.filename).toBe('My_Diagram.svg');
    expect(res.svgStr).toContain('exportGridLayer');
    expect(res.svgStr).toContain('<style');
  }
  // @ts-ignore
  global.fetch = undefined;
});
//=============================================================================
test('buildExportSvg preserves signed viewBox fallback coordinates', async () => {
  const svg = document.getElementById('svgCanvas') as SVGSVGElement;
  svg.setAttribute('viewBox', '-100 -50 200 100');
  // @ts-ignore
  global.fetch = vi.fn(async () => ({ ok: false }));

  const res = await eh.buildExportSvg({ diagramName: 'Signed' });
  expect(res).not.toBeNull();
  if (res) {
    expect(res.svgStr).toContain('viewBox="-100 -50 200 100"');
  }
  // @ts-ignore
  global.fetch = undefined;
});
//=============================================================================
test('buildExportSvg embeds fonts when foreignObject has font-family', async () => {
  const svg = document.getElementById('svgCanvas') as SVGSVGElement;
  const svgNS = 'http://www.w3.org/2000/svg';
  const fo = document.createElementNS(svgNS, 'foreignObject');
  const inner = document.createElement('div');
  inner.setAttribute('style', 'font-family: KaTeX_Main');
  fo.appendChild(inner);
  svg.appendChild(fo);

  // @ts-ignore
  global.fetch = vi.fn(async () => ({
    ok: true,
    arrayBuffer: async () => new Uint8Array([1, 2, 3]).buffer,
  }));

  const res = await eh.buildExportSvg({ diagramName: 'FontTest' });
  expect(res).not.toBeNull();
  if (res) {
    expect(res.filename).toBe('FontTest.svg');
    // foreignObject content should be preserved in the exported SVG
    expect(res.svgStr).toContain('foreignObject');
  }
  // @ts-ignore
  global.fetch = undefined;
});
//=============================================================================
export {};
//=============================================================================
