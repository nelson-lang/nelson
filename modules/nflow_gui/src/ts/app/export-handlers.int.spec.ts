//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { buildExportSvg, exportCanvasSVG } from './export-handlers';
import { state } from './state';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
  state.diagramName = 'my-diagram';
});
//=============================================================================
test('buildExportSvg returns svg string and filename', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.setAttribute('id', 'svgCanvas');
  svg.setAttribute('viewBox', '0 0 200 100');
  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  blockLayer.setAttribute('id', 'blockLayer');
  svg.appendChild(blockLayer);
  document.body.appendChild(svg);

  const res = await buildExportSvg({ diagramName: 'TestName' });
  expect(res).not.toBeNull();
  if (res) {
    expect(typeof res.svgStr).toBe('string');
    expect(res.filename.endsWith('.svg')).toBe(true);
  }
});
//=============================================================================
test('exportCanvasSVG picks a path then writes via nflow.file.writeText', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.setAttribute('id', 'svgCanvas');
  svg.setAttribute('viewBox', '0 0 50 50');
  document.body.appendChild(svg);

  const write = vi.fn(async () => JSON.stringify({ success: true }));
  (window as any).webui = {
    call: async (name: string, payload: string) => {
      expect(name === 'nflow_rpc' || name === 'rpc').toBe(true);
      const envelope = JSON.parse(payload);
      if (envelope.method === 'nflow.host.mode') {
        return JSON.stringify({
          id: envelope.id,
          result: { mode: 'webview' },
        });
      }
      if (envelope.method === 'nflow.fs.nativeDialog') {
        return JSON.stringify({
          id: envelope.id,
          result: JSON.stringify({ available: true, path: '/tmp/test.svg' }),
        });
      }
      expect(envelope.method).toBe('nflow.file.writeText');
      return JSON.stringify({ id: envelope.id, result: await write(envelope.params) });
    },
  };
  await exportCanvasSVG();
  expect(write).toHaveBeenCalled();
  delete (window as any).webui;
});
//=============================================================================
