//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { exportCanvasSVG, buildExportSvg } from './export-handlers';
import { beforeEach, test, expect, vi } from 'vitest';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
  const svgNS = 'http://www.w3.org/2000/svg';
  const parent = document.createElement('div');
  const svg = document.createElementNS(svgNS, 'svg');
  svg.setAttribute('id', 'svgCanvas');
  svg.setAttribute('viewBox', '0 0 100 50');
  parent.appendChild(svg);
  document.body.appendChild(parent);
});
//=============================================================================
test('exportCanvasSVG picks a path then writes via nflow.file.writeText', async () => {
  const spy = vi.fn(async () => JSON.stringify({ success: true }));
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
          result: JSON.stringify({ available: true, path: '/tmp/diagram.svg' }),
        });
      }
      expect(envelope.method).toBe('nflow.file.writeText');
      return JSON.stringify({ id: envelope.id, result: await spy(envelope.params) });
    },
  };
  await exportCanvasSVG();
  expect(spy).toHaveBeenCalled();
  delete (window as any).webui;
});
//=============================================================================
test('buildExportSvg returns svg string and filename', async () => {
  const res = await buildExportSvg({ diagramName: 'Test Name' });
  expect(res).not.toBeNull();
  if (res) {
    expect(res.filename).toBe('Test_Name.svg');
    expect(res.svgStr).toContain('<svg');
  }
});
//=============================================================================
export {};
//=============================================================================
