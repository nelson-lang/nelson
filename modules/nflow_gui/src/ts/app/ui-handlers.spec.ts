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
//=============================================================================
let mod: any;
beforeAll(async () => {
  await vi.doMock('../../../dist/app/workspace-handlers.js', async () => ({
    getViewBox: () => ({ x: 0, y: 0, w: 100, h: 100 }),
    setViewBox: vi.fn(),
    getZoomScale: () => 1,
    setZoomScale: vi.fn(),
    getUpdateStatusBar: () => vi.fn(),
    getViewportSize: () => ({ w: 100, h: 100 }),
    clearWorkspace: vi.fn(),
    ensureLeftMargin: vi.fn(),
    clampZoomScale: (s: number) => Math.max(0.01, Math.min(3, s)),
  }));

  await vi.doMock('../../../dist/app/state.js', async () => ({
    getFitToDiagram: () => vi.fn(),
    state: {},
    markDirty: vi.fn(),
    clearDirty: vi.fn(),
    signalDiagramChanged: vi.fn(),
    deepClone: (v: any) => JSON.parse(JSON.stringify(v)),
    getCurrentFilePath: () => null,
    setCurrentFilePath: vi.fn(),
  }));

  mod = await import('../../../dist/app/ui-handlers.js');
});
//=============================================================================
describe('ui-handlers basic behaviors', () => {
  afterEach(() => {
    document.body.innerHTML = '';
  });

  test('initSidebarUI increases retry counter when DOM missing', () => {
    document.body.innerHTML = '';
    // @ts-ignore
    window.__initSidebarUIRetryCount = 0;
    mod.initSidebarUI();
    // @ts-ignore
    expect(window.__initSidebarUIRetryCount || 0).toBeGreaterThanOrEqual(1);
  });

  test('initZoomButtons attaches handlers and zoom functions call workspace helpers', async () => {
    const home = document.createElement('button');
    const zin = document.createElement('button');
    const zout = document.createElement('button');
    document.body.appendChild(home);
    document.body.appendChild(zin);
    document.body.appendChild(zout);
    mod.setHomeBtnRef(home);
    mod.setZoomInBtnRef(zin);
    mod.setZoomOutBtnRef(zout);

    const ws = await import('../../../dist/app/workspace-handlers.js');
    mod.initZoomButtons();

    home.click();

    zin.click();
    expect(ws.setZoomScale).toHaveBeenCalled();
    expect(ws.setViewBox).toHaveBeenCalled();
    zout.click();
    expect(ws.setZoomScale).toHaveBeenCalled();
  });
});
//=============================================================================
export {};
//=============================================================================
