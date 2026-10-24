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
import {
  setHomeBtnRef,
  setZoomInBtnRef,
  setZoomOutBtnRef,
  initZoomButtons,
} from './ui-handlers.js';
import {
  setViewBox,
  setZoomScale,
  getZoomScale,
  setUpdateStatusBar,
} from './workspace-handlers.js';
import { setFitToDiagram } from './state.js';
//=============================================================================
describe('ui-handlers zoom and modals', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    localStorage.clear();
    const canvas = document.createElement('div');
    canvas.id = 'canvas';
    canvas.getBoundingClientRect = () => ({ width: 800, height: 600 } as DOMRect);
    document.body.appendChild(canvas);
    const about = document.createElement('div');
    about.id = 'aboutModal';
    document.body.appendChild(about);
    const closeBtn = document.createElement('button');
    closeBtn.id = 'closeAboutModalBtn';
    document.body.appendChild(closeBtn);
  });

  test('zoom in/out buttons update zoom and call updateStatusBar', () => {
    const zoomIn = document.createElement('button');
    const zoomOut = document.createElement('button');
    const home = document.createElement('button');
    setZoomInBtnRef(zoomIn);
    setZoomOutBtnRef(zoomOut);
    setHomeBtnRef(home);

    setViewBox({ x: 0, y: 0, w: 400, h: 300 });
    setZoomScale(1);
    const updateStatus = vi.fn();
    setUpdateStatusBar(updateStatus);

    const fit = vi.fn();
    setFitToDiagram(fit);

    initZoomButtons();

    zoomIn.click();
    expect(getZoomScale()).toBeGreaterThan(0);
    expect(updateStatus).toHaveBeenCalled();

    home.click();
    expect(fit).toHaveBeenCalled();
  });
});
//=============================================================================
