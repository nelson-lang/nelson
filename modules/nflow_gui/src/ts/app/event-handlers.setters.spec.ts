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
  setRendererRef,
  setAutoRouteInput,
  setRealTimeModeInput,
  initEventListeners,
} from './event-handlers.js';
import { state } from './state.js';
//=============================================================================
describe('event-handlers setters and autoRoute behavior', () => {
  beforeEach(() => {
    const existing = document.getElementById('autoRouteInput');
    if (existing && existing.parentNode) existing.parentNode.removeChild(existing);
  });

  test('setRendererRef and autoRoute change triggers forceFullRoute', () => {
    const mock = { current: { forceFullRoute: vi.fn(), updateConnections: vi.fn() } };
    setRendererRef(mock as any);

    const checkbox = document.createElement('input');
    checkbox.type = 'checkbox';
    checkbox.id = 'autoRouteInput';
    document.body.appendChild(checkbox);

    setAutoRouteInput(checkbox as HTMLInputElement);
    // mock matchMedia (jsdom may not provide it) and initialize listeners
    (globalThis as any).matchMedia = (q: string) => ({
      matches: false,
      media: q,
      addListener: () => {},
      removeListener: () => {},
      addEventListener: () => {},
      removeEventListener: () => {},
    });
    initEventListeners();

    checkbox.checked = true;
    checkbox.dispatchEvent(new Event('change', { bubbles: true }));

    expect(mock.current.forceFullRoute).toHaveBeenCalled();
  });

  test('real-time mode selector updates simulation pacing state', () => {
    state.realTime = undefined;
    const select = document.createElement('select');
    select.id = 'realTimeMode';
    select.innerHTML =
      '<option value="auto">Auto</option><option value="real-time">Real-time</option><option value="fast">Fast</option>';
    document.body.appendChild(select);

    setRealTimeModeInput(select);
    initEventListeners();

    select.value = 'real-time';
    select.dispatchEvent(new Event('change', { bubbles: true }));
    expect(state.realTime).toBe(true);

    select.value = 'fast';
    select.dispatchEvent(new Event('change', { bubbles: true }));
    expect(state.realTime).toBe(false);

    select.value = 'auto';
    select.dispatchEvent(new Event('change', { bubbles: true }));
    expect(state.realTime).toBeUndefined();
  });
});
//=============================================================================
import * as eh from './event-handlers';
//=============================================================================
test('event-handlers setters and viewport', () => {
  const el = document.createElement('div');
  const input = document.createElement('input');
  const textarea = document.createElement('textarea');
  const svg = document.createElementNS(
    'http://www.w3.org/2000/svg',
    'svg'
  ) as unknown as SVGSVGElement;
  document.body.appendChild(svg);
  eh.setRendererRef(null as any);
  eh.setStatusEl(el as any);
  eh.setRuntimeInput(input as any);
  eh.setSimDt(input as any);
  eh.setAutoRouteInput(input as any);
  eh.setVariablesInput(textarea as any);
  eh.setVariablesPreview(el as any);
  eh.setInspectorBody(el as any);
  eh.setRotateSelectionBtn(el as any);
  eh.setMarginLoopSelect(document.createElement('select') as any);
  eh.setMarginOutputText(el as any);
  eh.setGridSize(10);
  eh.setDiagramNameInput(input as any);
  eh.setSvg(svg as any);
  eh.setSubsystemUpBtn(el as any);
  const vp = eh.getViewportSize();
  expect(typeof vp.w).toBe('number');
  expect(typeof vp.h).toBe('number');
});
//=============================================================================
