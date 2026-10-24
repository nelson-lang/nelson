//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { beforeAll, beforeEach, test, expect, vi } from 'vitest';
import { initKeyboardShortcuts } from './keyboard-shortcuts.js';
//=============================================================================
// Register the global keydown listener exactly once so the view shortcuts are
// not double-fired by listeners stacking across tests.
beforeAll(() => {
  const refs: any = {
    rendererRef: { current: null },
    statusEl: null,
    GRID_SIZE: 20,
    renderInspector: () => {},
  };
  const sim: any = { handleRun: () => {}, handleReset: () => {} };
  initKeyboardShortcuts(refs, sim);
});
//=============================================================================
function makeButton(id: string): ReturnType<typeof vi.fn> {
  const btn = document.createElement('button');
  btn.id = id;
  const spy = vi.fn();
  btn.addEventListener('click', () => spy());
  document.body.appendChild(btn);
  return spy;
}
//=============================================================================
function pressCtrl(key: string): void {
  window.dispatchEvent(new KeyboardEvent('keydown', { key, ctrlKey: true, bubbles: true }));
}
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('Ctrl+0 triggers the Reset View button', () => {
  const home = makeButton('homeBtn');
  pressCtrl('0');
  expect(home).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('Ctrl+= triggers the Zoom In button', () => {
  const zin = makeButton('zoomInBtn');
  pressCtrl('=');
  expect(zin).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('Ctrl++ also triggers the Zoom In button', () => {
  const zin = makeButton('zoomInBtn');
  pressCtrl('+');
  expect(zin).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('Ctrl+- triggers the Zoom Out button', () => {
  const zout = makeButton('zoomOutBtn');
  pressCtrl('-');
  expect(zout).toHaveBeenCalledTimes(1);
});
//=============================================================================
export {};
//=============================================================================
