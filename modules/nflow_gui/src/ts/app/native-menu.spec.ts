//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, test, vi } from 'vitest';
import { installNFlowNativeMenu, nflowNativeMenuDescriptor } from './native-menu.js';
//=============================================================================
function setMessageHandler(postMessage: ReturnType<typeof vi.fn>): void {
  Object.defineProperty(window, 'webkit', {
    configurable: true,
    value: { messageHandlers: { webuiNativeMenu: { postMessage } } },
  });
}
//=============================================================================
afterEach(() => {
  Reflect.deleteProperty(window, 'webkit');
  document.body.replaceChildren();
  localStorage.clear();
});
//=============================================================================
test('keeps the current title bar outside the macOS WebKit host', () => {
  document.body.innerHTML = '<div class="titlebar"></div>';
  expect(installNFlowNativeMenu()).toBe(false);
  expect(document.querySelector<HTMLElement>('.titlebar')?.style.display).toBe('');
});
//=============================================================================
test('publishes the NFlow menu and removes its duplicate title bar', () => {
  const postMessage = vi.fn();
  setMessageHandler(postMessage);
  localStorage.setItem('nf-recent-files-v1', JSON.stringify(['/tmp/example.nflow']));
  document.body.innerHTML = '<div class="titlebar"></div>';

  expect(installNFlowNativeMenu()).toBe(true);
  expect(document.querySelector<HTMLElement>('.titlebar')?.style.display).toBe('none');
  expect(postMessage).toHaveBeenCalledWith(
    expect.objectContaining({
      type: 'set',
      menus: expect.arrayContaining([
        expect.objectContaining({ label: 'NFlow' }),
        expect.objectContaining({ label: 'File' }),
      ]),
    })
  );

  const fileMenu = nflowNativeMenuDescriptor().menus.find((menu) => menu.label === 'File');
  const recentMenu = fileMenu?.items.find((item) => item.label === 'Open Recent');
  expect(recentMenu?.items).toContainEqual(
    expect.objectContaining({
      label: 'example.nflow',
      action: 'openRecent:%2Ftmp%2Fexample.nflow',
    })
  );
});
//=============================================================================
