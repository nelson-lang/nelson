//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest';
import {
  installNativeMenu,
  isNativeMenuAvailable,
  publishNativeMenu,
  type NativeMenuDescriptor,
} from '@nelson/native-menu';
import { nelsonNativeMenuDescriptor } from '../../src/desktop/nativeMenu';
import { setDesktopCapabilities } from '../../src/desktop/capabilities';
//=============================================================================
const descriptor: NativeMenuDescriptor = {
  menus: [{ label: 'Application', items: [{ label: 'Run', action: 'run' }] }],
};
//=============================================================================
function setMessageHandler(postMessage: ReturnType<typeof vi.fn>): void {
  Object.defineProperty(window, 'webkit', {
    configurable: true,
    value: { messageHandlers: { webuiNativeMenu: { postMessage } } },
  });
}
//=============================================================================
beforeEach(() => {
  const entries = new Map<string, string>();
  Object.defineProperty(window, 'localStorage', {
    configurable: true,
    value: {
      clear: () => entries.clear(),
      getItem: (key: string) => entries.get(key) ?? null,
      removeItem: (key: string) => entries.delete(key),
      setItem: (key: string, value: string) => entries.set(key, value),
    },
  });
});
//=============================================================================
afterEach(() => {
  Reflect.deleteProperty(window, 'webkit');
  Reflect.deleteProperty(window, 'localStorage');
  vi.useRealTimers();
});
//=============================================================================
describe('native menu bridge', () => {
  test('keeps the HTML fallback when the WebKit handler is absent', () => {
    expect(isNativeMenuAvailable()).toBe(false);
    expect(publishNativeMenu(descriptor)).toBe(false);
  });

  test('publishes descriptors and forwards native actions', async () => {
    vi.useFakeTimers();
    const postMessage = vi.fn();
    const execute = vi.fn();
    setMessageHandler(postMessage);

    const removeListeners = installNativeMenu(() => descriptor, execute);
    expect(postMessage).toHaveBeenCalledWith({ type: 'set', ...descriptor });

    window.dispatchEvent(
      new CustomEvent('webui:native-menu-action', { detail: { action: 'run' } })
    );
    expect(execute).toHaveBeenCalledWith('run');
    await vi.runAllTimersAsync();
    expect(postMessage).toHaveBeenCalledTimes(2);

    removeListeners();
    window.dispatchEvent(
      new CustomEvent('webui:native-menu-action', { detail: { action: 'ignored' } })
    );
    expect(execute).toHaveBeenCalledTimes(1);
  });
});
//=============================================================================
describe('Nelson native menu', () => {
  test('describes standard macOS and application menus', () => {
    const menu = nelsonNativeMenuDescriptor();
    expect(menu.menus.map((entry) => entry.label)).toEqual([
      'Nelson',
      'File',
      'Edit',
      'View',
      'Window',
      'Help',
    ]);
    expect(menu.menus[0]?.items).toContainEqual(
      expect.objectContaining({ label: 'Quit Nelson', action: 'file.exit', shortcut: 'cmd+q' })
    );
    expect(menu.menus[0]?.items).toContainEqual(
      expect.objectContaining({ label: 'Hide Nelson', role: 'hide' })
    );
    const help = menu.menus.find((entry) => entry.label === 'Help');
    expect(help?.items).toContainEqual(
      expect.objectContaining({ label: 'Examples', action: 'help.examples' })
    );
  });

  test('adds the package manager to Window when nmm_gui is available', () => {
    setDesktopCapabilities({ nflow: false, packageManager: true });
    const menu = nelsonNativeMenuDescriptor();
    expect(menu.menus.map((entry) => entry.label)).toEqual([
      'Nelson',
      'File',
      'Edit',
      'View',
      'Window',
      'Help',
    ]);
    const windowMenu = menu.menus.find((entry) => entry.label === 'Window');
    expect(windowMenu?.items).toContainEqual(
      expect.objectContaining({ label: 'Package Manager', action: 'tools.packageManager' })
    );
    setDesktopCapabilities({ nflow: false, packageManager: false });
  });
});
//=============================================================================
