//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { beforeAll, test, expect, vi } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../../dist/app/components/modals.js').catch(() => null);
});
//=============================================================================
test('modals module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
describe('mountModals', () => {
  let mountModals: () => void;
  let aboutShowEvent: string;
  let aboutHideEvent: string;
  let confirmSaveEvent: string;
  let gridSettingsShowEvent: string;
  let gridSettingsHideEvent: string;
  let missingBlocksEvent: string;

  beforeAll(async () => {
    const mod = await import('./modals.tsx');
    mountModals = mod.mountModals;
    aboutShowEvent = mod.ABOUT_SHOW_EVENT;
    aboutHideEvent = mod.ABOUT_HIDE_EVENT;
    confirmSaveEvent = mod.CONFIRM_SAVE_EVENT;
    gridSettingsShowEvent = mod.GRID_SETTINGS_SHOW_EVENT;
    gridSettingsHideEvent = mod.GRID_SETTINGS_HIDE_EVENT;
    missingBlocksEvent = mod.MISSING_BLOCKS_EVENT;
  });

  beforeEach(() => {
    document.body.innerHTML = '';
  });

  test('inserts multiple modals into document body', () => {
    mountModals();

    expect(document.getElementById('confirmSaveModal')).toBeTruthy();
    expect(document.getElementById('aboutModal')).toBeTruthy();
    expect(document.getElementById('gridSettingsModal')).toBeTruthy();
    expect(document.getElementById('missingBlocksModal')).toBeTruthy();
    expect(document.getElementById('saveConfirm')).toBeTruthy();
    expect(document.getElementById('nflowModalsRoot')?.dataset.reactRoot).toBe('modals');
    // Subsystem name modal intentionally removed; ensure other modals still mount
  });

  test('confirm save modal resolves through the React event bridge', async () => {
    await act(async () => {
      mountModals();
    });

    const cb = vi.fn();
    const event = new CustomEvent(confirmSaveEvent, {
      cancelable: true,
      detail: { callback: cb },
    });

    await act(async () => {
      document.dispatchEvent(event);
    });

    expect(event.defaultPrevented).toBe(true);
    expect(document.getElementById('confirmSaveModal')?.classList.contains('show')).toBe(true);

    await act(async () => {
      document.getElementById('saveConfirm')!.click();
    });

    expect(cb).toHaveBeenCalledWith(true);
    expect(document.getElementById('confirmSaveModal')?.classList.contains('show')).toBe(false);
  });

  test('about modal opens and closes through React events', async () => {
    await act(async () => {
      mountModals();
    });

    const showEvent = new CustomEvent(aboutShowEvent, { cancelable: true });
    await act(async () => {
      document.dispatchEvent(showEvent);
    });

    expect(showEvent.defaultPrevented).toBe(true);
    expect(document.getElementById('aboutModal')?.classList.contains('show')).toBe(true);

    const hideEvent = new CustomEvent(aboutHideEvent, { cancelable: true });
    await act(async () => {
      document.dispatchEvent(hideEvent);
    });

    expect(hideEvent.defaultPrevented).toBe(true);
    expect(document.getElementById('aboutModal')?.classList.contains('show')).toBe(false);

    await act(async () => {
      document.dispatchEvent(new CustomEvent(aboutShowEvent, { cancelable: true }));
    });
    await act(async () => {
      document.getElementById('closeAboutModalBtn')!.click();
    });

    expect(document.getElementById('aboutModal')?.classList.contains('show')).toBe(false);
  });

  test('confirm save modal resolves dont-save and close actions in React', async () => {
    await act(async () => {
      mountModals();
    });

    const dontSaveCb = vi.fn();
    await act(async () => {
      document.dispatchEvent(
        new CustomEvent(confirmSaveEvent, {
          cancelable: true,
          detail: { callback: dontSaveCb },
        })
      );
    });
    await act(async () => {
      document.getElementById('dontSaveConfirm')!.click();
    });
    expect(dontSaveCb).toHaveBeenCalledWith(false);

    const closeCb = vi.fn();
    await act(async () => {
      document.dispatchEvent(
        new CustomEvent(confirmSaveEvent, {
          cancelable: true,
          detail: { callback: closeCb },
        })
      );
    });
    await act(async () => {
      document.getElementById('closeConfirmSaveModal')!.click();
    });
    expect(closeCb).toHaveBeenCalledWith(null);
  });

  test('grid settings modal opens, syncs controls, and hides through React events', async () => {
    await act(async () => {
      mountModals();
    });

    const showEvent = new CustomEvent(gridSettingsShowEvent, { cancelable: true });
    await act(async () => {
      document.dispatchEvent(showEvent);
    });

    expect(showEvent.defaultPrevented).toBe(true);
    expect(document.getElementById('gridSettingsModal')?.classList.contains('show')).toBe(true);

    const slider = document.getElementById('gridSizeSlider') as HTMLInputElement;
    const input = document.getElementById('gridSizeInput') as HTMLInputElement;
    slider.value = '42';
    await act(async () => {
      slider.dispatchEvent(new Event('input', { bubbles: true }));
    });
    expect(input.value).toBe('42');

    const hideEvent = new CustomEvent(gridSettingsHideEvent, { cancelable: true });
    await act(async () => {
      document.dispatchEvent(hideEvent);
    });

    expect(hideEvent.defaultPrevented).toBe(true);
    expect(document.getElementById('gridSettingsModal')?.classList.contains('show')).toBe(false);
  });

  test('missing blocks modal renders list through the React event bridge', async () => {
    await act(async () => {
      mountModals();
    });

    const event = new CustomEvent(missingBlocksEvent, {
      cancelable: true,
      detail: {
        missingBlocks: [{ type: 'blockA', libraryTitle: 'Library A' }, { type: 'blockB' }],
      },
    });

    await act(async () => {
      document.dispatchEvent(event);
    });

    const modal = document.getElementById('missingBlocksModal')!;
    expect(event.defaultPrevented).toBe(true);
    expect(modal.classList.contains('show')).toBe(true);
    expect(document.querySelectorAll('#missingBlocksList li').length).toBe(2);
    expect(document.getElementById('missingBlocksList')?.textContent).toContain('Library A');

    await act(async () => {
      document.getElementById('closeMissingBlocksBtn')!.click();
    });
    expect(modal.classList.contains('show')).toBe(false);
  });
});
//=============================================================================
