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
import { setLockedInput, initEventListeners } from './event-handlers.js';
import { state } from './state.js';
//=============================================================================
describe('locked input wiring', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    const locked = document.createElement('input');
    locked.type = 'checkbox';
    locked.id = 'lockedInput';
    document.body.appendChild(locked);
    if (typeof window.matchMedia !== 'function') {
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      global.window.matchMedia = (query: string) => ({
        matches: false,
        media: query,
        addListener: () => {},
        removeListener: () => {},
        addEventListener: () => {},
        removeEventListener: () => {},
        onchange: null,
        dispatchEvent: () => false,
      });
    }
    setLockedInput(locked as HTMLInputElement);
  });

  test('toggling checkbox updates state and body class', () => {
    const locked = document.getElementById('lockedInput') as HTMLInputElement;
    state.locked = false;
    locked.checked = false;
    initEventListeners();

    locked.checked = true;
    locked.dispatchEvent(new Event('change', { bubbles: true }));
    expect(state.locked).toBe(true);
    expect(document.body.classList.contains('diagram-locked')).toBe(true);

    locked.checked = false;
    locked.dispatchEvent(new Event('change', { bubbles: true }));
    expect(state.locked).toBe(false);
    expect(document.body.classList.contains('diagram-locked')).toBe(false);
  });
});
//=============================================================================
