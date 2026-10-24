//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test, vi } from 'vitest';
import { ABOUT_SHOW_EVENT } from './components/modal-events.js';
import { handleMenuAction } from './menu-handlers.js';
//=============================================================================
test('about menu action requests the React about modal', () => {
  const listener = vi.fn();
  document.addEventListener(ABOUT_SHOW_EVENT, listener);

  handleMenuAction('about');

  expect(listener).toHaveBeenCalledTimes(1);
  document.removeEventListener(ABOUT_SHOW_EVENT, listener);
});
//=============================================================================
