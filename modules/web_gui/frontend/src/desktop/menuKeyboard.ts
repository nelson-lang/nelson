//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { KeyboardEvent } from 'react';
//=============================================================================
export function handleMenuKeyDown(event: KeyboardEvent<HTMLElement>, close: () => void): void {
  if (event.key === 'Escape') {
    event.preventDefault();
    event.stopPropagation();
    close();
    return;
  }
  const buttons = [
    ...event.currentTarget.querySelectorAll<HTMLButtonElement>('button:not(:disabled)'),
  ];
  if (!buttons.length) return;
  const index = buttons.indexOf(document.activeElement as HTMLButtonElement);
  const target =
    event.key === 'ArrowDown'
      ? (index + 1 + buttons.length) % buttons.length
      : event.key === 'ArrowUp'
      ? (index - 1 + buttons.length) % buttons.length
      : event.key === 'Home'
      ? 0
      : event.key === 'End'
      ? buttons.length - 1
      : -1;
  if (target < 0) return;
  event.preventDefault();
  buttons[target]?.focus();
}
//=============================================================================
