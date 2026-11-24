//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect } from 'react';
//=============================================================================
/** Close an open dropdown on a pointer down outside insideSelector or on Escape. */
export function useMenuDismiss(open: boolean, close: () => void, insideSelector: string): void {
  useEffect(() => {
    if (!open) return undefined;
    const onPointerDown = (event: Event) => {
      const target = event.target as Element | null;
      if (target?.closest?.(insideSelector)) return;
      close();
    };
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') close();
    };
    document.addEventListener('pointerdown', onPointerDown, true);
    document.addEventListener('keydown', onKeyDown);
    return () => {
      document.removeEventListener('pointerdown', onPointerDown, true);
      document.removeEventListener('keydown', onKeyDown);
    };
  }, [open, close, insideSelector]);
}
//=============================================================================
