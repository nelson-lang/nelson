//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, type ReactNode } from 'react';
//=============================================================================
// Overlay + card; Escape (and a click outside) cancels, Enter confirms.
// The card itself takes the focus, never the confirm button: the Enter
// that opened a dialog (Enter on the list) must not also activate it.
export function Modal({ className, label, onCancel, onConfirm, children }: {
  className: string; label: string; onCancel: () => void; onConfirm?: () => void; children: ReactNode;
}) {
  const card = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (!card.current?.contains(document.activeElement)) card.current?.focus();
  }, []);
  useEffect(() => {
    // A dialog can open while the key event that caused it is still bubbling
    // (fast backend): only keys pressed after the dialog opened count.
    const openedAt = performance.now();
    const onKey = (e: KeyboardEvent) => {
      if (e.timeStamp < openedAt) return;
      if (e.key === 'Escape') onCancel();
      else if (e.key === 'Enter' && onConfirm && !(e.target instanceof HTMLTextAreaElement)) onConfirm();
    };
    document.addEventListener('keydown', onKey);
    return () => document.removeEventListener('keydown', onKey);
  }, [onCancel, onConfirm]);
  return (
    <div className={'modal ' + className} onClick={e => { if (e.target === e.currentTarget) onCancel(); }}>
      <div className={'modal__card ' + className.replace('modal--', 'modal__card--')} role="dialog" aria-modal="true" aria-label={label} tabIndex={-1} ref={card}>{children}</div>
    </div>
  );
}
//=============================================================================
