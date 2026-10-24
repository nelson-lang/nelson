//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { handleMenuKeyDown } from '../desktop/menuKeyboard';
import { DesktopIcon } from './DesktopIcon';
//=============================================================================
// Render <ContextMenu> once per panel and drive it with useContextMenu().
export type MenuItem =
  | { separator: true }
  | {
      label: string;
      onClick: () => void;
      disabled?: boolean;
      danger?: boolean;
      checked?: boolean;
    };
//=============================================================================
export type MenuState = { x: number; y: number; items: MenuItem[] } | null;
export function useContextMenu() {
  const [menu, setMenu] = useState<MenuState>(null);
  const returnFocus = useRef<HTMLElement | null>(null);
  const openMenu = (e: React.MouseEvent, items: MenuItem[]) => {
    e.preventDefault();
    e.stopPropagation();
    returnFocus.current = e.currentTarget instanceof HTMLElement ? e.currentTarget : null;
    setMenu({ x: e.clientX, y: e.clientY, items });
  };
  const closeMenu = () => {
    setMenu(null);
    setTimeout(() => returnFocus.current?.focus());
  };
  return { menu, openMenu, closeMenu };
}
//=============================================================================
export function ContextMenu({
  menu,
  onClose,
}: {
  menu: MenuState;
  onClose: () => void;
}): ReactNode {
  useEffect(() => {
    if (!menu) return;
    const close = () => onClose();
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose();
    };
    window.addEventListener('click', close);
    window.addEventListener('resize', close);
    window.addEventListener('keydown', onKey);
    const focusTimer = window.setTimeout(() =>
      document.querySelector<HTMLButtonElement>('.ctx-menu button:not(:disabled)')?.focus()
    );
    return () => {
      window.clearTimeout(focusTimer);
      window.removeEventListener('click', close);
      window.removeEventListener('resize', close);
      window.removeEventListener('keydown', onKey);
    };
  }, [menu, onClose]);

  if (!menu) return null;
  const x = Math.min(menu.x, window.innerWidth - 200);
  const y = Math.min(menu.y, window.innerHeight - menu.items.length * 26 - 8);
  // Portal into the shell root, not document.body: the theme variables are
  // scoped to it. The Dockview tab strip clips its content and creates a
  // containing block (will-change/transform) that would trap this fixed popup.
  const host = document.querySelector('.nelson-shell') ?? document.body;
  return createPortal(
    <div
      className="ctx-menu"
      role="menu"
      style={{ left: x, top: y }}
      onKeyDown={(event) => handleMenuKeyDown(event, onClose)}
      onMouseDown={(e) => e.stopPropagation()}
    >
      {menu.items.map((it, i) =>
        'separator' in it ? (
          <div key={i} className="ctx-menu__sep" role="separator" />
        ) : (
          <button
            type="button"
            role="menuitem"
            key={i}
            disabled={it.disabled}
            className={
              'ctx-menu__item' + (it.disabled ? ' disabled' : '') + (it.danger ? ' danger' : '')
            }
            onMouseDown={(e) => {
              e.preventDefault();
              if (it.disabled) return;
              it.onClick();
              onClose();
            }}
          >
            {it.checked ? <DesktopIcon name="check" size={16} /> : null}
            {it.label}
          </button>
        )
      )}
    </div>,
    host
  );
}
//=============================================================================
