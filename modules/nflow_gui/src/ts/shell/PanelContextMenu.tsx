//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Small right-click menu shared by the bottom panels (Console, Diagnostics):
// a fixed-position list of actions, clamped inside the window, closed by any
// outside mousedown, Escape, blur or resize. Also hosts the clipboard helper
// those menus use, with the execCommand fallback the WebView host needs.
//=============================================================================
import { useEffect } from 'react';
//=============================================================================
const MENU_WIDTH = 140;
const MENU_ITEM_HEIGHT = 30;
const MENU_MARGIN = 10;
export interface PanelContextMenuItem {
  label: string;
  onSelect: () => void;
}
//=============================================================================
export interface PanelContextMenuPosition {
  left: number;
  top: number;
}
//=============================================================================
export async function copyText(text: string): Promise<void> {
  try {
    if (!navigator.clipboard?.writeText) throw new Error('Clipboard API unavailable');
    await navigator.clipboard.writeText(text);
  } catch (_error) {
    const textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.setAttribute('readonly', 'true');
    textarea.style.position = 'fixed';
    textarea.style.left = '-9999px';
    document.body.appendChild(textarea);
    textarea.select();
    document.execCommand('copy');
    textarea.remove();
  }
}
//=============================================================================
// Keeps a menu of `itemCount` rows opened at the pointer inside the viewport.
export function clampMenuPosition(
  clientX: number,
  clientY: number,
  itemCount: number
): PanelContextMenuPosition {
  const height = MENU_ITEM_HEIGHT * Math.max(1, itemCount) + 8;
  const maxLeft = Math.max(MENU_MARGIN, window.innerWidth - MENU_WIDTH - MENU_MARGIN);
  const maxTop = Math.max(MENU_MARGIN, window.innerHeight - height - MENU_MARGIN);
  return {
    left: Math.max(MENU_MARGIN, Math.min(clientX, maxLeft)),
    top: Math.max(MENU_MARGIN, Math.min(clientY, maxTop)),
  };
}
//=============================================================================
export function PanelContextMenu({
  position,
  items,
  onClose,
  className,
}: {
  position: PanelContextMenuPosition;
  items: PanelContextMenuItem[];
  onClose: () => void;
  className?: string;
}): JSX.Element {
  useEffect(() => {
    const closeOnEscape = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
    };
    document.addEventListener('mousedown', onClose);
    document.addEventListener('keydown', closeOnEscape);
    window.addEventListener('blur', onClose);
    window.addEventListener('resize', onClose);
    return () => {
      document.removeEventListener('mousedown', onClose);
      document.removeEventListener('keydown', closeOnEscape);
      window.removeEventListener('blur', onClose);
      window.removeEventListener('resize', onClose);
    };
  }, [onClose]);

  return (
    <div
      className={`context-menu show nf-panel-context-menu${className ? ` ${className}` : ''}`}
      role="menu"
      style={{ left: position.left, top: position.top }}
      onMouseDown={(event) => event.stopPropagation()}
    >
      {items.map((item) => (
        <button
          type="button"
          className="context-menu-item"
          role="menuitem"
          key={item.label}
          onClick={() => {
            item.onSelect();
            onClose();
          }}
        >
          <span>{item.label}</span>
        </button>
      ))}
    </div>
  );
}
//=============================================================================
export default PanelContextMenu;
//=============================================================================
