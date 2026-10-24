//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useState } from 'react';
import type { MenuNode } from '../rpc/types';
import { DesktopIcon } from '../components/DesktopIcon';
//=============================================================================
// The figure's uimenu bar. Top-level menus open a dropdown of their children;
// leaf items send their handle so the engine runs their MenuSelectedFcn. The
// same descriptor drives arbitrarily nested submenus.
function MenuItems({
  items,
  onSelect,
}: {
  items: MenuNode[];
  onSelect: (handle: number) => void;
}): JSX.Element {
  return (
    <ul
      style={{
        listStyle: 'none',
        margin: 0,
        padding: '4px 0',
        minWidth: 160,
        background: '#fff',
        border: '1px solid #b0b0b0',
        boxShadow: '0 2px 8px rgba(0,0,0,0.25)',
      }}
    >
      {items.map((it, i) => {
        const hasChildren = !!it.children && it.children.length > 0;
        return (
          <li
            key={it.handle ?? i}
            className="figure-menu-item"
            style={{
              position: 'relative',
              borderTop: it.separator ? '1px solid #e0e0e0' : undefined,
            }}
          >
            <div
              style={{
                position: 'relative',
                padding: '3px 22px 3px 24px',
                whiteSpace: 'nowrap',
                color: it.disabled ? '#999' : undefined,
                cursor: it.disabled ? 'default' : 'pointer',
              }}
              onClick={(e) => {
                e.stopPropagation();
                if (it.disabled || hasChildren) return;
                onSelect(it.handle);
              }}
            >
              {it.checked ? (
                <span style={{ position: 'absolute', left: 5 }}>
                  <DesktopIcon name="check" size={16} />
                </span>
              ) : null}
              {it.text}
              {hasChildren ? (
                <span style={{ position: 'absolute', right: 3 }}>
                  <DesktopIcon name="right" size={16} />
                </span>
              ) : null}
            </div>
            {hasChildren ? (
              <div
                className="figure-submenu"
                style={{ position: 'absolute', top: -4, left: '100%' }}
              >
                <MenuItems items={it.children!} onSelect={onSelect} />
              </div>
            ) : null}
          </li>
        );
      })}
    </ul>
  );
}
//=============================================================================
// A floating context menu (uicontextmenu) at a screen position. A full-viewport
// backdrop closes it on any outside click or another right-click.
export function ContextMenuPopup({
  items,
  x,
  y,
  onSelect,
  onClose,
}: {
  items: MenuNode[];
  x: number;
  y: number;
  onSelect: (handle: number) => void;
  onClose: () => void;
}): JSX.Element {
  return (
    <div
      style={{ position: 'fixed', inset: 0, zIndex: 1000 }}
      onClick={onClose}
      onContextMenu={(e) => {
        e.preventDefault();
        onClose();
      }}
    >
      <div style={{ position: 'fixed', top: y, left: x }} onClick={(e) => e.stopPropagation()}>
        <MenuItems
          items={items}
          onSelect={(h) => {
            onClose();
            onSelect(h);
          }}
        />
      </div>
    </div>
  );
}
//=============================================================================
export function FigureMenuBar({
  menus,
  onSelect,
}: {
  menus: MenuNode[];
  onSelect: (handle: number) => void;
}): JSX.Element {
  const [open, setOpen] = useState<number | null>(null);
  return (
    <div
      className="figure-menubar"
      style={{
        display: 'flex',
        background: '#f5f5f5',
        borderBottom: '1px solid #d0d0d0',
        position: 'relative',
        zIndex: 5,
        userSelect: 'none',
      }}
      onMouseLeave={() => setOpen(null)}
    >
      {/* Reveal a submenu while its parent item is hovered. */}
      <style>{`.figure-menu-item > .figure-submenu { display: none; }
.figure-menu-item:hover > .figure-submenu { display: block; }`}</style>
      {menus.map((m, i) => {
        const hasChildren = !!m.children && m.children.length > 0;
        return (
          <div key={m.handle ?? i} style={{ position: 'relative' }}>
            <div
              style={{
                padding: '4px 12px',
                cursor: 'pointer',
                background: open === i ? '#e0e0e0' : undefined,
              }}
              onClick={() => {
                if (hasChildren) setOpen(open === i ? null : i);
                else {
                  setOpen(null);
                  onSelect(m.handle);
                }
              }}
              onMouseEnter={() => {
                if (open !== null && hasChildren) setOpen(i);
              }}
            >
              {m.text}
            </div>
            {open === i && hasChildren ? (
              <div
                style={{ position: 'absolute', top: '100%', left: 0, zIndex: 10 }}
                onClick={() => setOpen(null)}
              >
                <MenuItems
                  items={m.children!}
                  onSelect={(h) => {
                    setOpen(null);
                    onSelect(h);
                  }}
                />
              </div>
            ) : null}
          </div>
        );
      })}
    </div>
  );
}
//=============================================================================
