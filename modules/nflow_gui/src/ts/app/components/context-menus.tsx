//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import type { ReactNode } from 'react';
import { localizeDOM } from '../../utils/i18n.js';
//=============================================================================
const CONTEXT_MENUS_ROOT_ID = 'nflowContextMenusRoot';
let contextMenusRoot: Root | null = null;
let contextMenusHost: HTMLElement | null = null;
function ContextMenuItem({
  action,
  label,
  className = 'context-menu-item',
  children,
}: {
  action?: string;
  label?: string;
  className?: string;
  children?: ReactNode;
}): JSX.Element {
  return (
    <div className={className} data-action={action}>
      {label ? <span data-i18n={label}>{label}</span> : null}
      {children}
    </div>
  );
}
//=============================================================================
function ContextMenus(): JSX.Element {
  return (
    <>
      <div id="blockContextMenu" className="context-menu">
        <ContextMenuItem action="mirrorHorizontal" label="Mirror Horizontal" />
        <ContextMenuItem action="mirrorVertical" label="Mirror Vertical" />
        <ContextMenuItem action="rotateLeft" label="Rotate Left 90°" />
        <ContextMenuItem action="rotateRight" label="Rotate Right 90°" />
        <ContextMenuItem action="copy" label="Copy" />
        <ContextMenuItem action="cut" label="Cut" />
        <div className="context-menu-divider context-menu-divider--align" />
        <ContextMenuItem
          className="context-menu-item context-menu-item--align context-menu-item--has-submenu"
          label="Align Blocks"
        >
          <div className="context-menu-submenu">
            <ContextMenuItem action="alignLeft" label="Align Left" />
            <ContextMenuItem action="alignCenterH" label="Align Center Horizontally" />
            <ContextMenuItem action="alignRight" label="Align Right" />
            <div className="context-menu-divider" />
            <ContextMenuItem action="alignTop" label="Align Top" />
            <ContextMenuItem action="alignMiddleV" label="Align Middle Vertically" />
            <ContextMenuItem action="alignBottom" label="Align Bottom" />
            <div className="context-menu-divider context-menu-divider--distribute" />
            <ContextMenuItem
              className="context-menu-item context-menu-item--distribute"
              action="distributeH"
              label="Distribute Horizontally"
            />
            <ContextMenuItem
              className="context-menu-item context-menu-item--distribute"
              action="distributeV"
              label="Distribute Vertically"
            />
            <div className="context-menu-divider" />
            <ContextMenuItem action="snapToGrid" label="Snap to Grid" />
          </div>
        </ContextMenuItem>
        <div className="context-menu-divider" />
        <ContextMenuItem action="createSubsystem" label="Create Subsystem from Selection" />
        <div className="context-menu-divider" />
        <ContextMenuItem action="delete" label="Delete" />
        <ContextMenuItem action="properties" label="Properties" />
        <ContextMenuItem action="blockHelp" label="Block Help" />
      </div>

      <div id="canvasContextMenu" className="context-menu">
        <ContextMenuItem action="resetView" label="Reset View" />
        <ContextMenuItem action="autoArrange" label="Auto-arrange Blocks" />
        <ContextMenuItem action="recalculateWires" label="Recalculate Wire Paths" />
        <ContextMenuItem action="autoRouteWire" label="Auto-route Wire" />
        <div className="context-menu-divider" />
        <ContextMenuItem action="selectAll" label="Select All" />
        <ContextMenuItem action="paste" label="Paste" />
      </div>

      <div id="libraryBlockContextMenu" className="context-menu">
        <ContextMenuItem action="libraryBlockHelp" label="Block Help" />
      </div>
    </>
  );
}
//=============================================================================
function ensureContextMenusHost(): HTMLElement {
  let host = document.getElementById(CONTEXT_MENUS_ROOT_ID);
  if (!host) {
    host = document.createElement('div');
    host.id = CONTEXT_MENUS_ROOT_ID;
    host.dataset.reactRoot = 'context-menus';
    host.style.display = 'contents';
    document.body.appendChild(host);
  }
  if (host !== contextMenusHost) {
    contextMenusHost = host;
    contextMenusRoot = createRoot(host);
  }
  return host;
}
//=============================================================================
export function mountContextMenus(): void {
  if (
    document.getElementById('blockContextMenu') &&
    document.getElementById('libraryBlockContextMenu')
  )
    return;
  const host = ensureContextMenusHost();
  flushSync(() => {
    contextMenusRoot?.render(<ContextMenus />);
  });
  try {
    localizeDOM(host);
  } catch (e) {
    /* ignore localization errors */
  }
}
//=============================================================================
