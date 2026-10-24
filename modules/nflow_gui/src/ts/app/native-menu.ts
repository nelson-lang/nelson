//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  installNativeMenu,
  isNativeMenuAvailable,
  publishNativeMenu,
  type NativeMenuDescriptor,
  type NativeMenuItem,
} from '@nelson/native-menu';
import { t } from '../utils/i18n.js';
import { handleMenuAction } from './menu-handlers.js';
import { getRecentFiles } from './recent-files.js';
//=============================================================================
const separator = (): NativeMenuItem => ({ separator: true });
const action = (label: string, name: string, shortcut?: string): NativeMenuItem => ({
  label: t(label),
  action: name,
  shortcut,
});
//=============================================================================
function recentItems(): NativeMenuItem[] {
  const items: NativeMenuItem[] = getRecentFiles().map((path) => ({
    label: path.split(/[\\/]/).pop() || path,
    action: `openRecent:${encodeURIComponent(path)}`,
  }));
  items.push({
    label: t('Clear Recent'),
    action: 'clearRecent',
    enabled: items.length > 0,
  });
  return items;
}
//=============================================================================
export function nflowNativeMenuDescriptor(): NativeMenuDescriptor {
  return {
    menus: [
      {
        label: 'NFlow',
        items: [
          action('About', 'about'),
          separator(),
          { label: t('Hide NFlow'), role: 'hide', shortcut: 'cmd+h' },
          { label: t('Hide Others'), role: 'hideOthers', shortcut: 'alt+cmd+h' },
          { label: t('Show All'), role: 'showAll' },
          separator(),
          { label: t('Close NFlow'), role: 'closeWindow', shortcut: 'cmd+w' },
        ],
      },
      {
        label: t('File'),
        items: [
          action('New', 'new', 'cmd+n'),
          separator(),
          action('Open...', 'open', 'cmd+o'),
          { label: t('Open Recent'), items: recentItems() },
          separator(),
          action('Open SSP...', 'openSsp'),
          action('Import FMU...', 'importFmu'),
          separator(),
          action('Save', 'save', 'cmd+s'),
          action('Save As...', 'saveAs', 'shift+cmd+s'),
          action('Export as SSP...', 'exportSsp'),
          action('Export as FMU...', 'exportFmu'),
          { label: t('Export Model to Image'), items: [action('SVG', 'export-svg')] },
          separator(),
          { label: t('Close Window'), role: 'closeWindow', shortcut: 'cmd+w' },
        ],
      },
      {
        label: t('Edit'),
        items: [
          action('Undo', 'undo', 'cmd+z'),
          action('Redo', 'redo', 'shift+cmd+z'),
          separator(),
          action('Cut', 'cut', 'cmd+x'),
          action('Copy', 'copy', 'cmd+c'),
          action('Paste', 'paste', 'cmd+v'),
          action('Delete', 'delete'),
          separator(),
          action('Auto-arrange Blocks', 'autoArrange', 'shift+cmd+l'),
        ],
      },
      {
        label: t('View'),
        items: [
          action('Reset View', 'resetView', 'cmd+0'),
          action('Zoom In', 'zoomIn', 'cmd+='),
          action('Zoom Out', 'zoomOut', 'cmd+-'),
          separator(),
          action('Console', 'toggleConsole'),
          action('Minimap', 'toggleMinimap'),
        ],
      },
      {
        label: t('Window'),
        items: [
          { label: t('Minimize'), role: 'minimize', shortcut: 'cmd+m' },
          { label: t('Zoom'), role: 'zoom' },
        ],
      },
      {
        label: t('Help'),
        items: [action('About', 'about')],
      },
    ],
  };
}
//=============================================================================
let removeNativeMenuListeners: (() => void) | undefined;
export function installNFlowNativeMenu(): boolean {
  if (!isNativeMenuAvailable()) return false;
  removeNativeMenuListeners?.();
  removeNativeMenuListeners = installNativeMenu(nflowNativeMenuDescriptor, handleMenuAction);
  const titlebar = document.querySelector<HTMLElement>('.titlebar');
  if (titlebar) titlebar.style.display = 'none';
  return true;
}
//=============================================================================
export function refreshNFlowNativeMenu(): boolean {
  return publishNativeMenu(nflowNativeMenuDescriptor());
}
//=============================================================================
