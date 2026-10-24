//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
import { desktopActions, executeDesktopAction, type DesktopAction } from './actions';
//=============================================================================
const separator = (): NativeMenuItem => ({ separator: true });
function nativeShortcut(shortcut: string | undefined): string | undefined {
  if (!shortcut) return undefined;
  return shortcut
    .replace('⇧', 'shift+')
    .replace('⌘', 'cmd+')
    .replace(/^Ctrl\+/i, 'ctrl+')
    .toLowerCase();
}
//=============================================================================
function actionItem(action: DesktopAction): NativeMenuItem {
  return {
    label: action.title,
    action: action.id,
    shortcut: nativeShortcut(action.shortcut),
    enabled: action.enabled !== false,
    checked: action.checked,
  };
}
//=============================================================================
function descriptorItems(actions: DesktopAction[]) {
  const byId = new Map(actions.map((action) => [action.id, action]));
  const item = (id: string): NativeMenuItem => {
    const action = byId.get(id);
    return action ? actionItem(action) : { label: id, action: id, enabled: false };
  };
  const category = (name: string): NativeMenuItem[] =>
    actions.filter((action) => action.category === name).map(actionItem);
  return { item, category };
}
//=============================================================================
export function nelsonNativeMenuDescriptor(): NativeMenuDescriptor {
  const actions = desktopActions();
  const { category, item } = descriptorItems(actions);
  const recent = category('Recent File');
  const savedLayouts = category('Saved Layout');
  const openPanels = actions
    .filter((action) => action.category === 'Window' && action.id.startsWith('window.panel:'))
    .map(actionItem);
  const windowItems = [
    'window.terminal',
    'file.editor',
    'window.nflow',
    'tools.packageManager',
    'window.tile',
  ]
    .filter((id) => actions.some((action) => action.id === id))
    .map(item);

  return {
    menus: [
      {
        label: 'Nelson',
        items: [
          item('help.about'),
          { ...item('edit.preferences'), shortcut: 'cmd+,' },
          separator(),
          { label: 'Hide Nelson', role: 'hide', shortcut: 'cmd+h' },
          { label: 'Hide Others', role: 'hideOthers', shortcut: 'alt+cmd+h' },
          { label: 'Show All', role: 'showAll' },
          separator(),
          { ...item('file.exit'), label: 'Quit Nelson', shortcut: 'cmd+q' },
        ],
      },
      {
        label: 'File',
        items: [
          {
            label: 'New',
            items: [item('file.newScript'), item('file.newFunction'), item('file.newFigure')],
          },
          separator(),
          item('file.execute'),
          item('file.openEditor'),
          item('file.import'),
          separator(),
          item('file.changeDirectory'),
          item('file.displayDirectory'),
          item('file.setpath'),
          separator(),
          item('file.loadWorkspace'),
          item('file.saveWorkspace'),
          { label: 'Recent Files', items: recent },
        ],
      },
      {
        label: 'Edit',
        items: [
          item('edit.selectAll'),
          separator(),
          item('edit.clearConsole'),
          item('edit.clearWorkspace'),
          separator(),
          item('edit.preferences'),
        ],
      },
      {
        label: 'View',
        items: [
          ...category('View'),
          separator(),
          { label: 'Theme', items: category('Theme') },
          separator(),
          { label: 'Layout', items: [...category('Layout'), ...savedLayouts] },
        ],
      },
      {
        label: 'Window',
        items: [...windowItems, ...(openPanels.length ? [separator(), ...openPanels] : [])],
      },
      {
        label: 'Help',
        items: [
          item('help.documentation'),
          item('help.examples'),
          item('help.checkUpdates'),
          separator(),
          item('help.website'),
          item('help.issues'),
        ],
      },
    ],
  };
}
//=============================================================================
export function installNelsonNativeMenu(): () => void {
  return installNativeMenu(nelsonNativeMenuDescriptor, executeDesktopAction);
}
//=============================================================================
export function refreshNelsonNativeMenu(): boolean {
  return publishNativeMenu(nelsonNativeMenuDescriptor());
}
//=============================================================================
export { isNativeMenuAvailable };
//=============================================================================
