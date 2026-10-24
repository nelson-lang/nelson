//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState, useSyncExternalStore } from 'react';
import { getDockApi, listOpenPanels, subscribeDock } from '../desktop/desktop';
import { getRecentFiles } from '../desktop/recentFiles';
import { listNamedLayouts } from '../desktop/layout';
import { desktopActions, executeDesktopAction } from '../desktop/actions';
import { useShellVisibility } from '../desktop/shellState';
import { handleMenuKeyDown } from '../desktop/menuKeyboard';
import { EditMenu, FileMenu, HelpMenu, ViewMenu, WindowMenu } from './MenuContents';
import { getDesktopCapabilities, subscribeDesktopCapabilities } from '../desktop/capabilities';
import {
  installNelsonNativeMenu,
  isNativeMenuAvailable,
  refreshNelsonNativeMenu,
} from '../desktop/nativeMenu';
//=============================================================================
const MENU_NAMES = ['file', 'edit', 'view', 'window', 'help'];
interface MenuItemProps {
  children: JSX.Element;
  label: string;
  name: string;
  open: string | null;
  setOpen: (name: string | null) => void;
}
//=============================================================================
function MenuItem({ children, label, name, open, setOpen }: MenuItemProps): JSX.Element {
  const focusTrigger = () =>
    document
      .querySelector<HTMLButtonElement>(`.menubar__item[data-menu="${name}"] > button`)
      ?.focus();
  const openAndFocus = () => {
    setOpen(name);
    setTimeout(() =>
      document
        .querySelector<HTMLButtonElement>(`.menubar__item[data-menu="${name}"] .menu button`)
        ?.focus()
    );
  };
  return (
    <div className="menubar__item" data-menu={name}>
      <button
        type="button"
        role="menuitem"
        aria-haspopup="menu"
        aria-expanded={open === name}
        onClick={() => setOpen(open === name ? null : name)}
        onKeyDown={(event) => {
          if (event.key === 'ArrowDown') {
            event.preventDefault();
            openAndFocus();
          }
        }}
      >
        {label}
      </button>
      {open === name ? (
        <div
          className="menu"
          role="menu"
          onKeyDown={(event) =>
            handleMenuKeyDown(event, () => {
              setOpen(null);
              setTimeout(focusTrigger);
            })
          }
        >
          {children}
        </div>
      ) : null}
    </div>
  );
}
//=============================================================================
export function MenuBar(): JSX.Element {
  const [open, setOpen] = useState<string | null>(null);
  useShellVisibility();
  const capabilities = useSyncExternalStore(
    subscribeDesktopCapabilities,
    getDesktopCapabilities,
    getDesktopCapabilities
  );
  useSyncExternalStore(
    subscribeDock,
    () =>
      `${getDockApi()?.activePanel?.id ?? ''}:${listOpenPanels()
        .map((panel) => panel.id)
        .join(',')}`,
    () => ''
  );
  const nativeMenu = isNativeMenuAvailable();

  useEffect(() => {
    if (!nativeMenu) return;
    return installNelsonNativeMenu();
  }, [nativeMenu]);

  useEffect(() => {
    if (nativeMenu) refreshNelsonNativeMenu();
  });

  // Close the open menu on a pointer down outside the menu bar. The open
  // dropdown renders inside the bar, so it counts as inside.
  useEffect(() => {
    if (!open || nativeMenu) return undefined;
    const onPointerDown = (event: PointerEvent) => {
      const target = event.target as Element | null;
      if (target?.closest?.('.menubar')) return;
      setOpen(null);
    };
    document.addEventListener('pointerdown', onPointerDown, true);
    return () => document.removeEventListener('pointerdown', onPointerDown, true);
  }, [open, nativeMenu]);

  const close = () => setOpen(null);
  const run = (id: string) => {
    executeDesktopAction(id);
    close();
  };
  const onTopKeyDown = (event: React.KeyboardEvent) => {
    if (event.key === 'Escape') return close();
    if (!open || (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight')) return;
    event.preventDefault();
    const current = MENU_NAMES.indexOf(open);
    const delta = event.key === 'ArrowRight' ? 1 : -1;
    const next = MENU_NAMES[(current + delta + MENU_NAMES.length) % MENU_NAMES.length];
    setOpen(next);
    document
      .querySelector<HTMLButtonElement>(`.menubar__item[data-menu="${next}"] > button`)
      ?.focus();
  };

  const actions = desktopActions();
  const layoutActions = actions.filter((action) => action.category === 'Layout');
  const shared = { close, run };
  const item = (name: string, label: string, children: JSX.Element) => (
    <MenuItem name={name} label={label} open={open} setOpen={setOpen}>
      {children}
    </MenuItem>
  );

  if (nativeMenu) return <></>;

  return (
    <div
      className="menubar"
      role="menubar"
      aria-label="Application menu"
      data-focus-zone="menubar"
      onKeyDown={onTopKeyDown}
    >
      <span className="menubar__brand">Nelson</span>
      {item(
        'file',
        'File',
        <FileMenu {...shared} recent={open === 'file' ? getRecentFiles() : []} />
      )}
      {item('edit', 'Edit', <EditMenu run={run} />)}
      {item(
        'view',
        'View',
        <ViewMenu
          close={close}
          layoutActions={layoutActions}
          namedLayouts={listNamedLayouts()}
          run={run}
          themeActions={actions.filter((action) => action.category === 'Theme')}
          viewActions={actions.filter((action) => action.category === 'View')}
        />
      )}
      {item(
        'window',
        'Window',
        <WindowMenu nflowAvailable={capabilities.nflow} openPanels={listOpenPanels()} run={run} />
      )}
      {item('help', 'Help', <HelpMenu run={run} />)}
      <div className="menubar__spacer" />
    </div>
  );
}
//=============================================================================
