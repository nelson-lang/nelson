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
import { DockviewDefaultTab, type IDockviewPanelHeaderProps } from 'dockview';
import { ContextMenu, useContextMenu, type MenuItem } from '../components/ContextMenu';
import { closeDesktopPanel, undockFigure } from '../desktop/desktop';
import { isGroupCollapsed, toggleGroupCollapsed } from '../desktop/collapse';
import { movePanel, panelCanClose, togglePanelFloating } from '../desktop/panelActions';
//=============================================================================
type TabApi = IDockviewPanelHeaderProps['api'];
function findDockTab(api: TabApi, id = api.id): HTMLElement | undefined {
  return [...api.group.element.querySelectorAll<HTMLElement>('.dv-tab')].find(
    (element) => element.querySelector<HTMLElement>('.dv-default-tab')?.dataset.panelId === id
  );
}
//=============================================================================
function useAccessibleDockTab(api: TabApi): void {
  useEffect(() => {
    const updateAccessibility = (isSelected: boolean) => {
      const tab = findDockTab(api);
      if (!tab) return;
      tab.setAttribute('role', 'tab');
      tab.setAttribute('aria-label', api.title ?? api.id);
      tab.setAttribute('aria-selected', String(isSelected));
      const tabList = tab.closest('.dv-tabs-container');
      tabList?.setAttribute('role', 'tablist');
      tabList?.setAttribute('aria-label', 'Panel tabs');
      if (!isSelected) return;
      const content = tab
        .closest('.dv-groupview')
        ?.querySelector<HTMLElement>('.dv-content-container');
      content?.setAttribute('role', 'tabpanel');
      content?.setAttribute('aria-label', api.title ?? api.id);
    };
    const tab = findDockTab(api);
    const onKeyDown = (event: KeyboardEvent) => {
      if (!['ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(event.key)) return;
      const panels = api.group.panels;
      const current = panels.findIndex((panel) => panel.api.id === api.id);
      const next =
        event.key === 'Home'
          ? 0
          : event.key === 'End'
          ? panels.length - 1
          : (current + (event.key === 'ArrowRight' ? 1 : -1) + panels.length) % panels.length;
      event.preventDefault();
      panels[next]?.api.setActive();
      findDockTab(api, panels[next]?.api.id)?.focus();
    };
    tab?.addEventListener('keydown', onKeyDown);
    updateAccessibility(api.isVisible);
    const visibility = api.onDidVisibilityChange((event) => updateAccessibility(event.isVisible));
    const title = api.onDidTitleChange(() => updateAccessibility(api.isVisible));
    return () => {
      tab?.removeEventListener('keydown', onKeyDown);
      visibility.dispose();
      title.dispose();
    };
  }, [api]);
}
//=============================================================================
// Tab renderer for every dock panel, installed as DockviewReact's
// defaultTabComponent (a per-panel tabComponent is frozen into the serialized
// layout, so panels restored from a saved layout would miss it).
//
// Right-click opens the panel actions: float/re-dock, maximize, close, close
// others; all driven through the panel api, so they work for any panel.
// Figures add the browser-window pop-out, the only panel kind with a route
// for it (index.html?figure=N).
export function DesktopTab(props: IDockviewPanelHeaderProps): JSX.Element {
  const { menu, openMenu, closeMenu } = useContextMenu();
  const { api, containerApi } = props;
  useAccessibleDockTab(api);

  const panel = () => containerApi.getPanel(api.id);
  const toggleFloat = () => {
    const current = panel();
    if (current) togglePanelFloating(current, containerApi);
  };

  const closeOthers = () => {
    // Same group only: closing every other panel of the desktop would be a
    // surprising amount of destruction from a tab menu.
    for (const panel of [...api.group.panels]) {
      if (panel.api.id !== api.id && panelCanClose(panel)) void closeDesktopPanel(panel);
    }
  };

  const onContextMenu = (e: React.MouseEvent) => {
    const floating = api.location.type === 'floating';
    const figureHandle = api.id.startsWith('fig:') ? Number(api.id.slice(4)) : null;
    const items: MenuItem[] = [
      {
        label: floating ? 'Re-dock into the desktop' : 'Detach as floating window',
        onClick: toggleFloat,
      },
    ];
    if (figureHandle !== null && !Number.isNaN(figureHandle)) {
      items.push({
        label: 'Undock (separate window)',
        onClick: () => undockFigure(figureHandle, api.title ?? `Figure ${figureHandle + 1}`),
      });
    }
    items.push(
      {
        label: isGroupCollapsed(api.group) ? 'Expand panel' : 'Collapse panel',
        onClick: () => toggleGroupCollapsed(api.group),
        disabled: floating,
      },
      {
        // Maximizing is a grid-only notion: a floating panel is already free.
        label: api.isMaximized() ? 'Restore size' : 'Maximize',
        onClick: () => (api.isMaximized() ? api.exitMaximized() : api.maximize()),
        disabled: floating,
      },
      {
        label: 'Move Left',
        onClick: () => {
          const current = panel();
          if (current) movePanel(current, containerApi, 'left');
        },
        disabled: floating,
      },
      {
        label: 'Move Right',
        onClick: () => {
          const current = panel();
          if (current) movePanel(current, containerApi, 'right');
        },
        disabled: floating,
      },
      {
        label: 'Move Bottom',
        onClick: () => {
          const current = panel();
          if (current) movePanel(current, containerApi, 'bottom');
        },
        disabled: floating,
      },
      { separator: true },
      ...(panelCanClose(containerApi.getPanel(api.id)!)
        ? [
            {
              label: 'Close panel',
              onClick: () => {
                const current = panel();
                if (current) void closeDesktopPanel(current);
              },
            } as MenuItem,
          ]
        : []),
      {
        label: 'Close other tabs',
        onClick: closeOthers,
        disabled: api.group.panels.length < 2,
      }
    );
    openMenu(e, items);
  };

  return (
    <>
      <DockviewDefaultTab
        {...props}
        data-panel-id={api.id}
        hideClose
        onContextMenu={onContextMenu}
        onDoubleClick={(event) => {
          event.preventDefault();
          toggleFloat();
        }}
      />
      <ContextMenu menu={menu} onClose={closeMenu} />
    </>
  );
}
//=============================================================================
