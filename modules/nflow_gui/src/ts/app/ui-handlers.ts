//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getFitToDiagram } from './state.js';
import { getUiPref, setUiPref } from './ui-prefs.js';
import {
  readInspectorSectionOpen,
  writeInspectorSectionOpen,
} from '../blocks/inspector-section-prefs.js';
import { initInspectorDock, setInspectorOpen, showInspector } from './workspace-layout.js';
import { openExampleBrowser } from './example-browser.js';
import {
  handleMenuAction,
  populateOpenRecentMenu,
  updateSelectedBlockHelpAction,
} from './menu-handlers.js';
import {
  getViewBox,
  setViewBox,
  getZoomScale,
  setZoomScale,
  getUpdateStatusBar,
  getViewportSize,
  ensureLeftMargin,
  clampZoomScale,
} from './workspace-handlers.js';
//=============================================================================
declare global {
  interface Window {
    __initSidebarUIRetryCount?: number;
    __nfMenubarDelegated?: boolean;
  }
}
//=============================================================================
let _statusEl: HTMLElement | null = null;
let homeBtn: HTMLElement | null = null;
let zoomInBtn: HTMLElement | null = null;
let zoomOutBtn: HTMLElement | null = null;
const SIDEBAR_TAB_KEY = 'nf-sidebar-tab';
void _statusEl;
export function setStatusElRef(el: HTMLElement | null): void {
  _statusEl = el;
}
//=============================================================================
export function setHomeBtnRef(el: HTMLElement | null): void {
  homeBtn = el;
}
//=============================================================================
export function setZoomInBtnRef(el: HTMLElement | null): void {
  zoomInBtn = el;
}
//=============================================================================
export function setZoomOutBtnRef(el: HTMLElement | null): void {
  zoomOutBtn = el;
}
//=============================================================================
export function initSidebarUI(): void {
  initInspectorDock();
  const activityIcons =
    typeof document !== 'undefined' ? Array.from(document.querySelectorAll('.activity-icon')) : [];
  const sidebarPanels =
    typeof document !== 'undefined'
      ? Array.from(document.querySelectorAll('.sidebar > .sidebar-panel'))
      : [];
  const activitybar =
    typeof document !== 'undefined'
      ? (document.querySelector('.activity-bar') as HTMLElement | null)
      : null;
  const sidebar =
    typeof document !== 'undefined'
      ? (document.querySelector('.sidebar') as HTMLElement | null)
      : null;

  let lastSidebarWidth = 0;
  const readSidebarWidth = (): string | null => {
    const raw = getUiPref('nf-sidebar-width', localStorage.getItem('sidebarWidth'));
    if (!raw) return null;
    const parsed = Number.parseFloat(raw);
    return Number.isFinite(parsed) ? `${Math.min(600, Math.max(200, parsed))}px` : null;
  };
  const getSidebarWidth = (): number =>
    sidebar && !sidebar.classList.contains('collapsed') ? sidebar.offsetWidth : 0;

  let ensureTimeout: number | null = null;
  const scheduleEnsure = (w: number, padding: number) => {
    if (ensureTimeout) window.clearTimeout(ensureTimeout);
    ensureTimeout = window.setTimeout(() => {
      try {
        ensureLeftMargin(w, padding);
        lastSidebarWidth = getSidebarWidth();
      } catch (_e) {}
      ensureTimeout = null;
    }, 160);
  };

  if (activityIcons.length === 0 || sidebarPanels.length === 0) {
    window.__initSidebarUIRetryCount = (window.__initSidebarUIRetryCount || 0) + 1;
    if ((window.__initSidebarUIRetryCount || 0) < 6) setTimeout(initSidebarUI, 120);
    else console.error('[initSidebarUI] giving up after retries');
    return;
  }
  window.__initSidebarUIRetryCount = 0;

  if (activitybar && sidebar) {
    activitybar.addEventListener('dblclick', (_e) => {
      sidebar.classList.toggle('collapsed');
      try {
        setUiPref('nf-sidebar-open', sidebar.classList.contains('collapsed') ? '0' : '1');
      } catch (_e) {}
      if (!sidebar.classList.contains('collapsed')) {
        const savedWidth = readSidebarWidth();
        if (savedWidth) sidebar.style.width = savedWidth;
      }
      try {
        window.dispatchEvent(new Event('resize'));
      } catch (_e) {}
      try {
        const currW = getSidebarWidth();
        if (currW > 0 && lastSidebarWidth === 0) scheduleEnsure(currW, 12);
      } catch (_e) {}
    });
  }

  const resizeHandle =
    typeof document !== 'undefined'
      ? (document.querySelector('.sidebar-resize-handle') as HTMLElement | null)
      : null;
  let isResizing = false;
  let startX = 0;
  let startWidth = 0;

  if (resizeHandle && sidebar) {
    resizeHandle.addEventListener('mousedown', (e: MouseEvent) => {
      isResizing = true;
      startX = e.clientX;
      startWidth = sidebar.offsetWidth;
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
      sidebar.style.transition = 'none';
      e.preventDefault();
    });

    document.addEventListener('mousemove', (e: MouseEvent) => {
      if (!isResizing) return;
      const deltaX = e.clientX - startX;
      const newWidth = Math.max(200, Math.min(600, startWidth + deltaX));
      sidebar.style.width = `${newWidth}px`;
      try {
        window.dispatchEvent(new Event('resize'));
      } catch (_e) {}
    });

    document.addEventListener('mouseup', () => {
      if (!isResizing) return;
      isResizing = false;
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
      sidebar.style.transition = 'width 0.15s ease';
      try {
        setUiPref('nf-sidebar-width', sidebar.style.width);
      } catch (_e) {}
      try {
        window.dispatchEvent(new Event('resize'));
      } catch (_e) {}
      try {
        const w = getSidebarWidth();
        if (w > 0) scheduleEnsure(w, 12);
      } catch (_e) {}
    });
  }

  const savedWidth = typeof window !== 'undefined' ? readSidebarWidth() : null;
  const legacyCollapsed =
    typeof window !== 'undefined' ? localStorage.getItem('sidebarCollapsed') : null;
  const savedOpen =
    typeof window !== 'undefined'
      ? getUiPref(
          'nf-sidebar-open',
          legacyCollapsed === null ? '1' : legacyCollapsed === 'true' ? '0' : '1'
        )
      : '1';
  // On phones the palette is a drawer over the canvas: it starts collapsed
  // whatever width was saved on a wider screen.
  const mobile = typeof window !== 'undefined' && window.innerWidth <= 768;
  if (sidebar) {
    if (savedOpen !== '0' && !mobile) {
      sidebar.classList.remove('collapsed');
      if (savedWidth) sidebar.style.width = savedWidth;
      lastSidebarWidth = getSidebarWidth();
      try {
        ensureLeftMargin(lastSidebarWidth, 28);
      } catch (_e) {}
    } else {
      sidebar.classList.add('collapsed');
      lastSidebarWidth = 0;
    }
  }

  const activateSidebarPanel = (panelId: string): void => {
    activityIcons.forEach((item) => {
      const element = item as HTMLElement;
      element.classList.toggle('active', element.dataset.panel === panelId);
    });
    sidebarPanels.forEach((item) => {
      const element = item as HTMLElement;
      element.classList.toggle('active', element.id === `panel-${panelId}`);
    });
  };

  const savedSidebarTab = getUiPref(SIDEBAR_TAB_KEY, 'library');
  activateSidebarPanel(savedSidebarTab === 'settings' ? 'settings' : 'library');

  activityIcons.forEach((icon) => {
    icon.addEventListener('click', () => {
      const panelId = (icon as HTMLElement).dataset.panel;
      if (!panelId) return;

      if (panelId === 'properties' || panelId === 'simulation') {
        // On a phone the inspector is a drawer: tapping the icon of the tab
        // already showing closes it.
        const pane = document.querySelector('.inspector-pane');
        const inspectorOpen = !!pane && !pane.classList.contains('collapsed');
        const activeTab = document.querySelector<HTMLElement>('[data-inspector-tab].active');
        const onThisTab = activeTab?.dataset.inspectorTab === panelId;
        if (window.innerWidth <= 768 && inspectorOpen && onThisTab) {
          setInspectorOpen(false);
        } else {
          showInspector(panelId);
        }
        return;
      }

      if (panelId === 'examples') {
        openExampleBrowser();
        return;
      }

      const inspectorHidesSidebar =
        window.innerWidth < 1100 && document.body.classList.contains('inspector-open-compact');
      if (inspectorHidesSidebar) {
        setInspectorOpen(false);
      }

      const isCollapsed = sidebar && sidebar.classList.contains('collapsed');
      const isActive = (icon as HTMLElement).classList.contains('active');

      if (!inspectorHidesSidebar && !isCollapsed && isActive && sidebar) {
        sidebar.classList.add('collapsed');
        try {
          setUiPref('nf-sidebar-open', '0');
        } catch (_e) {}
        return;
      }

      if (isCollapsed && sidebar) {
        sidebar.classList.remove('collapsed');
        const currSavedWidth = readSidebarWidth();
        if (currSavedWidth) sidebar.style.width = currSavedWidth;
        try {
          setUiPref('nf-sidebar-open', '1');
        } catch (_e) {}
        try {
          window.dispatchEvent(new Event('resize'));
        } catch (_e) {}
        try {
          const w = getSidebarWidth();
          if (w > 0) scheduleEnsure(w, 12);
        } catch (_e) {}
      }

      activateSidebarPanel(panelId);
      setUiPref(SIDEBAR_TAB_KEY, panelId);
    });
  });

  const sidebarSectionTitles =
    typeof document !== 'undefined'
      ? Array.from(document.querySelectorAll('.sidebar-section-title'))
      : [];
  sidebarSectionTitles.forEach((title) => {
    const sectionId = (title as HTMLElement).dataset.preferenceSection;
    const content = (title as HTMLElement).nextElementSibling as HTMLElement | null;
    if (sectionId && content?.classList.contains('sidebar-section-content')) {
      const expanded = getUiPref(`nf-sidebar-section-${sectionId}-open`, '1') !== '0';
      (title as HTMLElement).classList.toggle('collapsed', !expanded);
      content.classList.toggle('collapsed', !expanded);
      title.setAttribute('aria-expanded', String(expanded));
      const toggle = title.querySelector('.section-toggle') as HTMLElement | null;
      if (toggle) toggle.textContent = expanded ? '▾' : '▸';
    }
    title.addEventListener('click', () => {
      (title as HTMLElement).classList.toggle('collapsed');
      if (content && content.classList.contains('sidebar-section-content')) {
        content.classList.toggle('collapsed');
        const isNowCollapsed = content.classList.contains('collapsed');
        try {
          title.setAttribute('aria-expanded', String(!isNowCollapsed));
        } catch (_e) {}
        const toggle = (title as HTMLElement).querySelector(
          '.section-toggle'
        ) as HTMLElement | null;
        if (toggle) toggle.textContent = isNowCollapsed ? '▸' : '▾';
        if (sectionId) {
          setUiPref(`nf-sidebar-section-${sectionId}-open`, isNowCollapsed ? '0' : '1');
        }
      }
    });
  });

  const sidebarItems =
    typeof document !== 'undefined' ? Array.from(document.querySelectorAll('.sidebar-item')) : [];
  sidebarItems.forEach((item) => {
    item.addEventListener('click', () => {
      sidebarItems.forEach((i) => (i as HTMLElement).classList.remove('selected'));
      (item as HTMLElement).classList.add('selected');
    });
  });

  // One delegated, capture-phase handler on `document`. The menubar lives in the
  // titlebar component, which mounts (and can re-render) AFTER this setup runs,
  // so binding the menubar items directly would capture an empty set and leave
  // every top-level menu dead. Capture phase + stopImmediatePropagation make this
  // handler own both open and close, so no other global click listener can race
  // it back closed. Registered once (guard) as initSidebarUI may retry.
  if (typeof document !== 'undefined' && !window.__nfMenubarDelegated) {
    window.__nfMenubarDelegated = true;
    const closeAllMenubarDropdowns = () =>
      document
        .querySelectorAll('.menubar-dropdown')
        .forEach((d) => ((d as HTMLElement).style.display = 'none'));

    document.addEventListener(
      'click',
      (e: Event) => {
        const target = e.target as HTMLElement | null;
        if (!target || typeof target.closest !== 'function') return;

        if (!target.closest('.menubar')) {
          closeAllMenubarDropdowns();
          return;
        }

        e.stopImmediatePropagation();

        if (target.closest('.menubar-dropdown')) {
          const dropItem = target.closest('.menubar-dropdown .menubar-item') as HTMLElement | null;
          if (!dropItem) return; // divider / chrome: keep the menu open
          const nested = dropItem.querySelector(':scope > .menubar-dropdown') as HTMLElement | null;
          if (nested) {
            const isVisible = nested.style.display === 'block';
            const parent = dropItem.closest('.menubar-dropdown') as HTMLElement | null;
            if (parent) {
              parent.querySelectorAll('.menubar-dropdown').forEach((d) => {
                if (d !== nested) (d as HTMLElement).style.display = 'none';
              });
            }
            nested.style.display = isVisible ? 'none' : 'block';
            return;
          }
          const action = dropItem.dataset.action;
          if (action) {
            if (dropItem.getAttribute('aria-disabled') === 'true') return;
            closeAllMenubarDropdowns();
            handleMenuAction(action);
          }
          return;
        }

        const topItem = target.closest('.menubar > .menubar-item') as HTMLElement | null;
        if (topItem && topItem.dataset.menu) {
          const dropdown = topItem.querySelector(
            ':scope > .menubar-dropdown'
          ) as HTMLElement | null;
          if (dropdown) {
            const isVisible = dropdown.style.display === 'block';
            closeAllMenubarDropdowns();
            dropdown.style.display = isVisible ? 'none' : 'block';
            try {
              if (!isVisible && topItem.dataset.menu === 'file') populateOpenRecentMenu();
              if (!isVisible && topItem.dataset.menu === 'help') updateSelectedBlockHelpAction();
            } catch (_e) {}
          }
        }
      },
      true // capture phase, before any legacy bubble-phase closeAll
    );
  }

  const attachInspectorFieldsetToggles = () => {
    const inspector = document.querySelector('.properties-content') as HTMLElement | null;
    if (!inspector) return;
    Array.from(inspector.querySelectorAll('fieldset.param-group')).forEach((fs) => {
      if ((fs as HTMLElement).dataset.toggleBound) return;
      (fs as HTMLElement).dataset.toggleBound = 'true';
      const legend = fs.querySelector('legend');
      if (legend) {
        const sectionName = legend.textContent?.trim() || 'parameters';
        const open = readInspectorSectionOpen(`group-${sectionName}`, true);
        (fs as HTMLElement).classList.toggle('collapsed', !open);
        legend.setAttribute('role', 'button');
        legend.setAttribute('tabindex', '0');
        legend.setAttribute('aria-expanded', String(open));
        (legend as HTMLElement).style.cursor = 'pointer';
        const toggle = () => {
          const collapsed = (fs as HTMLElement).classList.toggle('collapsed');
          legend.setAttribute('aria-expanded', String(!collapsed));
          writeInspectorSectionOpen(`group-${sectionName}`, !collapsed);
        };
        legend.addEventListener('click', toggle);
        legend.addEventListener('keydown', (event) => {
          if (event.key === 'Enter' || event.key === ' ') {
            event.preventDefault();
            toggle();
          }
        });
      }
    });
  };
  attachInspectorFieldsetToggles();
  const inspectorBodyEl = document.getElementById('inspectorBody');
  if (inspectorBodyEl) {
    const mo = new MutationObserver(() => attachInspectorFieldsetToggles());
    mo.observe(inspectorBodyEl, { childList: true, subtree: true });
  }
}
//=============================================================================
export function initZoomButtons(): void {
  const updateStatusBar = getUpdateStatusBar();
  const fitToDiagram = getFitToDiagram();

  if (homeBtn) {
    homeBtn.addEventListener('click', () => {
      if (typeof fitToDiagram === 'function') fitToDiagram();
    });
  }

  if (zoomInBtn) {
    zoomInBtn.addEventListener('click', () => {
      const newZoomScale = clampZoomScale(getZoomScale() * 1.1);
      const viewBox = getViewBox();
      const center = { x: viewBox.x + viewBox.w / 2, y: viewBox.y + viewBox.h / 2 };
      const { w, h } = getViewportSize();
      const newW = w / newZoomScale;
      const newH = h / newZoomScale;
      setZoomScale(newZoomScale);
      setViewBox({ x: center.x - newW / 2, y: center.y - newH / 2, w: newW, h: newH });
      if (updateStatusBar) updateStatusBar(null, null, newZoomScale);
    });
  }

  if (zoomOutBtn) {
    zoomOutBtn.addEventListener('click', () => {
      const newZoomScale = clampZoomScale(getZoomScale() / 1.1);
      const viewBox = getViewBox();
      const center = { x: viewBox.x + viewBox.w / 2, y: viewBox.y + viewBox.h / 2 };
      const { w, h } = getViewportSize();
      const newW = w / newZoomScale;
      const newH = h / newZoomScale;
      setZoomScale(newZoomScale);
      setViewBox({ x: center.x - newW / 2, y: center.y - newH / 2, w: newW, h: newH });
      if (updateStatusBar) updateStatusBar(null, null, newZoomScale);
    });
  }
}
//=============================================================================
export function initModals(): void {
  // Modals are mounted and wired by the React modal host.
}
//=============================================================================
export default null;
//=============================================================================
