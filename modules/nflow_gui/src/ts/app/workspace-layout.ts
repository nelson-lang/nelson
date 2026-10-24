//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getUiPref, setUiPref } from './ui-prefs.js';
import { onAppEvent } from './app-events.js';
//=============================================================================
export type InspectorTab = 'properties' | 'simulation';
const OPEN_KEY = 'nf-inspector-open';
const WIDTH_KEY = 'nf-inspector-width';
const TAB_KEY = 'nf-inspector-tab';
const DEFAULT_WIDTH = 306;
const MIN_WIDTH = 240;
const MAX_WIDTH = 480;
function readInspectorWidth(): string {
  const saved = Number.parseFloat(getUiPref(WIDTH_KEY, `${DEFAULT_WIDTH}px`) || '');
  const width = Number.isFinite(saved)
    ? Math.min(MAX_WIDTH, Math.max(MIN_WIDTH, saved))
    : DEFAULT_WIDTH;
  return `${width}px`;
}
//=============================================================================
function inspectorPane(): HTMLElement | null {
  return document.querySelector('.inspector-pane');
}
//=============================================================================
function setCompactBodyState(open: boolean): void {
  document.body.classList.toggle('inspector-open-compact', open && window.innerWidth < 1100);
}
//=============================================================================
export function activateInspectorTab(tab: InspectorTab): void {
  document.querySelectorAll<HTMLElement>('[data-inspector-tab]').forEach((button) => {
    const active = button.dataset.inspectorTab === tab;
    button.classList.toggle('active', active);
    button.setAttribute('aria-selected', active ? 'true' : 'false');
  });
  document
    .querySelectorAll<HTMLElement>('.inspector-pane .sidebar-panel')
    .forEach((panel) => panel.classList.toggle('active', panel.id === `panel-${tab}`));
  setUiPref(TAB_KEY, tab);
}
//=============================================================================
export function setInspectorOpen(open: boolean, persist = true): void {
  const pane = inspectorPane();
  if (!pane) return;
  pane.classList.toggle('collapsed', !open);
  pane.classList.toggle('force-open', open);
  setCompactBodyState(open);
  if (persist) setUiPref(OPEN_KEY, open ? '1' : '0');
  window.dispatchEvent(new Event('resize'));
}
//=============================================================================
export function showInspector(tab: InspectorTab = 'properties'): void {
  activateInspectorTab(tab);
  setInspectorOpen(true);
}
//=============================================================================
function bindResize(pane: HTMLElement): void {
  const handle = pane.querySelector<HTMLElement>('.inspector-resize-handle');
  if (!handle || handle.dataset.bound === '1') return;
  handle.dataset.bound = '1';
  handle.addEventListener('pointerdown', (event) => {
    const startX = event.clientX;
    const startWidth = pane.offsetWidth || DEFAULT_WIDTH;
    const onMove = (moveEvent: PointerEvent): void => {
      const width = Math.max(
        MIN_WIDTH,
        Math.min(MAX_WIDTH, startWidth + startX - moveEvent.clientX)
      );
      pane.style.width = `${width}px`;
    };
    const onUp = (): void => {
      window.removeEventListener('pointermove', onMove);
      window.removeEventListener('pointerup', onUp);
      setUiPref(WIDTH_KEY, pane.style.width);
      window.dispatchEvent(new Event('resize'));
    };
    window.addEventListener('pointermove', onMove);
    window.addEventListener('pointerup', onUp);
    event.preventDefault();
  });
}
//=============================================================================
export function initInspectorDock(): void {
  const pane = inspectorPane();
  if (!pane || pane.dataset.bound === '1') return;
  pane.dataset.bound = '1';
  pane.style.width = readInspectorWidth();

  document.querySelectorAll<HTMLElement>('[data-inspector-tab]').forEach((button) => {
    button.addEventListener('click', () => {
      showInspector(button.dataset.inspectorTab === 'simulation' ? 'simulation' : 'properties');
    });
  });
  document
    .getElementById('inspectorCollapseBtn')
    ?.addEventListener('click', () => setInspectorOpen(false));
  onAppEvent('nf-inspector-show', (detail) => {
    showInspector((detail?.tab as InspectorTab | undefined) ?? 'properties');
  });
  window.addEventListener('resize', () => {
    if (window.innerWidth >= 1100) document.body.classList.remove('inspector-open-compact');
    else setCompactBodyState(!pane.classList.contains('collapsed'));
  });
  bindResize(pane);

  const savedTab = getUiPref(TAB_KEY, 'properties') === 'simulation' ? 'simulation' : 'properties';
  activateInspectorTab(savedTab);
  // On phones the inspector is a drawer that overlays the canvas, so it starts
  // closed regardless of a width preference saved on a wider screen.
  const mobile = window.innerWidth <= 768;
  const defaultOpen = window.innerWidth >= 1280;
  const savedOpen = getUiPref(OPEN_KEY, null);
  setInspectorOpen(mobile ? false : savedOpen === null ? defaultOpen : savedOpen === '1', false);
}
//=============================================================================
