//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { initSidebarUI } from './ui-handlers.js';
import { _resetUiPrefsForTests } from './ui-prefs.js';
//=============================================================================
describe('ui-handlers initSidebarUI (sidebar interactions)', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    _resetUiPrefsForTests();
    localStorage.clear();
    const activityBar = document.createElement('div');
    activityBar.className = 'activity-bar';
    document.body.appendChild(activityBar);

    const sidebar = document.createElement('div');
    sidebar.className = 'sidebar';
    Object.defineProperty(sidebar, 'offsetWidth', { value: 300, configurable: true });
    document.body.appendChild(sidebar);

    const resizeHandle = document.createElement('div');
    resizeHandle.className = 'sidebar-resize-handle';
    document.body.appendChild(resizeHandle);

    const icon = document.createElement('div');
    icon.className = 'activity-icon';
    icon.dataset.panel = 'foo';
    document.body.appendChild(icon);

    const panel = document.createElement('div');
    panel.className = 'sidebar-panel';
    panel.id = 'panel-foo';
    sidebar.appendChild(panel);
  });

  test('dblclick toggles sidebar collapsed and saves to localStorage', () => {
    const activityBar = document.querySelector('.activity-bar') as HTMLElement;
    initSidebarUI();
    const sidebar = document.querySelector('.sidebar') as HTMLElement;
    sidebar.classList.remove('collapsed');
    const ev = new MouseEvent('dblclick', { bubbles: true });
    activityBar.dispatchEvent(ev);
    expect(['0', '1'].includes(localStorage.getItem('nf-sidebar-open') || '')).toBe(true);
  });

  test('activity icon click activates corresponding panel and toggles classes', () => {
    initSidebarUI();
    const icon = document.querySelector('.activity-icon') as HTMLElement;
    const panel = document.getElementById('panel-foo') as HTMLElement;
    expect(icon).toBeTruthy();
    icon.click();
    expect(icon.classList.contains('active')).toBe(true);
    expect(panel.classList.contains('active')).toBe(true);
  });

  test('restoring the left sidebar does not deactivate the inspector panel', () => {
    const inspector = document.createElement('aside');
    inspector.className = 'inspector-pane';
    const inspectorPanel = document.createElement('div');
    inspectorPanel.id = 'panel-properties';
    inspectorPanel.className = 'sidebar-panel active';
    inspector.appendChild(inspectorPanel);
    document.body.appendChild(inspector);

    initSidebarUI();

    expect(inspectorPanel.classList.contains('active')).toBe(true);
  });

  test('activity icon reveals the sidebar when a compact inspector hides it', () => {
    const inspector = document.createElement('aside');
    inspector.className = 'inspector-pane';
    document.body.appendChild(inspector);
    Object.defineProperty(window, 'innerWidth', { value: 1000, configurable: true });

    initSidebarUI();
    inspector.classList.remove('collapsed');
    inspector.classList.add('force-open');
    document.body.classList.add('inspector-open-compact');
    const sidebar = document.querySelector('.sidebar') as HTMLElement;
    const icon = document.querySelector('.activity-icon') as HTMLElement;
    icon.classList.add('active');
    icon.click();

    expect(document.body.classList.contains('inspector-open-compact')).toBe(false);
    expect(inspector.classList.contains('collapsed')).toBe(true);
    expect(sidebar.classList.contains('collapsed')).toBe(false);
  });

  test('resize handle changes sidebar width and saves', () => {
    vi.useFakeTimers();
    initSidebarUI();
    const resizeHandle = document.querySelector('.sidebar-resize-handle') as HTMLElement;
    const down = new MouseEvent('mousedown', { clientX: 100, bubbles: true });
    resizeHandle.dispatchEvent(down);
    const move = new MouseEvent('mousemove', { clientX: 200, bubbles: true });
    document.dispatchEvent(move);
    const up = new MouseEvent('mouseup', { bubbles: true });
    document.dispatchEvent(up);
    vi.runAllTimers();
    expect(localStorage.getItem('nf-sidebar-width')).toBeTruthy();
    vi.useRealTimers();
  });

  test('restores and persists a collapsible sidebar section', () => {
    localStorage.setItem('nf-sidebar-section-variables-open', '0');
    const title = document.createElement('div');
    title.className = 'sidebar-section-title';
    title.dataset.preferenceSection = 'variables';
    title.innerHTML = '<button class="section-toggle"></button>';
    const content = document.createElement('div');
    content.className = 'sidebar-section-content';
    document.body.append(title, content);

    initSidebarUI();
    expect(title.getAttribute('aria-expanded')).toBe('false');
    expect(content.classList.contains('collapsed')).toBe(true);
    title.click();
    expect(localStorage.getItem('nf-sidebar-section-variables-open')).toBe('1');
  });
});
//=============================================================================
