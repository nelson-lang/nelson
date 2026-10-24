//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('mountLayout', () => {
  let mountLayout: () => void;
  let createTitlebar: () => HTMLElement;
  let createMainContainer: () => HTMLElement;

  beforeAll(async () => {
    const mod = await import('./mount-layout.tsx');
    mountLayout = mod.mountLayout;
    createTitlebar = mod.createTitlebar;
    createMainContainer = mod.createMainContainer;
  });

  beforeEach(() => {
    document.body.innerHTML = '';
  });

  test('creates titlebar and main container from React components', () => {
    const titlebar = createTitlebar();
    const container = createMainContainer();

    expect(titlebar.className).toBe('titlebar');
    expect(titlebar.querySelector('#menu-file')).toBeTruthy();
    expect(container.className).toBe('nflow-container');
    expect(container.querySelector('#svgCanvas')).toBeTruthy();
    expect(container.querySelectorAll('.activity-icon').length).toBe(5);
    expect(container.querySelector('.inspector-pane')).toBeTruthy();
    expect(container.querySelectorAll('#gridSettingsBtn').length).toBe(1);
    expect(titlebar.querySelector('#menu-simulation')).toBeTruthy();
    expect(titlebar.querySelector('[data-action="toggleViewControls"]')).toBeTruthy();
    expect(titlebar.querySelector('[data-action="toggleSimulationToolbar"]')).toBeTruthy();
    const selectedHelp = titlebar.querySelector('#selectedBlockHelpAction');
    expect(selectedHelp).toBeTruthy();
    expect(selectedHelp?.getAttribute('aria-disabled')).toBe('true');
    expect(selectedHelp?.textContent).toContain('F1');
  });

  test('mounts full layout once', () => {
    document.body.innerHTML = '<div class="nf-loading-overlay"></div>';

    mountLayout();
    mountLayout();

    expect(document.querySelectorAll('.titlebar').length).toBe(1);
    expect(document.querySelectorAll('.nflow-container').length).toBe(1);
    expect(document.querySelector('.status-bar')).toBeTruthy();
    expect(document.getElementById('fileOpenInput')).toBeTruthy();
    expect(document.getElementById('loadSubsystemInput')).toBeTruthy();
    expect(document.getElementById('loadInput')).toBeTruthy();
  });
});
//=============================================================================
