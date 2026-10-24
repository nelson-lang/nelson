//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { mountSidebarPanels } from './sidebar-panels';
//=============================================================================
describe('mountSidebarPanels toolchain build checkbox', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    try {
      sessionStorage.clear();
      localStorage.clear();
    } catch (e) {
      // ignore
    }
    const sidebar = document.createElement('div');
    sidebar.className = 'sidebar';
    document.body.appendChild(sidebar);
    const inspector = document.createElement('div');
    inspector.id = 'inspectorDock';
    document.body.appendChild(inspector);
  });

  it('disables build checkbox for C when C toolchain is absent', () => {
    mountSidebarPanels();
    const langSel = document.getElementById('codegenLang') as HTMLSelectElement | null;
    const buildChk = document.getElementById('codegenBuildAfterGen') as HTMLInputElement | null;
    expect(langSel).toBeTruthy();
    expect(buildChk).toBeTruthy();
    if (!langSel || !buildChk) return;
    langSel.value = 'c';
    langSel.dispatchEvent(new Event('change'));
    expect(buildChk.disabled).toBe(true);
  });

  it('enables build checkbox for C when C toolchain is present', () => {
    try {
      sessionStorage.setItem('nf-have-c-toolchain', '1');
    } catch (e) {}
    mountSidebarPanels();
    const langSel = document.getElementById('codegenLang') as HTMLSelectElement | null;
    const buildChk = document.getElementById('codegenBuildAfterGen') as HTMLInputElement | null;
    expect(langSel).toBeTruthy();
    expect(buildChk).toBeTruthy();
    if (!langSel || !buildChk) return;
    langSel.value = 'c';
    langSel.dispatchEvent(new Event('change'));
    expect(buildChk.disabled).toBe(false);
  });

  it('disables build checkbox for Rust when Rust toolchain is absent', () => {
    mountSidebarPanels();
    const langSel = document.getElementById('codegenLang') as HTMLSelectElement | null;
    const buildChk = document.getElementById('codegenBuildAfterGen') as HTMLInputElement | null;
    expect(langSel).toBeTruthy();
    expect(buildChk).toBeTruthy();
    if (!langSel || !buildChk) return;
    langSel.value = 'rust';
    langSel.dispatchEvent(new Event('change'));
    expect(buildChk.disabled).toBe(true);
  });

  it('enables build checkbox for Rust when Rust toolchain is present', () => {
    try {
      sessionStorage.setItem('nf-have-rust-toolchain', '1');
    } catch (e) {}
    mountSidebarPanels();
    const langSel = document.getElementById('codegenLang') as HTMLSelectElement | null;
    const buildChk = document.getElementById('codegenBuildAfterGen') as HTMLInputElement | null;
    expect(langSel).toBeTruthy();
    expect(buildChk).toBeTruthy();
    if (!langSel || !buildChk) return;
    langSel.value = 'rust';
    langSel.dispatchEvent(new Event('change'));
    expect(buildChk.disabled).toBe(false);
  });
});
//=============================================================================
describe('mountSidebarPanels', () => {
  let mountSidebarPanels: () => void;

  beforeAll(async () => {
    const mod = await import('./sidebar-panels.tsx');
    mountSidebarPanels = mod.mountSidebarPanels;
  });

  beforeEach(() => {
    document.body.innerHTML = '';
    const sidebar = document.createElement('div');
    sidebar.className = 'sidebar';
    document.body.appendChild(sidebar);
    const inspector = document.createElement('div');
    inspector.id = 'inspectorDock';
    document.body.appendChild(inspector);
  });

  test('populates sidebar when .sidebar exists', () => {
    mountSidebarPanels();

    const panelLibrary =
      document.getElementById('panel-library') || document.querySelector('#panel-library');
    expect(panelLibrary).toBeTruthy();
    const inspectorBody = document.getElementById('inspectorBody');
    expect(inspectorBody).toBeTruthy();
    const diagramName = document.getElementById('diagramName');
    expect(diagramName).toBeTruthy();
    const realTimeMode = document.getElementById('realTimeMode');
    expect(realTimeMode).toBeTruthy();
    const lockedInput = document.getElementById('lockedInput');
    expect(lockedInput).toBeTruthy();
    expect(document.getElementById('modelVersionInput')).toHaveProperty('readOnly', true);
    expect(document.getElementById('authorInput')).toHaveProperty('readOnly', false);
    expect(document.getElementById('createdByInput')).toHaveProperty('readOnly', true);
    expect(document.getElementById('createdInput')).toHaveProperty('readOnly', true);
    expect(document.getElementById('lastSavedByInput')).toHaveProperty('readOnly', true);
    expect(document.getElementById('lastSavedAtInput')).toHaveProperty('readOnly', true);
    expect(document.querySelector('label[for="commentsInput"]')?.textContent).toContain(
      'Description'
    );
    expect(document.getElementById('codegenLang')).toBeTruthy();
    expect(document.getElementById('marginLoopSelect')).toBeNull();
    expect(document.getElementById('marginOutputText')).toBeNull();
    expect(document.getElementById('copyDebug')).toBeNull();
  });

  test('does nothing when .sidebar is missing', () => {
    document.body.innerHTML = '';

    mountSidebarPanels();

    expect(document.getElementById('panel-library')).toBeNull();
  });
});
//=============================================================================
import fs from 'fs';
import path from 'path';
//=============================================================================
test('loads sidebar-panels.tsx if present', async () => {
  const p = path.resolve(
    process.cwd(),
    'modules/nflow_gui/src/ts',
    'app/components/sidebar-panels.tsx'
  );
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import('./sidebar-panels');
  expect(mod).toBeDefined();
});
//=============================================================================
describe('mountSidebarPanels solver tolerance widgets (A4.1)', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    const sidebar = document.createElement('div');
    sidebar.className = 'sidebar';
    document.body.appendChild(sidebar);
    const inspector = document.createElement('div');
    inspector.id = 'inspectorDock';
    document.body.appendChild(inspector);
  });

  it('renders the relTol/absTol/maxStep inputs inside a hidden-by-default group', () => {
    mountSidebarPanels();
    const group = document.getElementById('solverToleranceGroup');
    const rel = document.getElementById('simRelTol') as HTMLInputElement | null;
    const abs = document.getElementById('simAbsTol') as HTMLInputElement | null;
    const max = document.getElementById('simMaxStep') as HTMLInputElement | null;
    expect(group).toBeTruthy();
    expect(rel).toBeTruthy();
    expect(abs).toBeTruthy();
    expect(max).toBeTruthy();
    // Group starts hidden (the default solver is the fixed-step / discrete one).
    expect(group?.style.display).toBe('none');
  });
});
//=============================================================================
