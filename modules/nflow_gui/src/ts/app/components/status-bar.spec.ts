//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for status-bar component
//=============================================================================
describe('mountStatusBar', () => {
  let mountStatusBar: () => void;

  beforeAll(async () => {
    const mod = await import('./status-bar.tsx');
    mountStatusBar = mod.mountStatusBar;
  });

  beforeEach(() => {
    document.body.innerHTML = '';
    const container = document.createElement('div');
    container.className = 'status-bar';
    document.body.appendChild(container);
  });

  test('populates status bar and lock indicator present', () => {
    mountStatusBar();
    const info = document.getElementById('statusBarInfo');
    expect(info).toBeTruthy();
    const lock = document.getElementById('lockIndicator');
    expect(lock).toBeTruthy();
    expect(document.getElementById('stopSimBtn')).toBeNull();
    expect(document.getElementById('diagnosticErrorCount')).toBeTruthy();
  });
});
//=============================================================================
describe('mountStatusBar', () => {
  let mountStatusBar: () => void;

  beforeAll(async () => {
    const mod = await import('./status-bar.tsx');
    mountStatusBar = mod.mountStatusBar;
  });

  beforeEach(() => {
    document.body.innerHTML = '';
    const container = document.createElement('div');
    container.className = 'status-bar';
    document.body.appendChild(container);
  });

  test('renders status bar items into container', () => {
    mountStatusBar();

    expect(document.getElementById('statusBar')).toBeTruthy();
  });

  test('does nothing when .status-bar is absent', () => {
    document.body.innerHTML = '';

    mountStatusBar();

    expect(document.getElementById('statusBar')).toBeNull();
  });
});
//=============================================================================
