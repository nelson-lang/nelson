//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import type { DockviewApi } from 'dockview';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import {
  isFigureInWindow,
  setDockApi,
  updateFigurePanelTitle,
  markFigureInWindow,
  unmarkFigureInWindow,
} from '../../src/desktop/desktop';
//=============================================================================
// The windowed-figure registry tracks which figures live in their own window so
// the desktop never re-docks them during figure.list reconciliation.
describe('windowed-figure registry', () => {
  it('marks and unmarks a figure as living in its own window', () => {
    expect(isFigureInWindow(42)).toBe(false);
    markFigureInWindow(42);
    expect(isFigureInWindow(42)).toBe(true);
    unmarkFigureInWindow(42);
    expect(isFigureInWindow(42)).toBe(false);
  });

  it('tracks figures independently', () => {
    markFigureInWindow(1);
    markFigureInWindow(2);
    expect(isFigureInWindow(1)).toBe(true);
    expect(isFigureInWindow(2)).toBe(true);
    unmarkFigureInWindow(1);
    expect(isFigureInWindow(1)).toBe(false);
    expect(isFigureInWindow(2)).toBe(true);
    unmarkFigureInWindow(2);
  });
});
//=============================================================================
describe('empty figure surface', () => {
  it('uses the default figure color while the first display list is loading', () => {
    const css = readFileSync(resolve(process.cwd(), 'src/panels.css'), 'utf8');
    expect(css).toMatch(
      /\.nelson-shell \.figure-panel__canvas\s*{[^}]*background:\s*rgb\(245, 245, 245\)/s
    );
  });

  it('deploys the rebuilt bundle to development runtime trees', () => {
    const postbuild = readFileSync(resolve(process.cwd(), 'scripts/postbuild.mjs'), 'utf8');
    expect(postbuild).toContain("const runtimePlatforms = ['linux', 'macOS', 'x64', 'ARM64'];");
    expect(postbuild).toMatch(/cpSync\(dist, target, \{ recursive: true \}\)/);
  });
});
//=============================================================================
describe('figure title synchronization', () => {
  it('updates both the tab and the figure component after title reconciliation', () => {
    const panel = {
      title: 'Figure 1',
      params: { title: 'Figure 1' },
      api: { setTitle: vi.fn(), updateParameters: vi.fn() },
    };
    const event = () => ({ dispose() {} });
    setDockApi({
      getPanel: () => panel,
      onDidActivePanelChange: event,
      onDidAddPanel: event,
      onDidRemovePanel: event,
      onDidMovePanel: event,
    } as unknown as DockviewApi);
    updateFigurePanelTitle(0, 'Point inspection');
    expect(panel.api.setTitle).toHaveBeenCalledWith('Point inspection');
    expect(panel.api.updateParameters).toHaveBeenCalledWith({ title: 'Point inspection' });
  });
});
//=============================================================================
