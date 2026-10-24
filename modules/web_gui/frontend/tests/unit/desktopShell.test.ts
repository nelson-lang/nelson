//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, describe, expect, it, vi } from 'vitest';
import type { DockviewApi, DockviewGroupPanel, IDockviewPanel } from 'dockview';
import { readFileSync, readdirSync } from 'node:fs';
import { resolve } from 'node:path';
import { commandMatchScore } from '../../src/components/CommandPalette';
import { desktopActions } from '../../src/desktop/actions';
import {
  clearCollapsedGroups,
  collapseGroup,
  restoreCollapsedGroups,
  snapshotCollapsedGroups,
} from '../../src/desktop/collapse';
import {
  layoutStorageKeys,
  listNamedLayouts,
  sanitizeDockviewForPersistence,
} from '../../src/desktop/layout';
import { panelCanClose } from '../../src/desktop/panelActions';
import { panelToggleTransition } from '../../src/desktop/desktop';
import { normalizeCurrentDirectory } from '../../src/desktop/currentDirectory';
import { formatPrimaryShortcut } from '../../src/desktop/platform';
import {
  FONT_ZOOM_EVENT,
  installBrowserZoomLock,
  type FontZoomDetail,
} from '../../src/desktop/browserZoom';
import { _resetPersistentStorageForTests } from '../../src/desktop/persistentStorage';
import {
  getDesktopCapabilities,
  packageManagerTitle,
  packageUpdatesBadge,
  refreshDesktopCapabilities,
  setDesktopCapabilities,
  setPackageUpdates,
} from '../../src/desktop/capabilities';
//=============================================================================
describe('desktop command ranking', () => {
  it('prioritizes prefixes, then word starts, then contained text', () => {
    expect(commandMatchScore('Workspace Browser', 'work')).toBeGreaterThan(
      commandMatchScore('Open Workspace', 'work')
    );
    expect(commandMatchScore('Open Workspace', 'work')).toBeGreaterThan(
      commandMatchScore('Save MyWorkspace', 'work')
    );
  });

  it('is case insensitive and rejects missing text', () => {
    expect(commandMatchScore('Command Window', 'COMMAND')).toBeGreaterThanOrEqual(0);
    expect(commandMatchScore('Command Window', 'profiler')).toBe(-1);
  });

  it('formats shortcuts using native macOS and Windows/Linux notation', () => {
    expect(formatPrimaryShortcut('p', true, true)).toBe('⇧⌘P');
    expect(formatPrimaryShortcut('p', true, false)).toBe('Ctrl+Shift+P');
    expect(formatPrimaryShortcut('f6', false, true)).toBe('⌘F6');
  });

  it('exposes the examples gallery in actions and the panel rail', () => {
    expect(desktopActions()).toContainEqual(
      expect.objectContaining({ id: 'help.examples', title: 'Examples', icon: 'examples' })
    );
    const sidebar = readFileSync(resolve(process.cwd(), 'src/components/Sidebar.tsx'), 'utf8');
    const menus = readFileSync(resolve(process.cwd(), 'src/components/MenuContents.tsx'), 'utf8');
    expect(sidebar).toContain("id: 'examples'");
    expect(menus).toContain("run('help.examples')");
  });

  it('exposes the package manager (Window menu, rail) only when nmm_gui is available', () => {
    setDesktopCapabilities({ nflow: false, packageManager: false });
    expect(desktopActions().some((action) => action.id === 'tools.packageManager')).toBe(false);
    setDesktopCapabilities({ nflow: false, packageManager: true });
    expect(desktopActions()).toContainEqual(
      expect.objectContaining({
        id: 'tools.packageManager',
        title: 'Package Manager',
        category: 'Window',
        icon: 'package',
      })
    );
    const sidebar = readFileSync(resolve(process.cwd(), 'src/components/Sidebar.tsx'), 'utf8');
    const menus = readFileSync(resolve(process.cwd(), 'src/components/MenuContents.tsx'), 'utf8');
    expect(sidebar).toContain("id: 'packageManager'");
    expect(menus).toContain("run('tools.packageManager')");
    setDesktopCapabilities({ nflow: false, packageManager: false });
  });

  it('shows the known package update count on the package manager action', () => {
    setDesktopCapabilities({ nflow: false, packageManager: true, packageUpdates: 3 });
    const title = () =>
      desktopActions().find((action) => action.id === 'tools.packageManager')?.title;
    expect(title()).toBe('Package Manager (3)');
    // pushed by the Package Manager window, invalid values count as none
    setPackageUpdates(0);
    expect(title()).toBe('Package Manager');
    setPackageUpdates(12);
    expect(getDesktopCapabilities().packageUpdates).toBe(12);
    setPackageUpdates('bogus');
    expect(getDesktopCapabilities().packageUpdates).toBe(0);
    expect(packageUpdatesBadge(0)).toBe('');
    expect(packageUpdatesBadge(4)).toBe('4');
    expect(packageUpdatesBadge(12)).toBe('9+');
    expect(packageManagerTitle(2)).toBe('Package Manager (2)');
    const menus = readFileSync(resolve(process.cwd(), 'src/components/MenuContents.tsx'), 'utf8');
    expect(menus).toContain('packageManagerTitle(capabilities.packageUpdates)');
    const toolbar = readFileSync(
      resolve(process.cwd(), 'src/components/DesktopToolbar.tsx'),
      'utf8'
    );
    const sidebar = readFileSync(resolve(process.cwd(), 'src/components/Sidebar.tsx'), 'utf8');
    expect(toolbar).toContain('desktop-badge');
    expect(sidebar).toContain('desktop-badge');
    setDesktopCapabilities({ nflow: false, packageManager: false });
  });
});
//=============================================================================
describe('browser zoom lock', () => {
  it('blocks browser zoom gestures without blocking other shortcuts', () => {
    const uninstall = installBrowserZoomLock(window);
    const fontZoomDeltas: number[] = [];
    const onFontZoom = (event: Event) =>
      fontZoomDeltas.push((event as CustomEvent<FontZoomDetail>).detail.delta);
    window.addEventListener(FONT_ZOOM_EVENT, onFontZoom);
    try {
      const zoomKey = new KeyboardEvent('keydown', {
        key: '=',
        ctrlKey: true,
        bubbles: true,
        cancelable: true,
      });
      expect(window.dispatchEvent(zoomKey)).toBe(false);
      expect(zoomKey.defaultPrevented).toBe(true);

      const resetKey = new KeyboardEvent('keydown', {
        key: '0',
        ctrlKey: true,
        bubbles: true,
        cancelable: true,
      });
      expect(window.dispatchEvent(resetKey)).toBe(false);

      const findKey = new KeyboardEvent('keydown', {
        key: 'f',
        ctrlKey: true,
        bubbles: true,
        cancelable: true,
      });
      expect(window.dispatchEvent(findKey)).toBe(true);
      expect(findKey.defaultPrevented).toBe(false);

      const wheel = new WheelEvent('wheel', {
        deltaY: -1,
        ctrlKey: true,
        bubbles: true,
        cancelable: true,
      });
      expect(window.dispatchEvent(wheel)).toBe(false);
      expect(wheel.defaultPrevented).toBe(true);

      // Blocked gestures are re-emitted as app font zoom events.
      expect(fontZoomDeltas).toEqual([1, 0, 1]);
    } finally {
      window.removeEventListener(FONT_ZOOM_EVENT, onFontZoom);
      uninstall();
    }
  });
});
//=============================================================================
describe('layout schema v3', () => {
  const values = new Map<string, string>();
  const storage: Storage = {
    get length() {
      return values.size;
    },
    clear: () => values.clear(),
    getItem: (key) => values.get(key) ?? null,
    key: (index) => [...values.keys()][index] ?? null,
    removeItem: (key) => void values.delete(key),
    setItem: (key, value) => void values.set(key, String(value)),
  };

  beforeEach(() => {
    values.clear();
    Object.defineProperty(globalThis, 'localStorage', { value: storage, configurable: true });
    _resetPersistentStorageForTests();
  });

  it('clears legacy session and named layouts once', () => {
    localStorage.setItem(layoutStorageKeys.last, '{"legacy":true}');
    localStorage.setItem(layoutStorageKeys.named, '{"Old":{}}');
    expect(listNamedLayouts()).toEqual([]);
    expect(localStorage.getItem(layoutStorageKeys.last)).toBeNull();
    expect(localStorage.getItem(layoutStorageKeys.named)).toBeNull();
    expect(localStorage.getItem(layoutStorageKeys.schema)).toBe('3');
  });

  it('migrates v2 storage without deleting its layouts', () => {
    localStorage.setItem(layoutStorageKeys.schema, '2');
    localStorage.setItem(layoutStorageKeys.named, '{}');
    listNamedLayouts();
    expect(localStorage.getItem(layoutStorageKeys.named)).toBe('{}');
    expect(localStorage.getItem(layoutStorageKeys.schema)).toBe('3');
  });

  it('removes document panels before a layout can be restored', () => {
    const layout = {
      grid: {
        root: {
          type: 'leaf',
          size: 900,
          data: {
            id: 'main',
            views: ['console', 'editor:C:/work/demo.m', 'fig:1'],
            activeView: 'editor:C:/work/demo.m',
          },
        },
        width: 1200,
        height: 800,
        orientation: 'HORIZONTAL',
      },
      panels: {
        console: { id: 'console', contentComponent: 'console' },
        'editor:C:/work/demo.m': {
          id: 'editor:C:/work/demo.m',
          contentComponent: 'editor',
          params: { path: 'C:/work/demo.m' },
        },
        'fig:1': { id: 'fig:1', contentComponent: 'figure', params: { handle: 1 } },
      },
      activeGroup: 'main',
    } as Parameters<typeof sanitizeDockviewForPersistence>[0];

    const sanitized = sanitizeDockviewForPersistence(layout);
    expect(Object.keys(sanitized.panels)).toEqual(['console']);
    expect(JSON.stringify(sanitized.grid)).not.toContain('demo.m');
    expect(JSON.stringify(sanitized.grid)).not.toContain('fig:1');
    expect(JSON.stringify(sanitized.grid)).toContain('console');
  });
});
//=============================================================================
describe('HiDPI desktop icons', () => {
  const resources = resolve(process.cwd(), '../../../resources');
  const icons = readdirSync(resources)
    .filter((name) => /^(action|panel)-.*\.svg$/.test(name))
    .map((name) => readFileSync(resolve(resources, name), 'utf8'));

  it('uses vector-only SVG assets with a consistent 24 px viewBox', () => {
    for (const icon of icons) {
      expect(icon).toMatch(/<svg\b/);
      expect(icon).toMatch(/viewBox=["']0 0 24 24["']/);
      expect(icon).not.toMatch(/<image\b|<text\b|<filter\b|data:image\/(png|jpe?g)/i);
      const document = new DOMParser().parseFromString(icon, 'image/svg+xml');
      expect(document.querySelector('parsererror')).toBeNull();
    }
  });

  it('tints icons through currentColor and keeps 32 px hit targets', () => {
    const shellCss = readFileSync(resolve(process.cwd(), 'src/shell.css'), 'utf8');
    expect(shellCss).toMatch(/background-color:\s*currentColor/);
    expect(shellCss).toMatch(/mask:\s*var\(--desktop-icon-url\)/);
    expect(shellCss).toMatch(/min-width:\s*32px/);
    expect(shellCss).toMatch(/min-height:\s*32px/);
  });

  it('keeps shell styles scoped and every source file below 900 lines', () => {
    for (const name of ['styles.css', 'console.css', 'panels.css', 'dialogs.css', 'shell.css']) {
      const css = readFileSync(resolve(process.cwd(), 'src', name), 'utf8');
      expect(css.split('\n').length).toBeLessThan(900);
      expect(css).not.toMatch(/^\.(?!nelson-shell(?:\b|[.:#]))/gm);
      expect(css).not.toMatch(/^#(?!root\b)/gm);
    }
  });
});
//=============================================================================
describe('desktop visual contract', () => {
  const themeCss = readFileSync(resolve(process.cwd(), 'src/styles.css'), 'utf8');
  const shellCss = readFileSync(resolve(process.cwd(), 'src/shell.css'), 'utf8');

  it('keeps the common light and dark desktop palette', () => {
    expect(themeCss).toMatch(/\.nelson-shell\s*{[^}]*--bg:\s*#1e1e1e/s);
    expect(themeCss).toMatch(/\.nelson-shell\s*{[^}]*--bg-alt:\s*#252526/s);
    expect(themeCss).toMatch(/\.nelson-shell\s*{[^}]*--fg:\s*#d4d4d4/s);
    expect(themeCss).toMatch(/\.nelson-shell\s*{[^}]*--border:\s*#333333/s);
    expect(themeCss).toMatch(/\.nelson-shell--light\s*{[^}]*--fg:\s*#1f1f1f/s);
    expect(themeCss).toMatch(/\.nelson-shell--light\s*{[^}]*--border:\s*#d4d4d4/s);
  });

  it('keeps the common desktop shell metrics', () => {
    expect(themeCss).toMatch(/--menubar-h:\s*32px/);
    expect(shellCss).toMatch(/\.desktop-toolbar\s*{[^}]*flex:\s*0 0 38px/s);
    expect(shellCss).toMatch(/\.sidebar\s*{[^}]*width:\s*40px/s);
    expect(shellCss).toMatch(/\.sidebar__btn\s*{[^}]*width:\s*32px[^}]*height:\s*32px/s);
    expect(shellCss).toMatch(/\.statusbar\s*{[^}]*height:\s*24px/s);
  });

  it('limits native text selection to meaningful content surfaces', () => {
    expect(shellCss).toMatch(/\.nelson-shell\s*{[^}]*user-select:\s*none/s);
    expect(shellCss).toMatch(
      /\.nelson-shell input,[^{]+\.console-host,[^{]+\.monaco-editor,[^{]+\.help-panel__body,[^{]+\.dialog-card__message\s*{[^}]*user-select:\s*text/s
    );
    expect(shellCss).toMatch(
      /input:is\([^)]*\[type='checkbox'\][^)]*\[type='range'\][^)]*\)\s*{[^}]*user-select:\s*none/s
    );
  });
});
//=============================================================================
describe('desktop shell actions', () => {
  beforeEach(() => setDesktopCapabilities({ nflow: false }));

  it('publishes unique shared actions for panels, layouts and shell chrome', () => {
    const actions = desktopActions();
    const ids = actions.map((action) => action.id);
    expect(new Set(ids).size).toBe(ids.length);
    expect(ids).toEqual(
      expect.arrayContaining([
        'view.console',
        'view.files',
        'view.workspace',
        'view.terminal',
        'view.theme.system',
        'view.theme.light',
        'view.theme.dark',
        'view.toolbar',
        'view.rail',
        'view.status',
        'engine.stop',
        'edit.selectAll',
        'file.newScript',
        'file.openEditor',
        'help.checkUpdates',
        'help.about',
        'layout.default',
        'layout.threeColumns',
        'layout.commandOnly',
        'layout.restore',
      ])
    );
  });

  it('keeps the advertised newsfeed shortcut wired globally', () => {
    const app = readFileSync(resolve(process.cwd(), 'src/App.tsx'), 'utf8');
    const sidebar = readFileSync(resolve(process.cwd(), 'src/components/Sidebar.tsx'), 'utf8');
    expect(sidebar).toContain("{ id: 'newsfeed', icon: 'newsfeed', label: 'News feed', key: '6' }");
    expect(app).toContain("'6': 'newsfeed'");
  });

  it('publishes NFlow only when the module is installed', () => {
    expect(desktopActions().some((action) => action.id === 'window.nflow')).toBe(false);
    setDesktopCapabilities({ nflow: true });
    const action = desktopActions().find((candidate) => candidate.id === 'window.nflow');
    expect(action).toMatchObject({ title: 'NFlow', icon: 'nflow' });
  });

  it('refreshes optional launchers from the backend capability registry', async () => {
    expect(getDesktopCapabilities().nflow).toBe(false);
    expect(await refreshDesktopCapabilities()).toBe(true);
    expect(getDesktopCapabilities().nflow).toBe(true);
  });

  it('keeps Command Window non-closable', () => {
    expect(panelCanClose({ id: 'console' } as IDockviewPanel)).toBe(false);
    expect(panelCanClose({ id: 'workspace' } as IDockviewPanel)).toBe(true);
  });

  it('maps rail clicks to open, focus and close transitions', () => {
    expect(panelToggleTransition(false, false)).toBe('open');
    expect(panelToggleTransition(true, false)).toBe('focus');
    expect(panelToggleTransition(true, true)).toBe('close');
  });

  it('normalizes the shared current-directory state backward-compatibly', () => {
    expect(normalizeCurrentDirectory({ cwd: '/tmp', separator: '/' })).toEqual({
      cwd: '/tmp',
      separator: '/',
      recentDirectories: [],
    });
    expect(normalizeCurrentDirectory({ cwd: 'legacy' })).toEqual({
      cwd: 'legacy',
      separator: '/',
      recentDirectories: [],
    });
    expect(
      normalizeCurrentDirectory({
        cwd: 'C:\\work',
        separator: '\\',
        recentDirectories: ['C:\\work', '', 42 as unknown as string],
      })
    ).toEqual({
      cwd: 'C:\\work',
      separator: '\\',
      recentDirectories: ['C:\\work'],
    });
  });
});
//=============================================================================
describe('collapsed panel serialization', () => {
  beforeEach(() => clearCollapsedGroups());

  it('stores the expanded height and restores the collapsed constraint', () => {
    const setConstraints = vi.fn();
    const setSize = vi.fn();
    const group = {
      id: 'left-files',
      api: {
        height: 420,
        location: { type: 'grid' },
        setConstraints,
        setSize,
      },
    } as unknown as DockviewGroupPanel;
    const api = { groups: [group] } as unknown as DockviewApi;

    collapseGroup(group);
    expect(snapshotCollapsedGroups(api)).toEqual({ 'left-files': 420 });
    expect(setSize).toHaveBeenLastCalledWith({ height: 35 });

    clearCollapsedGroups();
    restoreCollapsedGroups(api, { 'left-files': 420 });
    expect(setConstraints).toHaveBeenLastCalledWith({ maximumHeight: 35, minimumHeight: 35 });
    expect(snapshotCollapsedGroups(api)).toEqual({ 'left-files': 420 });
  });
});
//=============================================================================
