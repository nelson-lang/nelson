//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DesktopIconName } from '../components/DesktopIcon';
import { OPEN_ABOUT_EVENT } from '../components/AboutHost';
import { openDesktopPicker } from '../components/DesktopFilePickerHost';
import { OPEN_IMPORT_EVENT } from '../components/ImportDataHost';
import { OPEN_PREFS_EVENT } from '../components/PreferencesHost';
import { OPEN_SETPATH_EVENT } from '../components/SetPathHost';
import {
  ensureCorePanel,
  focusPanelById,
  getDockApi,
  listOpenPanels,
  openEditor,
  openNotebook,
  openHelp,
  openShell,
  tilePanels,
  toggleCorePanel,
} from './desktop';
import {
  applyNamedLayout,
  buildCommandOnlyLayout,
  buildDefaultLayout,
  buildThreeColumnsLayout,
  listNamedLayouts,
  saveNamedLayout,
} from './layout';
import { clearRecentFiles, getRecentFiles } from './recentFiles';
import { getShellVisibility, setShellVisibility } from './shellState';
import { isWasmRuntime, primaryShortcut } from './platform';
import { webConfirm, webPrompt } from '../components/PromptHost';
import { isEngineBusy, runCommand } from './engineSession';
import { rpc } from '../rpc/client';
import { getThemePref, requestTheme, type ThemePref } from './theme';
import { getDesktopCapabilities, packageManagerTitle } from './capabilities';
//=============================================================================
export interface DesktopAction {
  id: string;
  title: string;
  category: string;
  run: () => void;
  icon?: DesktopIconName;
  shortcut?: string;
  checked?: boolean;
  enabled?: boolean;
}
//=============================================================================
export const SELECT_CONSOLE_ALL_EVENT = 'nelson:console-select-all';
const fire = (name: string, detail?: unknown) =>
  window.dispatchEvent(new CustomEvent(name, { detail }));
function createAndOpen(name: string, content: string): void {
  void rpc('fs.create', { path: name, kind: 'file', content })
    .then(() => openEditor(name))
    .catch(() => undefined);
}
//=============================================================================
function fileAndEditActions(): DesktopAction[] {
  const actions: DesktopAction[] = [
    {
      id: 'file.newScript',
      title: 'New Script',
      category: 'File',
      run: () => createAndOpen('untitled.m', '%% Nelson script\n'),
    },
    {
      id: 'file.newFunction',
      title: 'New Function',
      category: 'File',
      run: () =>
        createAndOpen(
          'untitled.m',
          'function varargout = untitled(varargin)\n  % untitled summary\nend\n'
        ),
    },
    {
      id: 'file.newFigure',
      title: 'New Figure',
      category: 'File',
      run: () => void runCommand('figure'),
    },
    {
      id: 'file.editor',
      title: 'Text Editor',
      category: 'File',
      icon: 'editor',
      run: () => openEditor('untitled.m'),
    },
    {
      id: 'file.execute',
      title: 'Execute…',
      category: 'File',
      icon: 'execute',
      enabled: !isEngineBusy(),
      run: () => openDesktopPicker('execute'),
    },
    {
      id: 'engine.stop',
      title: 'Stop execution',
      category: 'Engine',
      icon: 'stop',
      shortcut: primaryShortcut('C'),
      enabled: isEngineBusy(),
      run: () => void rpc('term.interrupt', { busy: true }).catch(() => undefined),
    },
    {
      id: 'file.import',
      title: 'Import Data…',
      category: 'File',
      run: () => fire(OPEN_IMPORT_EVENT, {}),
    },
    {
      id: 'file.openEditor',
      title: 'Open in Editor…',
      category: 'File',
      run: () => openDesktopPicker('editor'),
    },
    {
      id: 'file.changeDirectory',
      title: 'Change current directory…',
      category: 'File',
      icon: 'folder',
      run: () => openDesktopPicker('directory'),
    },
    {
      id: 'file.displayDirectory',
      title: 'Display current directory',
      category: 'File',
      run: () => void runCommand('disp(pwd())'),
    },
    {
      id: 'file.setpath',
      title: 'Set Path…',
      category: 'File',
      run: () => fire(OPEN_SETPATH_EVENT),
    },
    {
      id: 'file.loadWorkspace',
      title: 'Load Workspace…',
      category: 'File',
      icon: 'load',
      run: () =>
        void webPrompt('Load workspace from file:').then((path) => {
          if (path) void runCommand(`load('${path.replace(/'/g, "''")}')`);
        }),
    },
    {
      id: 'file.saveWorkspace',
      title: 'Save Workspace…',
      category: 'File',
      icon: 'save',
      run: () =>
        void webPrompt('Save workspace to file:', 'workspace.nh5').then((path) => {
          if (path) void runCommand(`save('${path.replace(/'/g, "''")}')`);
        }),
    },
    {
      id: 'edit.clearConsole',
      title: 'Clear Command Window',
      category: 'Edit',
      shortcut: primaryShortcut('L'),
      run: () => void runCommand('clc'),
    },
    {
      id: 'edit.selectAll',
      title: 'Select All',
      category: 'Edit',
      shortcut: primaryShortcut('A'),
      run: () => fire(SELECT_CONSOLE_ALL_EVENT),
    },
    {
      id: 'edit.clearWorkspace',
      title: 'Clear Workspace',
      category: 'Edit',
      run: () => void runCommand('clear'),
    },
    {
      id: 'edit.preferences',
      title: 'Preferences…',
      category: 'Edit',
      run: () => fire(OPEN_PREFS_EVENT),
    },
    {
      id: 'file.exit',
      title: 'Exit',
      category: 'File',
      run: () =>
        void webConfirm('Exit Nelson?').then((confirmed) => {
          if (confirmed) void runCommand('exit');
        }),
    },
  ];
  if (getDesktopCapabilities().nflow) {
    actions.splice(4, 0, {
      id: 'window.nflow',
      title: 'NFlow',
      category: 'Window',
      icon: 'nflow',
      run: () => void runCommand('nflow()'),
    });
  }
  if (getDesktopCapabilities().packageManager) {
    // Window > Package Manager: the nmm_gui singleton WebUI window. The title
    // carries the known update count, so menus show it too.
    actions.push({
      id: 'tools.packageManager',
      title: packageManagerTitle(getDesktopCapabilities().packageUpdates),
      category: 'Window',
      icon: 'package',
      run: () => ensureCorePanel('packages'),
    });
  }
  return actions;
}
//=============================================================================
function themeActions(): DesktopAction[] {
  const selected = getThemePref();
  return (['system', 'light', 'dark'] as ThemePref[]).map((theme) => ({
    id: `view.theme.${theme}`,
    title: theme[0].toUpperCase() + theme.slice(1),
    category: 'Theme',
    checked: selected === theme,
    run: () => requestTheme(theme),
  }));
}
//=============================================================================
function panelActions(): DesktopAction[] {
  const dock = getDockApi();
  const actions: DesktopAction[] = [];
  const panels: [string, string, DesktopIconName, string?][] = [
    ['console', 'Command Window', 'command'],
    ['files', 'Files', 'files', '2'],
    ['workspace', 'Workspace', 'workspace', '3'],
    ['history', 'Command History', 'history', '1'],
    ['variables', 'Variables Editor', 'variables', '4'],
    ['profiler', 'Profiler', 'profiler', '5'],
    ['newsfeed', 'News feed', 'newsfeed', '6'],
    ['diagnostics', 'Diagnostics', 'info'],
    ['terminal', 'Terminal', 'terminal'],
  ];
  for (const [id, title, icon, key] of panels) {
    // Diagnostics describes the browser runtime and the WebAssembly bundle;
    // the native engine only returns a stub, so it has no View entry there.
    if (id === 'diagnostics' && !isWasmRuntime()) continue;
    actions.push({
      id: `view.${id}`,
      title,
      category: 'View',
      icon,
      shortcut: key ? primaryShortcut(key) : undefined,
      checked: !!dock?.getPanel(id),
      run: () => toggleCorePanel(id),
    });
  }
  actions.push({
    id: 'view.documentation',
    title: 'Documentation',
    category: 'View',
    icon: 'help',
    checked: !!dock?.getPanel('help'),
    run: () => toggleCorePanel('help'),
  });
  return actions;
}
//=============================================================================
function shellAndLayoutActions(): DesktopAction[] {
  const shell = getShellVisibility();
  return [
    {
      id: 'view.toolbar',
      title: 'Main toolbar',
      category: 'View',
      checked: shell.toolbar,
      run: () => setShellVisibility('toolbar', !getShellVisibility().toolbar),
    },
    {
      id: 'view.rail',
      title: 'Panels sidebar',
      category: 'View',
      checked: shell.rail,
      run: () => setShellVisibility('rail', !getShellVisibility().rail),
    },
    {
      id: 'view.status',
      title: 'Status bar',
      category: 'View',
      checked: shell.status,
      run: () => setShellVisibility('status', !getShellVisibility().status),
    },
    {
      id: 'layout.default',
      title: 'Two columns (Default)',
      category: 'Layout',
      icon: 'layout',
      run: () => {
        const current = getDockApi();
        if (current) buildDefaultLayout(current);
      },
    },
    {
      id: 'layout.threeColumns',
      title: 'Three columns',
      category: 'Layout',
      run: () => {
        const current = getDockApi();
        if (current) buildThreeColumnsLayout(current);
      },
    },
    {
      id: 'layout.commandOnly',
      title: 'Command Window only',
      category: 'Layout',
      run: () => {
        const current = getDockApi();
        if (current) buildCommandOnlyLayout(current);
      },
    },
    {
      id: 'layout.restore',
      title: 'Restore default layout',
      category: 'Layout',
      run: () => {
        const current = getDockApi();
        if (current) buildDefaultLayout(current);
      },
    },
    {
      id: 'layout.save',
      title: 'Save current layout…',
      category: 'Layout',
      run: () =>
        void webPrompt('Layout name:').then((name) => {
          const current = getDockApi();
          if (current && name) saveNamedLayout(name, current);
        }),
    },
    {
      id: 'window.terminal',
      title: 'Terminal',
      category: 'Window',
      icon: 'terminal',
      run: openShell,
    },
    {
      id: 'window.tile',
      title: 'Tile panels',
      category: 'Window',
      run: tilePanels,
    },
    {
      id: 'help.documentation',
      title: 'Documentation',
      category: 'Help',
      icon: 'help',
      run: () => openHelp(),
    },
    {
      id: 'help.examples',
      title: 'Examples',
      category: 'Help',
      icon: 'examples',
      run: () => ensureCorePanel('examples'),
    },
    {
      id: 'file.newNotebook',
      title: 'New Notebook',
      category: 'File',
      icon: 'examples',
      run: () => openNotebook(),
    },
    {
      id: 'help.checkUpdates',
      title: 'Check for updates',
      category: 'Help',
      run: () => void runCommand('checkupdate'),
    },
    {
      id: 'help.website',
      title: 'Nelson website',
      category: 'Help',
      run: () => window.open('https://nelson-lang.github.io/nelson-website/', '_blank', 'noopener'),
    },
    {
      id: 'help.issues',
      title: 'Bugs and Requests',
      category: 'Help',
      run: () => window.open('https://github.com/nelson-lang/nelson/issues', '_blank', 'noopener'),
    },
    {
      id: 'help.about',
      title: 'About Nelson',
      category: 'Help',
      run: () => fire(OPEN_ABOUT_EVENT),
    },
  ];
}
//=============================================================================
function dynamicActions(): DesktopAction[] {
  const actions: DesktopAction[] = [];
  for (const name of listNamedLayouts()) {
    actions.push({
      id: `layout.named:${name}`,
      title: name,
      category: 'Saved Layout',
      run: () => {
        const current = getDockApi();
        if (current) applyNamedLayout(name, current);
      },
    });
  }
  for (const panel of listOpenPanels()) {
    actions.push({
      id: `window.panel:${panel.id}`,
      title: panel.title,
      category: 'Window',
      run: () => focusPanelById(panel.id),
    });
  }
  for (const path of getRecentFiles()) {
    actions.push({
      id: `recent:${path}`,
      title: `Open ${path.split(/[\\/]/).pop() ?? path}`,
      category: 'Recent File',
      run: () => openEditor(path),
    });
  }
  actions.push({
    id: 'recent.clear',
    title: 'Clear recent files',
    category: 'Recent File',
    enabled: getRecentFiles().length > 0,
    run: clearRecentFiles,
  });
  return actions;
}
//=============================================================================
export function desktopActions(): DesktopAction[] {
  return [
    ...fileAndEditActions(),
    ...panelActions(),
    ...themeActions(),
    ...shellAndLayoutActions(),
    ...dynamicActions(),
  ];
}
//=============================================================================
export function executeDesktopAction(id: string): void {
  const action = desktopActions().find((candidate) => candidate.id === id);
  if (action?.enabled !== false) action?.run();
}
//=============================================================================
