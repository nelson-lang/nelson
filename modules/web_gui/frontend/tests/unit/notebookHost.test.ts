//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The Notebook window of the Qt desktop reuses the web desktop's notebook.open
// push and panels: these tests pin the shared pieces that host relies on.
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
//=============================================================================
const openNotebook = vi.fn();
vi.mock('../../src/desktop/desktop', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../../src/desktop/desktop')>()),
  openNotebook: (...args: unknown[]) => openNotebook(...args),
}));
//=============================================================================
import { installNotebookOpenPush } from '../../src/desktop/figureLifecycle';
import { getClientRole, setClientRole } from '../../src/desktop/capabilities';
//=============================================================================
describe('notebook.open push (web desktop and Qt Notebook window)', () => {
  let off: () => void = () => undefined;
  beforeEach(() => {
    openNotebook.mockClear();
    off = installNotebookOpenPush();
  });
  afterEach(() => off());

  it('opens one tab per document path', () => {
    window.__nelsonPush?.('notebook.open', JSON.stringify({ path: 'C:/work/demo.nlx' }));
    expect(openNotebook).toHaveBeenCalledWith('C:/work/demo.nlx');
  });

  it('opens a new untitled document when the path is empty', () => {
    window.__nelsonPush?.('notebook.open', JSON.stringify({ path: '' }));
    expect(openNotebook).toHaveBeenCalledWith(undefined);
    window.__nelsonPush?.('notebook.open', JSON.stringify({}));
    expect(openNotebook).toHaveBeenCalledTimes(2);
  });
});
//=============================================================================
describe('client role', () => {
  afterEach(() => setClientRole('desktop'));

  it('defaults to the desktop and accepts the notebook window', () => {
    expect(getClientRole()).toBe('desktop');
    setClientRole('notebook');
    expect(getClientRole()).toBe('notebook');
    setClientRole(undefined);
    expect(getClientRole()).toBe('notebook');
  });

  it('is learned from client.role and selects the notebook shell', () => {
    const app = readFileSync(resolve(process.cwd(), 'src/App.tsx'), 'utf8');
    expect(app).toContain('setClientRole(r.role)');
    expect(app).toContain("standaloneFigure === 'notebook'");
    expect(app).toContain('onReady={onNotebookReady}');
  });

  it('keeps inline figures inside the Qt Notebook window', () => {
    const cell = readFileSync(resolve(process.cwd(), 'src/notebook/blocks/CodeCell.tsx'), 'utf8');
    expect(cell).toContain("getClientRole() === 'notebook' ? undefined : () => popOutFigure(i)");
  });

  it('gives the Notebook window a menu bar and the About dialog', () => {
    const app = readFileSync(resolve(process.cwd(), 'src/App.tsx'), 'utf8');
    const shell = app.slice(app.indexOf("standaloneFigure === 'notebook'"));
    expect(shell.indexOf('<NotebookMenuBar />')).toBeGreaterThan(0);
    expect(shell.indexOf('<AboutHost />')).toBeGreaterThan(0);
  });
});
describe('menus close like native ones', () => {
  it('the desktop menu bar and the Notebook toolbar menus share the dismiss hook', () => {
    const menuBar = readFileSync(resolve(process.cwd(), 'src/components/MenuBar.tsx'), 'utf8');
    expect(menuBar).toContain("useMenuDismiss(!!open && !nativeMenu, close, '.menubar')");
    const panel = readFileSync(resolve(process.cwd(), 'src/notebook/NotebookPanel.tsx'), 'utf8');
    expect(panel).toContain("useMenuDismiss(openMenu !== null, closeMenu, '.nls-menu[open]')");
    const notebookBar = readFileSync(
      resolve(process.cwd(), 'src/components/NotebookMenuBar.tsx'),
      'utf8'
    );
    expect(notebookBar).toContain("useMenuDismiss(open !== null, close, '.menubar')");
  });
});
describe('Notebook save goes through the host', () => {
  it('the panel saves with the shared helper, the picker host offers a save mode', () => {
    const panel = readFileSync(resolve(process.cwd(), 'src/notebook/NotebookPanel.tsx'), 'utf8');
    expect(panel).toContain('saveNotebookDocument(');
    expect(panel).toContain('pickPath: requestSavePath');
    expect(panel).toContain('browserOnly: isWasmRuntime');
    const host = readFileSync(
      resolve(process.cwd(), 'src/components/DesktopFilePickerHost.tsx'),
      'utf8'
    );
    expect(host).toContain('export async function requestPath(');
    expect(host).toContain("mode: 'save'");
    // the web editor's Save As shares the same picker
    const editor = readFileSync(resolve(process.cwd(), 'src/panels/EditorPanel.tsx'), 'utf8');
    expect(editor).toContain('requestSavePath(');
    const menuBar = readFileSync(
      resolve(process.cwd(), 'src/components/NotebookMenuBar.tsx'),
      'utf8'
    );
    expect(menuBar).toContain("command('save')");
    expect(menuBar).toContain("command('saveAs')");
  });
});
describe('web desktop Notebook shortcuts match the Qt desktop', () => {
  it('toolbar button, side rail entry and action share the notebook icon', () => {
    const toolbar = readFileSync(
      resolve(process.cwd(), 'src/components/DesktopToolbar.tsx'),
      'utf8'
    );
    expect(toolbar).toContain("executeDesktopAction('file.newNotebook')");
    expect(toolbar).toContain('<DesktopIcon name="notebook" />');
    const sidebar = readFileSync(resolve(process.cwd(), 'src/components/Sidebar.tsx'), 'utf8');
    expect(sidebar).toContain("notebook: 'file.newNotebook'");
    const icons = readFileSync(resolve(process.cwd(), 'src/components/DesktopIcon.tsx'), 'utf8');
    expect(icons).toContain('resources/action-notebook.svg');
    const actions = readFileSync(resolve(process.cwd(), 'src/desktop/actions.ts'), 'utf8');
    expect(actions.slice(actions.indexOf("id: 'file.newNotebook'"))).toMatch(
      /^[^}]*icon: 'notebook'/
    );
  });
});
describe('Notebook window menu commands', () => {
  it('reach the active document only', async () => {
    const { onNotebookCommand, sendNotebookCommand } = await import('../../src/notebook/commands');
    const active = vi.fn();
    const inactive = vi.fn();
    const offActive = onNotebookCommand(() => true, active);
    const offInactive = onNotebookCommand(() => false, inactive);
    sendNotebookCommand('runAll');
    sendNotebookCommand('saveAs');
    expect(active.mock.calls.map((c) => c[0])).toEqual(['runAll', 'saveAs']);
    expect(inactive).not.toHaveBeenCalled();
    offActive();
    sendNotebookCommand('open');
    expect(active).toHaveBeenCalledTimes(2);
    offInactive();
  });
});
//=============================================================================
