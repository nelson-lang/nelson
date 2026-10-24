//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DesktopAction } from '../desktop/actions';
import { clearRecentFiles } from '../desktop/recentFiles';
import { removeNamedLayout } from '../desktop/layout';
import { DesktopIcon } from './DesktopIcon';
import { useSyncExternalStore } from 'react';
import {
  getDesktopCapabilities,
  packageManagerTitle,
  subscribeDesktopCapabilities,
} from '../desktop/capabilities';
//=============================================================================
interface CommonMenuProps {
  close: () => void;
  run: (id: string) => void;
}
//=============================================================================
interface FileMenuProps extends CommonMenuProps {
  recent: string[];
}
//=============================================================================
export function FileMenu({ close, recent, run }: FileMenuProps): JSX.Element {
  return (
    <>
      <div className="menu__section">New</div>
      <button role="menuitem" onClick={() => run('file.newScript')}>
        New Script
      </button>
      <button role="menuitem" onClick={() => run('file.newNotebook')}>
        New Notebook
      </button>
      <button role="menuitem" onClick={() => run('file.newFunction')}>
        New Function
      </button>
      <button role="menuitem" onClick={() => run('file.newFigure')}>
        New Figure
      </button>
      <hr />
      <button role="menuitem" onClick={() => run('file.execute')}>
        Execute…
      </button>
      <button role="menuitem" onClick={() => run('file.openEditor')}>
        Open in Editor…
      </button>
      <button role="menuitem" onClick={() => run('file.import')}>
        Import Data…
      </button>
      <hr />
      <button role="menuitem" onClick={() => run('file.changeDirectory')}>
        Change current directory…
      </button>
      <button role="menuitem" onClick={() => run('file.displayDirectory')}>
        Display current directory
      </button>
      <button role="menuitem" onClick={() => run('file.setpath')}>
        Set Path…
      </button>
      <hr />
      <button role="menuitem" onClick={() => run('file.loadWorkspace')}>
        Load Workspace…
      </button>
      <button role="menuitem" onClick={() => run('file.saveWorkspace')}>
        Save Workspace…
      </button>
      <RecentFileItems close={close} recent={recent} run={run} />
      <hr />
      <button role="menuitem" onClick={() => run('file.exit')}>
        Exit
      </button>
    </>
  );
}
//=============================================================================
function RecentFileItems({
  close,
  recent,
  run,
}: {
  close: () => void;
  recent: string[];
  run: (id: string) => void;
}): JSX.Element {
  if (!recent.length) return <></>;
  return (
    <>
      <hr />
      <div className="menu__section">Recent Files</div>
      {recent.map((path) => (
        <button
          key={path}
          role="menuitem"
          className="menu__recent"
          title={path}
          onClick={() => {
            run(`recent:${path}`);
          }}
        >
          {path.split(/[\\/]/).pop() ?? path}
        </button>
      ))}
      <button
        role="menuitem"
        className="menu__x"
        onClick={() => {
          clearRecentFiles();
          close();
        }}
      >
        Clear recent files
      </button>
    </>
  );
}
//=============================================================================
export function EditMenu({ run }: Pick<CommonMenuProps, 'run'>): JSX.Element {
  return (
    <>
      <button role="menuitem" onClick={() => run('edit.selectAll')}>
        Select All
      </button>
      <hr />
      <button role="menuitem" onClick={() => run('edit.clearConsole')}>
        Clear Command Window
      </button>
      <button role="menuitem" onClick={() => run('edit.clearWorkspace')}>
        Clear Workspace
      </button>
      <hr />
      <button role="menuitem" onClick={() => run('edit.preferences')}>
        Preferences…
      </button>
    </>
  );
}
//=============================================================================
interface ViewMenuProps extends Pick<CommonMenuProps, 'close' | 'run'> {
  layoutActions: DesktopAction[];
  namedLayouts: string[];
  themeActions: DesktopAction[];
  viewActions: DesktopAction[];
}
//=============================================================================
export function ViewMenu(props: ViewMenuProps): JSX.Element {
  const { close, layoutActions, namedLayouts, run, themeActions, viewActions } = props;
  return (
    <>
      {viewActions.map((action) => (
        <button
          key={action.id}
          role="menuitemcheckbox"
          aria-checked={!!action.checked}
          onClick={() => run(action.id)}
        >
          <span className="menu__check">
            {action.checked ? <DesktopIcon name="check" size={16} /> : null}
          </span>
          <span className="menu__label">{action.title}</span>
          {action.shortcut ? <kbd>{action.shortcut}</kbd> : null}
        </button>
      ))}
      <hr />
      <div className="menu__section">Theme</div>
      {themeActions.map((action) => (
        <button
          key={action.id}
          role="menuitemradio"
          aria-checked={!!action.checked}
          onClick={() => run(action.id)}
        >
          <span className="menu__check">
            {action.checked ? <DesktopIcon name="check" size={16} /> : null}
          </span>
          {action.title}
        </button>
      ))}
      <hr />
      <div className="menu__section">Layout</div>
      {layoutActions.map((action) => (
        <button key={action.id} role="menuitem" onClick={() => run(action.id)}>
          {action.title}
        </button>
      ))}
      {namedLayouts.map((name) => (
        <div key={name} className="menu__row">
          <button role="menuitem" onClick={() => run(`layout.named:${name}`)}>
            {name}
          </button>
          <button
            className="menu__x"
            aria-label={`Remove layout ${name}`}
            onClick={() => {
              removeNamedLayout(name);
              close();
            }}
          >
            <DesktopIcon name="close" size={16} />
          </button>
        </div>
      ))}
    </>
  );
}
//=============================================================================
interface WindowMenuProps extends Pick<CommonMenuProps, 'run'> {
  nflowAvailable: boolean;
  openPanels: { id: string; title: string }[];
}
//=============================================================================
export function WindowMenu({ nflowAvailable, openPanels, run }: WindowMenuProps): JSX.Element {
  const capabilities = useSyncExternalStore(
    subscribeDesktopCapabilities,
    getDesktopCapabilities,
    getDesktopCapabilities
  );
  return (
    <>
      <button role="menuitem" onClick={() => run('window.terminal')}>
        Terminal
      </button>
      <button role="menuitem" onClick={() => run('file.editor')}>
        Text Editor
      </button>
      {nflowAvailable ? (
        <button role="menuitem" onClick={() => run('window.nflow')}>
          NFlow
        </button>
      ) : null}
      {capabilities.packageManager ? (
        <button role="menuitem" onClick={() => run('tools.packageManager')}>
          {packageManagerTitle(capabilities.packageUpdates)}
        </button>
      ) : null}
      <button role="menuitem" onClick={() => run('window.tile')}>
        Tile panels
      </button>
      {openPanels.length ? <hr /> : null}
      {openPanels.map((panel) => (
        <button key={panel.id} role="menuitem" onClick={() => run(`window.panel:${panel.id}`)}>
          {panel.title}
        </button>
      ))}
    </>
  );
}
//=============================================================================
export function HelpMenu({ run }: Pick<CommonMenuProps, 'run'>): JSX.Element {
  return (
    <>
      <button role="menuitem" onClick={() => run('help.documentation')}>
        Documentation
      </button>
      <button role="menuitem" onClick={() => run('help.examples')}>
        Examples
      </button>
      <button role="menuitem" onClick={() => run('help.checkUpdates')}>
        Check for updates
      </button>
      <hr />
      <button role="menuitem" onClick={() => run('help.website')}>
        Nelson website
      </button>
      <button role="menuitem" onClick={() => run('help.issues')}>
        Bugs and Requests
      </button>
      <hr />
      <button role="menuitem" onClick={() => run('help.about')}>
        About Nelson
      </button>
    </>
  );
}
//=============================================================================
