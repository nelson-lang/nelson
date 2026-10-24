//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useRef, useState, useSyncExternalStore } from 'react';
import { executeDesktopAction } from '../desktop/actions';
import { isEngineBusy, subscribeEngine } from '../desktop/engineSession';
import { primaryShortcut } from '../desktop/platform';
import { handleMenuKeyDown } from '../desktop/menuKeyboard';
import { CurrentDirectoryControl } from './CurrentDirectoryControl';
import { DesktopIcon } from './DesktopIcon';
import {
  getDesktopCapabilities,
  packageUpdatesBadge,
  subscribeDesktopCapabilities,
} from '../desktop/capabilities';
//=============================================================================
export function DesktopToolbar(): JSX.Element {
  const busy = useSyncExternalStore(subscribeEngine, isEngineBusy, () => false);
  const capabilities = useSyncExternalStore(
    subscribeDesktopCapabilities,
    getDesktopCapabilities,
    getDesktopCapabilities
  );
  const [moreOpen, setMoreOpen] = useState(false);
  const updatesBadge = packageUpdatesBadge(capabilities.packageUpdates);
  const moreRef = useRef<HTMLButtonElement>(null);

  return (
    <div
      className="desktop-toolbar"
      role="toolbar"
      aria-label="Main toolbar"
      data-focus-zone="toolbar"
    >
      {capabilities.nflow ? (
        <button
          type="button"
          className="desktop-toolbar__action"
          title="NFlow block-diagram editor"
          aria-label="NFlow"
          onClick={() => executeDesktopAction('window.nflow')}
        >
          <DesktopIcon name="nflow" />
          <span className="desktop-toolbar__label">NFlow</span>
        </button>
      ) : null}
      {capabilities.packageManager ? (
        <button
          type="button"
          className="desktop-toolbar__action"
          title={
            updatesBadge
              ? `Install, update and remove Nelson packages (${capabilities.packageUpdates} updates available)`
              : 'Install, update and remove Nelson packages'
          }
          aria-label={
            updatesBadge
              ? `Package Manager, ${capabilities.packageUpdates} updates available`
              : 'Package Manager'
          }
          onClick={() => executeDesktopAction('tools.packageManager')}
        >
          <span className="desktop-badge-host desktop-badge-host--icon">
            <DesktopIcon name="package" />
            {updatesBadge ? <span className="desktop-badge">{updatesBadge}</span> : null}
          </span>
          <span className="desktop-toolbar__label">Packages</span>
        </button>
      ) : null}
      <button
        type="button"
        className="desktop-toolbar__action"
        title="Text Editor"
        aria-label="Text Editor"
        onClick={() => executeDesktopAction('file.editor')}
      >
        <DesktopIcon name="editor" />
        <span className="desktop-toolbar__label">Editor</span>
      </button>
      <button
        type="button"
        className="desktop-toolbar__action"
        title="Execute a Nelson file"
        aria-label="Execute a Nelson file"
        disabled={busy}
        onClick={() => executeDesktopAction('file.execute')}
      >
        <DesktopIcon name="execute" />
        <span className="desktop-toolbar__label">Execute</span>
      </button>
      <button
        type="button"
        className="desktop-toolbar__action"
        title={`Stop execution (${primaryShortcut('C')})`}
        aria-label="Stop the current execution"
        disabled={!busy}
        onClick={() => executeDesktopAction('engine.stop')}
      >
        <DesktopIcon name="stop" />
        <span className="desktop-toolbar__label">Stop</span>
      </button>
      <div className="desktop-toolbar__secondary">
        {(
          [
            ['file.loadWorkspace', 'load' as const, 'Load Workspace'],
            ['file.saveWorkspace', 'save' as const, 'Save Workspace'],
            ['file.import', 'files' as const, 'Import Data'],
          ] as const
        ).map(([id, icon, title]) => (
          <button
            key={id}
            type="button"
            className="desktop-toolbar__action"
            title={title}
            aria-label={title}
            disabled={busy}
            onClick={() => executeDesktopAction(id)}
          >
            <DesktopIcon name={icon} />
            <span className="desktop-toolbar__label">{title}</span>
          </button>
        ))}
      </div>
      <span className="desktop-toolbar__separator" aria-hidden="true" />
      <CurrentDirectoryControl busy={busy} />
      <div className="desktop-toolbar__more">
        <button
          ref={moreRef}
          type="button"
          className="desktop-toolbar__icon-btn"
          title="More actions"
          aria-label="More actions"
          aria-expanded={moreOpen}
          onClick={() => setMoreOpen((open) => !open)}
        >
          <DesktopIcon name="more" />
        </button>
        {moreOpen ? (
          <div
            className="desktop-toolbar__menu"
            role="menu"
            onKeyDown={(event) =>
              handleMenuKeyDown(event, () => {
                setMoreOpen(false);
                setTimeout(() => moreRef.current?.focus());
              })
            }
          >
            {(
              [
                ['file.loadWorkspace', 'load' as const, 'Load Workspace…'],
                ['file.saveWorkspace', 'save' as const, 'Save Workspace…'],
                ['file.import', 'files' as const, 'Import Data…'],
              ] as const
            ).map(([id, icon, title]) => (
              <button
                key={id}
                type="button"
                role="menuitem"
                onClick={() => {
                  executeDesktopAction(id);
                  setMoreOpen(false);
                }}
              >
                <DesktopIcon name={icon} size={16} />
                {title}
              </button>
            ))}
          </div>
        ) : null}
      </div>
    </div>
  );
}
//=============================================================================
