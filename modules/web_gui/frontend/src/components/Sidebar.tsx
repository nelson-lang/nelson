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
import { DesktopIcon, type DesktopIconName } from './DesktopIcon';
import { getDockApi, listOpenPanels, panelState, subscribeDock } from '../desktop/desktop';
import { isWasmRuntime, primaryShortcut } from '../desktop/platform';
import { executeDesktopAction } from '../desktop/actions';
import {
  getDesktopCapabilities,
  packageUpdatesBadge,
  subscribeDesktopCapabilities,
} from '../desktop/capabilities';
import { handleMenuKeyDown } from '../desktop/menuKeyboard';
//=============================================================================
interface RailButton {
  id: string;
  icon: DesktopIconName;
  label: string;
  key?: string;
}
//=============================================================================
const PANELS: RailButton[] = [
  { id: 'console', icon: 'command', label: 'Command Window' },
  { id: 'files', icon: 'files', label: 'Files', key: '2' },
  { id: 'workspace', icon: 'workspace', label: 'Workspace', key: '3' },
  { id: 'history', icon: 'history', label: 'Command History', key: '1' },
  { id: 'variables', icon: 'variables', label: 'Variables Editor', key: '4' },
  { id: 'profiler', icon: 'profiler', label: 'Profiler', key: '5' },
  { id: 'newsfeed', icon: 'newsfeed', label: 'News feed', key: '6' },
  { id: 'diagnostics', icon: 'info', label: 'Diagnostics' },
  { id: 'terminal', icon: 'terminal', label: 'Terminal' },
  { id: 'examples', icon: 'examples', label: 'Examples' },
  { id: 'packageManager', icon: 'package', label: 'Package Manager' },
];
//=============================================================================
// Rail entries that run a desktop action instead of toggling a panel.
const COMMAND_BUTTONS: Record<string, string> = {
  examples: 'help.examples',
  packageManager: 'tools.packageManager',
};
//=============================================================================
// The Diagnostics panel reports the browser runtime and the WebAssembly
// bundle; the native engine only answers with a stub, so hide it there.
function railPanels(): RailButton[] {
  return isWasmRuntime() ? PANELS : PANELS.filter((button) => button.id !== 'diagnostics');
}
//=============================================================================
function dockSnapshot(): string {
  return `${getDockApi()?.activePanel?.id ?? ''}|${listOpenPanels()
    .map((panel) => panel.id)
    .join(',')}`;
}
//=============================================================================
function RailPanelButton({ button }: { button: RailButton }): JSX.Element | null {
  const capabilities = useSyncExternalStore(subscribeDesktopCapabilities, getDesktopCapabilities);
  const command = COMMAND_BUTTONS[button.id];
  if (command) {
    if (button.id === 'packageManager' && !capabilities.packageManager) {
      return null;
    }
    const badge =
      button.id === 'packageManager' ? packageUpdatesBadge(capabilities.packageUpdates) : '';
    const label = badge
      ? `${button.label} (${capabilities.packageUpdates} updates available)`
      : button.label;
    return (
      <button
        type="button"
        className="sidebar__btn desktop-badge-host"
        aria-label={label}
        title={label}
        onClick={() => executeDesktopAction(command)}
      >
        <DesktopIcon name={button.icon} />
        {badge ? <span className="desktop-badge">{badge}</span> : null}
      </button>
    );
  }
  const state = panelState(button.id);
  const shortcut = button.key ? primaryShortcut(button.key) : '';
  const tooltip = shortcut ? `${button.label} (${shortcut})` : button.label;
  return (
    <button
      type="button"
      className="sidebar__btn"
      data-visible={state.visible || undefined}
      data-active={state.active || undefined}
      aria-pressed={state.visible}
      aria-current={state.active ? 'page' : undefined}
      aria-label={tooltip}
      title={tooltip}
      onClick={() => executeDesktopAction(`view.${button.id}`)}
    >
      <DesktopIcon name={button.icon} />
    </button>
  );
}
//=============================================================================
export function Sidebar(): JSX.Element {
  useSyncExternalStore(subscribeDock, dockSnapshot, () => '');
  const [moreOpen, setMoreOpen] = useState(false);
  const moreRef = useRef<HTMLButtonElement>(null);
  const panels = railPanels();

  return (
    <nav className="sidebar" aria-label="Panels sidebar" data-focus-zone="rail">
      {panels.map((button, index) => (
        <div
          key={button.id}
          className={index === 1 || button.id === 'terminal' ? 'sidebar__section' : undefined}
        >
          <RailPanelButton button={button} />
        </div>
      ))}
      <div className="sidebar__more">
        <button
          ref={moreRef}
          type="button"
          className="sidebar__btn sidebar__more-btn"
          aria-label="More panels"
          title="More panels"
          aria-expanded={moreOpen}
          onClick={() => setMoreOpen((open) => !open)}
        >
          <DesktopIcon name="more" />
        </button>
        {moreOpen ? (
          <div
            className="sidebar__menu"
            role="menu"
            onKeyDown={(event) =>
              handleMenuKeyDown(event, () => {
                setMoreOpen(false);
                setTimeout(() => moreRef.current?.focus());
              })
            }
          >
            {[
              ...panels.filter((button) => !COMMAND_BUTTONS[button.id]),
              { id: 'help', icon: 'help' as const, label: 'Documentation' },
            ].map((button) => {
              const state = panelState(button.id);
              return (
                <button
                  key={button.id}
                  type="button"
                  role="menuitemcheckbox"
                  aria-checked={state.visible}
                  onClick={() => {
                    executeDesktopAction(
                      button.id === 'help' ? 'view.documentation' : `view.${button.id}`
                    );
                    setMoreOpen(false);
                  }}
                >
                  <DesktopIcon name={button.icon} size={16} />
                  <span>{button.label}</span>
                  {state.visible ? <DesktopIcon name="check" size={16} /> : null}
                </button>
              );
            })}
          </div>
        ) : null}
      </div>
    </nav>
  );
}
//=============================================================================
