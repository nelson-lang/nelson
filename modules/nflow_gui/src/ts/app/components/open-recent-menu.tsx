//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import { t } from '../../utils/i18n.js';
//=============================================================================
const roots = new WeakMap<HTMLElement, Root>();
function displayNameForPath(path: string): string {
  try {
    const value = String(path || '').trim();
    if (!value) return value;
    const index = Math.max(value.lastIndexOf('/'), value.lastIndexOf('\\'));
    if (index >= 0 && index < value.length - 1) return value.slice(index + 1);
    return value;
  } catch {
    return String(path);
  }
}
//=============================================================================
function OpenRecentMenu({
  recents,
  onAction,
}: {
  recents: string[];
  onAction: (action: string) => void;
}): JSX.Element {
  const activate = (action: string) => {
    onAction(action);
  };

  return (
    <>
      {recents.length === 0 ? (
        <div className="menubar-item">
          <span>{t('No recently opened files.')}</span>
        </div>
      ) : (
        recents.map((path) => {
          const display = displayNameForPath(path);
          const action = `openRecent:${encodeURIComponent(String(path))}`;
          return (
            <div
              className="menubar-item"
              data-action={action}
              key={path}
              onClick={(event) => {
                event.stopPropagation();
                activate(action);
              }}
            >
              <span title={display !== path ? path : undefined}>{display}</span>
            </div>
          );
        })
      )}
      <div className="menubar-divider" />
      <div
        className="menubar-item"
        data-action="clearRecent"
        onClick={(event) => {
          event.stopPropagation();
          activate('clearRecent');
        }}
      >
        <span>{t('Clear Recently Opened files...')}</span>
      </div>
    </>
  );
}
//=============================================================================
export function renderOpenRecentMenu(
  container: HTMLElement,
  recents: string[],
  onAction: (action: string) => void
): void {
  let root = roots.get(container);
  if (!root) {
    if (container.childNodes.length) container.replaceChildren();
    root = createRoot(container);
    roots.set(container, root);
  }
  flushSync(() => {
    root?.render(<OpenRecentMenu recents={recents} onAction={onAction} />);
  });
}
//=============================================================================
export default renderOpenRecentMenu;
//=============================================================================
