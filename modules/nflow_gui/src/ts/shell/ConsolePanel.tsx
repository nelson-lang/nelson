//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// React console panel: live log of the application status messages pushed
// through the central status-service.
//=============================================================================
import {
  useCallback,
  useEffect,
  useRef,
  useState,
  type KeyboardEvent as ReactKeyboardEvent,
  type MouseEvent as ReactMouseEvent,
} from 'react';
import { subscribeStatus, type StatusItem } from '../app/status-service.js';
import { t } from '../utils/i18n.js';
import { emitAppEvent } from '../app/app-events.js';
import {
  PanelContextMenu,
  clampMenuPosition,
  copyText,
  type PanelContextMenuPosition,
} from './PanelContextMenu.js';
//=============================================================================
const MAX_ENTRIES = 200;
type ConsoleContextMenu = PanelContextMenuPosition & { text: string };
export function ConsolePanel(): JSX.Element {
  const [entries, setEntries] = useState<StatusItem[]>([]);
  const [contextMenu, setContextMenu] = useState<ConsoleContextMenu | null>(null);
  const panelRef = useRef<HTMLDivElement | null>(null);
  const bottomRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    const unsubscribe = subscribeStatus((item) => {
      if (!item) return;
      setEntries((prev) => {
        const last = prev.length > 0 ? prev[prev.length - 1] : null;
        // Progress updates reuse the same id, and legacy call sites may
        // repeat the same message in a loop - replace instead of appending.
        if (
          last &&
          (last.id === item.id || (last.message === item.message && last.level === item.level))
        ) {
          return [...prev.slice(0, -1), item];
        }
        const next = [...prev, item];
        return next.length > MAX_ENTRIES ? next.slice(next.length - MAX_ENTRIES) : next;
      });
    });
    return unsubscribe;
  }, []);

  useEffect(() => {
    // scrollIntoView is absent in jsdom - guard the method itself.
    bottomRef.current?.scrollIntoView?.({ block: 'end' });
  }, [entries]);

  const closeContextMenu = useCallback(() => setContextMenu(null), []);

  const openContextMenu = (event: ReactMouseEvent<HTMLDivElement>) => {
    const panel = panelRef.current;
    const selection = window.getSelection();
    const text = selection?.toString() ?? '';
    if (
      !panel ||
      !selection?.anchorNode ||
      !selection.focusNode ||
      !text.trim() ||
      !panel.contains(selection.anchorNode) ||
      !panel.contains(selection.focusNode)
    ) {
      return;
    }

    event.preventDefault();
    event.stopPropagation();
    setContextMenu({ ...clampMenuPosition(event.clientX, event.clientY, 1), text });
  };

  const selectConsoleContents = (event: ReactKeyboardEvent<HTMLDivElement>) => {
    if (!(event.ctrlKey || event.metaKey) || event.key.toLowerCase() !== 'a') return;
    const panel = panelRef.current;
    const selection = window.getSelection();
    if (!panel || !selection) return;

    const range = document.createRange();
    range.selectNodeContents(panel);
    selection.removeAllRanges();
    selection.addRange(range);
    event.preventDefault();
  };

  return (
    <div
      ref={panelRef}
      className="nf-console-panel"
      data-testid="console-panel"
      tabIndex={0}
      aria-label={t('Console')}
      onKeyDown={selectConsoleContents}
      onContextMenu={openContextMenu}
    >
      {entries.length === 0 ? (
        <div className="nf-console-empty">{t('No messages yet')}</div>
      ) : (
        entries.map((e) => (
          <div
            key={e.id + String(e.createdAt)}
            className={`nf-console-entry nf-console-${e.level}${
              e.blockId ? ' nf-console-link' : ''
            }`}
            title={e.blockId ? `${e.blockId}` : undefined}
            onClick={
              e.blockId
                ? () => emitAppEvent('nf-select-block', { blockId: String(e.blockId) })
                : undefined
            }
          >
            <span className="nf-console-time">{new Date(e.createdAt).toLocaleTimeString()}</span>
            <span className="nf-console-message">{e.message}</span>
            {e.level === 'progress' && e.progress != null ? (
              <span className="nf-console-pct">{Math.round(e.progress)}%</span>
            ) : null}
          </div>
        ))
      )}
      <div ref={bottomRef} />
      {contextMenu ? (
        <PanelContextMenu
          position={contextMenu}
          onClose={closeContextMenu}
          items={[{ label: t('Copy'), onSelect: () => void copyText(contextMenu.text) }]}
        />
      ) : null}
    </div>
  );
}
//=============================================================================
export default ConsolePanel;
//=============================================================================
