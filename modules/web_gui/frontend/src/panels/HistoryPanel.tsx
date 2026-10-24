//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useMemo, useRef, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import type { HistoryListResult } from '../rpc/types';
import { ContextMenu, useContextMenu, type MenuItem } from '../components/ContextMenu';
import { sendToConsoleInput } from '../desktop/consoleBus';
import { runCommand } from '../desktop/engineSession';
import { openEditor } from '../desktop/desktop';
//=============================================================================
export function HistoryPanel(): JSX.Element {
  const [entries, setEntries] = useState<string[]>([]);
  const [filter, setFilter] = useState('');
  const { menu, openMenu, closeMenu } = useContextMenu();
  const listRef = useRef<HTMLUListElement>(null);

  useEffect(() => {
    const refresh = () =>
      rpc<HistoryListResult>('history.list')
        .then((r) => setEntries(r.entries))
        .catch(() => undefined);
    refresh();
    return onPush('history.changed', refresh);
  }, []);

  // New commands land at the bottom: keep the end in view on every refresh,
  // unless the user scrolled up to read older entries.
  const wasNearBottom = useRef(true);
  useEffect(() => {
    const el = listRef.current;
    if (el && wasNearBottom.current) {
      el.scrollTop = el.scrollHeight;
    }
  }, [entries]);

  const shown = useMemo(
    () => entries.map((e, i) => ({ e, i })).filter(({ e }) => e.includes(filter)),
    [entries, filter]
  );

  const rerun = (line: string) => void runCommand(line);
  const clearAll = () => rpc('history.clear', {}).catch(() => undefined);
  const removeAt = (index: number) => rpc('history.remove', { index }).catch(() => undefined);

  const isMarker = (e: string) => e.startsWith('%%');

  const createScript = async (line: string) => {
    const cwd = await rpc<{ cwd: string }>('fs.cwd').catch(() => ({ cwd: '.' }));
    const path = `${cwd.cwd || '.'}/from_history.m`;
    await rpc('fs.create', {
      path,
      kind: 'file',
      content: '%% Script from history\n' + line + '\n',
    }).catch(() => undefined);
    openEditor(path);
  };

  const itemsFor = (e: string, index: number): MenuItem[] => {
    const marker = isMarker(e);
    return [
      { label: 'Evaluate', onClick: () => rerun(e), disabled: marker },
      { label: 'Copy to Command Window', onClick: () => sendToConsoleInput(e), disabled: marker },
      { label: 'Create Script', onClick: () => void createScript(e), disabled: marker },
      {
        label: 'Copy',
        onClick: () => void navigator.clipboard?.writeText(e).catch(() => undefined),
      },
      { separator: true },
      { label: 'Delete', onClick: () => void removeAt(index), disabled: marker },
      { label: 'Clear history', onClick: clearAll, danger: true },
    ];
  };

  return (
    <div className="browser">
      <input
        className="browser__search"
        placeholder="Search history…"
        value={filter}
        onChange={(e) => setFilter(e.target.value)}
      />
      <ul
        className="history-list"
        ref={listRef}
        onScroll={(ev) => {
          const el = ev.currentTarget;
          wasNearBottom.current = el.scrollHeight - el.scrollTop - el.clientHeight < 40;
        }}
      >
        {shown.map(({ e, i }) => (
          <li
            key={i}
            className={isMarker(e) ? 'history-list__marker' : undefined}
            onDoubleClick={() => !isMarker(e) && rerun(e)}
            onContextMenu={(ev) => openMenu(ev, itemsFor(e, i))}
            title={isMarker(e) ? undefined : 'Double-click to run'}
          >
            {e}
          </li>
        ))}
      </ul>
      <ContextMenu menu={menu} onClose={closeMenu} />
    </div>
  );
}
//=============================================================================
