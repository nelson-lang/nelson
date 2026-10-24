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
import { desktopActions, type DesktopAction } from '../desktop/actions';
//=============================================================================
export const OPEN_PALETTE_EVENT = 'nelson-webview.open-palette';
export function commandMatchScore(text: string, query: string): number {
  const normalizedText = text.toLocaleLowerCase();
  const normalizedQuery = query.trim().toLocaleLowerCase();
  if (!normalizedQuery) return 1;
  if (normalizedText.startsWith(normalizedQuery)) return 300 - Math.min(normalizedText.length, 200);
  const word = normalizedText.indexOf(` ${normalizedQuery}`);
  if (word >= 0) return 200 - Math.min(word, 100);
  const contained = normalizedText.indexOf(normalizedQuery);
  return contained >= 0 ? 100 - Math.min(contained, 90) : -1;
}
//=============================================================================
function rankedActions(actions: DesktopAction[], query: string): DesktopAction[] {
  return actions
    .map((action) => ({
      action,
      score: commandMatchScore(`${action.title} ${action.category}`, query),
    }))
    .filter((match) => match.score >= 0)
    .sort(
      (left, right) =>
        right.score - left.score || left.action.title.localeCompare(right.action.title)
    )
    .map((match) => match.action);
}
//=============================================================================
export function CommandPalette(): JSX.Element | null {
  const [open, setOpen] = useState(false);
  const [query, setQuery] = useState('');
  const [selected, setSelected] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);
  const paletteRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const onOpen = (event: Event) => {
      const initialQuery = (event as CustomEvent<{ query?: string }>).detail?.query ?? '';
      setOpen(true);
      setQuery(initialQuery);
      setSelected(0);
    };
    window.addEventListener(OPEN_PALETTE_EVENT, onOpen as EventListener);
    return () => window.removeEventListener(OPEN_PALETTE_EVENT, onOpen as EventListener);
  }, []);

  useEffect(() => {
    if (!open) return;
    const focusInput = () => {
      inputRef.current?.focus();
      inputRef.current?.select();
    };
    const keepModalFocus = (event: FocusEvent) => {
      if (!paletteRef.current?.contains(event.target as Node)) focusInput();
    };
    focusInput();
    const frame = requestAnimationFrame(focusInput);
    document.addEventListener('focusin', keepModalFocus);
    return () => {
      cancelAnimationFrame(frame);
      document.removeEventListener('focusin', keepModalFocus);
    };
  }, [open]);

  const actions = useMemo(() => (open ? desktopActions() : []), [open]);
  const filtered = useMemo(() => rankedActions(actions, query), [actions, query]);
  if (!open) return null;

  const runAt = (index: number) => {
    const action = filtered[index];
    if (!action || action.enabled === false) return;
    setOpen(false);
    action.run();
  };

  return (
    <div
      className="dialog-overlay palette-overlay"
      role="presentation"
      onMouseDown={() => setOpen(false)}
    >
      <div
        ref={paletteRef}
        className="palette"
        role="dialog"
        aria-modal="true"
        aria-label="Command palette"
        onMouseDown={(event) => event.stopPropagation()}
      >
        <input
          ref={inputRef}
          className="palette__input"
          aria-label="Search commands"
          placeholder="Search commands"
          value={query}
          onChange={(event) => {
            setQuery(event.target.value);
            setSelected(0);
          }}
          onKeyDown={(event) => {
            if (event.key === 'ArrowDown') {
              event.preventDefault();
              setSelected((value) => Math.min(filtered.length - 1, value + 1));
            } else if (event.key === 'ArrowUp') {
              event.preventDefault();
              setSelected((value) => Math.max(0, value - 1));
            } else if (event.key === 'Enter') {
              event.preventDefault();
              runAt(selected);
            } else if (event.key === 'Escape') {
              event.preventDefault();
              setOpen(false);
            }
          }}
        />
        <ul className="palette__list" role="listbox" aria-label="Command results">
          {filtered.slice(0, 50).map((action, index) => (
            <li
              key={action.id}
              role="option"
              aria-selected={index === selected}
              aria-disabled={action.enabled === false}
              className={
                'palette__item' +
                (index === selected ? ' palette__item--sel' : '') +
                (action.enabled === false ? ' palette__item--disabled' : '')
              }
              onMouseEnter={() => setSelected(index)}
              onMouseDown={(event) => {
                event.preventDefault();
                runAt(index);
              }}
            >
              <span>{action.title}</span>
              <span className="palette__category">{action.category}</span>
              {action.shortcut ? <kbd>{action.shortcut}</kbd> : null}
            </li>
          ))}
          {filtered.length === 0 ? <li className="palette__empty">No matching command</li> : null}
        </ul>
      </div>
    </div>
  );
}
//=============================================================================
