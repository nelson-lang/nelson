//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useId, useRef, useState } from 'react';
import {
  changeCurrentDirectory,
  refreshCurrentDirectory,
  useCurrentDirectory,
} from '../desktop/currentDirectory';
import { showStatusMessage } from '../desktop/statusMessage';
import { DesktopIcon } from './DesktopIcon';
import { openDesktopPicker } from './DesktopFilePickerHost';
//=============================================================================
interface CurrentDirectoryControlProps {
  busy: boolean;
}
//=============================================================================
export function CurrentDirectoryControl({ busy }: CurrentDirectoryControlProps): JSX.Element {
  const directory = useCurrentDirectory();
  const [draft, setDraft] = useState(directory.cwd);
  const [open, setOpen] = useState(false);
  const [activeIndex, setActiveIndex] = useState(-1);
  const inputRef = useRef<HTMLInputElement>(null);
  const listId = useId();

  useEffect(() => {
    void refreshCurrentDirectory().catch(() => undefined);
  }, []);

  useEffect(() => {
    if (document.activeElement !== inputRef.current) setDraft(directory.cwd);
  }, [directory.cwd]);

  useEffect(() => {
    if (activeIndex >= directory.recentDirectories.length) setActiveIndex(-1);
  }, [activeIndex, directory.recentDirectories.length]);

  const chooseDirectory = (path: string) => {
    const requested = path.trim();
    setOpen(false);
    setActiveIndex(-1);
    if (!requested) {
      setDraft(directory.cwd);
      return;
    }
    void changeCurrentDirectory(requested)
      .then((result) => {
        setDraft(result.cwd);
        showStatusMessage(`Current directory: ${result.cwd}`);
      })
      .catch(() => {
        setDraft(directory.cwd);
        showStatusMessage('Directory does not exist');
      });
  };

  const moveActive = (delta: number) => {
    const count = directory.recentDirectories.length;
    if (count === 0) return;
    setOpen(true);
    setActiveIndex((current) => {
      if (current < 0) return delta > 0 ? 0 : count - 1;
      return (current + delta + count) % count;
    });
  };

  return (
    <div
      className="directory-control"
      onBlur={(event) => {
        if (!event.currentTarget.contains(event.relatedTarget)) {
          setOpen(false);
          setActiveIndex(-1);
        }
      }}
    >
      <button
        type="button"
        className="desktop-toolbar__icon-btn"
        title="Choose current directory"
        aria-label="Choose current directory"
        disabled={busy}
        onClick={() => openDesktopPicker('directory')}
      >
        <DesktopIcon name="folder" />
      </button>
      <input
        ref={inputRef}
        className="directory-control__input"
        role="combobox"
        aria-label="Current directory"
        aria-expanded={open}
        aria-controls={listId}
        aria-autocomplete="list"
        aria-activedescendant={activeIndex >= 0 ? `${listId}-${activeIndex}` : undefined}
        title={directory.cwd}
        value={draft}
        disabled={busy}
        spellCheck={false}
        onChange={(event) => setDraft(event.target.value)}
        onKeyDown={(event) => {
          if (event.key === 'ArrowDown') {
            event.preventDefault();
            moveActive(1);
          } else if (event.key === 'ArrowUp') {
            event.preventDefault();
            moveActive(-1);
          } else if (event.key === 'Enter') {
            event.preventDefault();
            const selected = directory.recentDirectories[activeIndex];
            chooseDirectory(open && selected ? selected : draft);
          } else if (event.key === 'Escape') {
            setOpen(false);
            setActiveIndex(-1);
            setDraft(directory.cwd);
          }
        }}
      />
      <button
        type="button"
        className="directory-control__history"
        title="Recent directories"
        aria-label="Recent directories"
        aria-expanded={open}
        aria-controls={listId}
        disabled={busy || directory.recentDirectories.length === 0}
        onClick={() => {
          setOpen((visible) => !visible);
          setActiveIndex(-1);
          inputRef.current?.focus();
        }}
      >
        ▾
      </button>
      {open ? (
        <div id={listId} className="directory-control__list" role="listbox">
          {directory.recentDirectories.map((path, index) => (
            <button
              id={`${listId}-${index}`}
              key={path}
              type="button"
              role="option"
              aria-selected={index === activeIndex}
              className={index === activeIndex ? 'is-active' : undefined}
              title={path}
              onMouseDown={(event) => event.preventDefault()}
              onClick={() => chooseDirectory(path)}
            >
              {path}
            </button>
          ))}
        </div>
      ) : null}
    </div>
  );
}
//=============================================================================
