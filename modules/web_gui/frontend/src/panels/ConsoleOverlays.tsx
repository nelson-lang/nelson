//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { RefObject } from 'react';
import { DesktopIcon } from '../components/DesktopIcon';
import type { MenuState } from '../console/consolePanelTypes';
//=============================================================================
type FindState = { term: string; index: number };
export function ConsoleFindBar({
  find,
  findRef,
  matchCount,
  onClose,
  onStep,
  onTermChange,
}: {
  find: FindState;
  findRef: RefObject<HTMLInputElement>;
  matchCount: number;
  onClose: () => void;
  onStep: (delta: number) => void;
  onTermChange: (term: string) => void;
}): JSX.Element {
  return (
    <div className="console-find-bar">
      <input
        ref={findRef}
        className="console-find-bar__input"
        placeholder="Find"
        value={find.term}
        onChange={(e) => onTermChange(e.target.value)}
        onKeyDown={(e) => {
          if (e.key === 'Enter') {
            e.preventDefault();
            onStep(e.shiftKey ? -1 : 1);
          } else if (e.key === 'Escape') {
            onClose();
          }
        }}
      />
      <span className="console-find-bar__count">
        {find.term ? `${matchCount === 0 ? 0 : find.index + 1}/${matchCount}` : ''}
      </span>
      <button onClick={() => onStep(-1)} title="Previous (Shift+Enter)">
        <DesktopIcon name="up" size={16} />
      </button>
      <button onClick={() => onStep(1)} title="Next (Enter)">
        <DesktopIcon name="down" size={16} />
      </button>
      <button onClick={onClose} title="Close (Esc)">
        <DesktopIcon name="close" size={16} />
      </button>
    </div>
  );
}
//=============================================================================
export function ConsoleContextMenu({
  menu,
  onClear,
  onCopy,
  onExportHtml,
  onFind,
  onHelp,
  onPaste,
  onPrint,
  onSelectAll,
  onToggleWrap,
  wrap,
}: {
  menu: MenuState;
  onClear: () => void;
  onCopy: () => void;
  onExportHtml: () => void;
  onFind: () => void;
  onHelp: () => void;
  onPaste: () => void;
  onPrint: () => void;
  onSelectAll: () => void;
  onToggleWrap: () => void;
  wrap: boolean;
}): JSX.Element {
  return (
    <div
      className="console-menu"
      style={{
        left: Math.min(menu.x, window.innerWidth - 170),
        top: Math.min(menu.y, window.innerHeight - 150),
      }}
      onMouseDown={(e) => e.stopPropagation()}
    >
      <div
        className={'console-menu__item' + (menu.hasSelection ? '' : ' disabled')}
        onMouseDown={onHelp}
      >
        <span>Help on Selection</span>
      </div>
      <div
        className={'console-menu__item' + (menu.hasSelection ? '' : ' disabled')}
        onMouseDown={onCopy}
      >
        <span>Copy</span>
        <span>Ctrl+C</span>
      </div>
      <div className="console-menu__item" onMouseDown={onPaste}>
        <span>Paste</span>
        <span>Ctrl+V</span>
      </div>
      <div className="console-menu__sep" />
      <div className="console-menu__item" onMouseDown={onFind}>
        <span>Find…</span>
        <span>Ctrl+F</span>
      </div>
      <div className="console-menu__item" onMouseDown={onToggleWrap}>
        <span>Wrap lines</span>
        {wrap ? <DesktopIcon name="check" size={16} /> : null}
      </div>
      <div className="console-menu__item" onMouseDown={onExportHtml}>
        <span>Export to HTML…</span>
      </div>
      <div className="console-menu__item" onMouseDown={onPrint}>
        <span>Print…</span>
      </div>
      <div className="console-menu__sep" />
      <div className="console-menu__item" onMouseDown={onSelectAll}>
        <span>Select All</span>
        <span>Ctrl+A</span>
      </div>
      <div className="console-menu__sep" />
      <div className="console-menu__item" onMouseDown={onClear}>
        <span>Clear</span>
      </div>
    </div>
  );
}
//=============================================================================
