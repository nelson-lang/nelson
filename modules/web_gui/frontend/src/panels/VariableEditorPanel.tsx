//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useRef, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import { webPrompt } from '../components/PromptHost';
import { runCommand } from '../desktop/engineSession';
import type { CellValue, VarBlockResult, VarOpenResult } from '../rpc/types';
import { DesktopIcon } from '../components/DesktopIcon';
//=============================================================================
// Variable editor: opens a numeric, char or string variable as an editable
// grid. Every edit (single cell, block paste, insert/delete row-col,
// undo/redo) is computed on the grid here and pushed back through var.replace,
// which rebuilds the whole variable in the interpreter; one path for all.
type Cell = number | string;
type Grid = Cell[][];
type Sel = { r: number; c: number };
const toNum = (v: CellValue): number => {
  const n = Number(v);
  return Number.isFinite(n) ? n : 0;
};
//=============================================================================
const deep = (g: Grid): Grid => g.map((row) => row.slice());
const blanks = (n: number, text: boolean): Cell[] =>
  Array.from({ length: n }, () => (text ? '' : 0));
type PanelApi = { setTitle?: (title: string) => void };
export function VariableEditorPanel({
  name,
  panelApi,
}: {
  name: string;
  panelApi?: PanelApi;
}): JSX.Element {
  const [meta, setMeta] = useState<VarOpenResult | null>(null);
  const [grid, setGrid] = useState<Grid>([]);
  const [error, setError] = useState<string | null>(null);
  const [sel, setSel] = useState<Sel | null>(null);
  const [editing, setEditing] = useState<Sel | null>(null);
  const [draft, setDraft] = useState('');
  const [ctxMenu, setCtxMenu] = useState<{ x: number; y: number; r: number; c: number } | null>(
    null
  );

  const undoRef = useRef<Grid[]>([]);
  const redoRef = useRef<Grid[]>([]);
  // Count of writes started by this panel; used to ignore the var.changed echo they cause
  // so a reload does not clobber the grid mid-edit.
  const selfWrites = useRef(0);

  const load = useCallback(async () => {
    try {
      const m = await rpc<VarOpenResult>('var.open', { name });
      setMeta(m);
      // Tab title: "name [class rows×cols] (read-only)".
      panelApi?.setTitle?.(
        `${name} [${m.className} ${m.rows}×${m.cols}]${m.editable ? '' : ' (read-only)'}`
      );
      if (m.kind === 'text') {
        // Full display() dump, rendered as a read-only text block (no grid).
        setGrid([]);
        setError(null);
        return;
      }
      const text = m.kind !== 'matrix';
      const block = await rpc<VarBlockResult>('var.getBlock', {
        name,
        row0: 0,
        col0: 0,
        rows: m.rows,
        cols: m.cols,
      });
      setGrid(block.cells.map((row) => row.map((c) => (text ? String(c ?? '') : toNum(c)))));
      setError(null);
    } catch (e) {
      setError((e as Error).message);
    }
  }, [name, panelApi]);

  // Mirror `editing` into a ref so the push handlers below see it without
  // re-subscribing on every keystroke.
  const editingRef = useRef<Sel | null>(null);
  editingRef.current = editing;

  useEffect(() => {
    load();
    const offVar = onPush('var.changed', (p) => {
      if ((p as { name?: string }).name !== name) return;
      if (selfWrites.current > 0) {
        selfWrites.current -= 1;
        return; // own write; the grid already reflects it
      }
      load();
    });
    // A console reassignment (`a = ...`, `clear a`) bypasses the write RPCs and
    // only surfaces as workspace.changed. Reload to pick up the new value/type,
    // unless mid-edit or it is the echo of its own pending write.
    const offWs = onPush('workspace.changed', () => {
      if (editingRef.current || selfWrites.current > 0) return;
      load();
    });
    return () => {
      offVar();
      offWs();
    };
  }, [load, name]);

  const persist = useCallback(
    async (next: Grid) => {
      selfWrites.current += 1;
      try {
        await rpc('var.replace', { name, values: next });
      } catch (e) {
        selfWrites.current = Math.max(0, selfWrites.current - 1);
        setError((e as Error).message);
      }
    },
    [name]
  );

  const apply = useCallback(
    (next: Grid) => {
      undoRef.current.push(deep(grid));
      redoRef.current = [];
      setGrid(next);
      void persist(next);
    },
    [grid, persist]
  );

  const undo = useCallback(() => {
    const prev = undoRef.current.pop();
    if (!prev) return;
    redoRef.current.push(deep(grid));
    setGrid(prev);
    void persist(prev);
  }, [grid, persist]);

  const redo = useCallback(() => {
    const next = redoRef.current.pop();
    if (!next) return;
    undoRef.current.push(deep(grid));
    setGrid(next);
    void persist(next);
  }, [grid, persist]);

  // For the grid, rows/cols come from the loaded block; for the text view the
  // grid is empty, so fall back to the reported dimensions.
  const rows = meta?.kind === 'text' ? meta.rows : grid.length;
  const cols = meta?.kind === 'text' ? meta.cols : grid[0]?.length ?? meta?.cols ?? 0;
  const isText = meta?.kind !== 'matrix';
  const canEdit = meta?.editable ?? false;
  const blank: Cell = isText ? '' : 0;
  // Struct/cell grids are read-only at the variable level, but individual scalar
  // elements can be edited in place through var.setNested.
  const nestedKind: 'struct' | 'cell' | null =
    meta?.kind === 'struct' ? 'struct' : meta?.kind === 'cell' ? 'cell' : null;
  // A cell summary is a scalar (thus editable) unless it's a dimension summary
  // ("3x3 double") or empty ("[]").
  const isScalarSummary = (s: string): boolean => s !== '' && s !== '[]' && !/^\d+x\d+ \S/.test(s);
  // Strip the display decoration so the raw value is edited (' ' for char, " " for string).
  const stripDecoration = (s: string): string => {
    const m = /^'(.*)'$/.exec(s) ?? /^"(.*)"$/.exec(s);
    return m ? m[1] : s;
  };
  // logical grids show true/false but stay numeric 0/1 internally.
  const isLogical = meta?.className === 'logical';
  const display = (val: Cell): string =>
    isLogical && !isText ? (val ? 'true' : 'false') : String(val ?? '');
  // A <missing> string shows the token in italic.
  const renderCell = (val: Cell): JSX.Element | string =>
    isText && val === '<missing>' ? (
      <span className="var-editor__missing">&lt;missing&gt;</span>
    ) : (
      display(val)
    );
  // Parse an edited logical cell: true/1/yes → 1, everything else → 0.
  const parseLogical = (s: string): number => (/^(true|1|yes|on)$/i.test(s.trim()) ? 1 : 0);

  const commitNested = async (r: number, c: number, text: string) => {
    if (!nestedKind) return;
    try {
      await rpc('var.setNested', { name, kind: nestedKind, row: r, col: c, value: text });
      await load();
    } catch (e) {
      setError((e as Error).message);
    }
  };

  const commit = () => {
    if (!editing) return;
    const { r, c } = editing;
    setEditing(null);
    if (nestedKind) {
      void commitNested(r, c, draft);
      return;
    }
    let value: Cell;
    if (isText) {
      value = draft;
    } else if (isLogical) {
      value = parseLogical(draft);
    } else {
      const num = Number(draft);
      if (Number.isNaN(num)) return;
      value = num;
    }
    if (grid[r]?.[c] === value) return;
    const next = deep(grid);
    next[r][c] = value;
    apply(next);
  };

  const insertRow = (at: number) => {
    const next = deep(grid);
    next.splice(at, 0, blanks(cols, isText));
    apply(next);
  };
  const deleteRow = (at: number) => {
    if (rows <= 1) return;
    const next = deep(grid);
    next.splice(at, 1);
    apply(next);
  };
  const insertCol = (at: number) => {
    const next = grid.map((row) => {
      const r = row.slice();
      r.splice(at, 0, blank);
      return r;
    });
    apply(next);
  };
  const deleteCol = (at: number) => {
    if (cols <= 1) return;
    const next = grid.map((row) => {
      const r = row.slice();
      r.splice(at, 1);
      return r;
    });
    apply(next);
  };

  // Parse tab- (Excel) or comma-separated text into a matrix; numeric grids
  // coerce to numbers, text grids keep the raw strings.
  const parseClipboard = (text: string): Grid => {
    const lines = text.replace(/\r/g, '').replace(/\n+$/, '').split('\n');
    return lines.map((line) => {
      const parts = line.includes('\t') ? line.split('\t') : line.split(',');
      return parts.map((p) => {
        if (isText) return p;
        const n = Number(p.trim());
        return Number.isFinite(n) ? n : 0;
      });
    });
  };

  const pasteAt = (anchor: Sel, matrix: Grid) => {
    if (!matrix.length) return;
    const needR = anchor.r + matrix.length;
    const needC = anchor.c + Math.max(...matrix.map((m) => m.length));
    const next = deep(grid);
    while (next.length < needR) next.push(blanks(cols, isText));
    for (const row of next) while (row.length < needC) row.push(blank);
    for (let i = 0; i < matrix.length; i++) {
      for (let j = 0; j < matrix[i].length; j++) {
        next[anchor.r + i][anchor.c + j] = matrix[i][j];
      }
    }
    apply(next);
  };

  const onGridKeyDown = async (e: React.KeyboardEvent) => {
    if (editing) return;
    // Ctrl+C works on any variable; all other shortcuts mutate, so they need an
    // editable variable (struct/cell/table are read-only).
    if (e.ctrlKey && e.key.toLowerCase() === 'c' && sel) {
      e.preventDefault();
      try {
        await navigator.clipboard.writeText(display(grid[sel.r]?.[sel.c] ?? ''));
      } catch {
        /* ignore */
      }
      return;
    }
    if (!canEdit) return;
    if (e.ctrlKey && e.key.toLowerCase() === 'z') {
      e.preventDefault();
      undo();
      return;
    }
    if (e.ctrlKey && (e.key.toLowerCase() === 'y' || (e.shiftKey && e.key.toLowerCase() === 'z'))) {
      e.preventDefault();
      redo();
      return;
    }
    if (e.ctrlKey && e.key.toLowerCase() === 'v') {
      e.preventDefault();
      if (!sel) return;
      try {
        const text = await navigator.clipboard.readText();
        pasteAt(sel, parseClipboard(text));
      } catch {
        /* clipboard blocked; ignore */
      }
      return;
    }
    if (sel && (e.key === 'Delete' || e.key === 'Backspace')) {
      e.preventDefault();
      const next = deep(grid);
      next[sel.r][sel.c] = blank;
      apply(next);
    }
  };

  // Toolbar Copy: the selected cell, or the whole grid as TSV if none selected.
  const copySelection = async () => {
    try {
      const text = sel
        ? display(grid[sel.r]?.[sel.c] ?? '')
        : grid.map((r) => r.map(display).join('\t')).join('\n');
      await navigator.clipboard.writeText(text);
    } catch {
      /* clipboard blocked */
    }
  };
  const pasteClipboard = async () => {
    if (!canEdit || !sel) return;
    try {
      pasteAt(sel, parseClipboard(await navigator.clipboard.readText()));
    } catch {
      /* clipboard blocked */
    }
  };

  // Create Variable from selection. Uses the engine's own indexing so every type
  // works: a selected cell of a matrix or string array becomes `new = name(r,c)`;
  // otherwise the whole variable is copied (`new = name`), e.g. for
  // struct/cell/table where a grid cell is not a plain sub-index.
  const createVariable = async () => {
    const target = await webPrompt('New variable name:', `${name}_sel`);
    if (!target || !meta) return;
    const indexable = meta.kind === 'matrix' || meta.kind === 'string';
    const expr = sel && indexable ? `${name}(${sel.r + 1}, ${sel.c + 1})` : name;
    void runCommand(`${target} = ${expr};`);
  };

  if (error) return <div className="var-editor__error">{error}</div>;
  if (!meta) return <div className="browser__empty">loading…</div>;

  const menuItem = (label: string, fn: () => void) => (
    <div
      className="console-menu__item"
      onMouseDown={(e) => {
        e.preventDefault();
        fn();
        setCtxMenu(null);
      }}
    >
      {label}
    </div>
  );

  return (
    <div
      className="var-editor"
      onKeyDown={onGridKeyDown}
      tabIndex={0}
      onClick={() => setCtxMenu(null)}
    >
      <div className="var-editor__bar">
        <b>{meta.name}</b>{' '}
        <span className="browser__mono">
          {rows}×{cols} {meta.className}
        </span>
        <span className="var-editor__spacer" />
        <button className="var-editor__btn" title="Refresh" onClick={() => load()}>
          <DesktopIcon name="refresh" size={18} />
        </button>
        <button className="var-editor__btn" title="Copy (Ctrl+C)" onClick={copySelection}>
          <DesktopIcon name="copy" size={18} />
        </button>
        <button
          className="var-editor__btn"
          title="Create variable from selection"
          onClick={createVariable}
        >
          <DesktopIcon name="add" size={18} />
        </button>
        {canEdit && (
          <>
            <button className="var-editor__btn" title="Paste (Ctrl+V)" onClick={pasteClipboard}>
              <DesktopIcon name="paste" size={18} />
            </button>
            <button className="var-editor__btn" title="Undo (Ctrl+Z)" onClick={undo}>
              <DesktopIcon name="undo" size={18} />
            </button>
            <button className="var-editor__btn" title="Redo (Ctrl+Y)" onClick={redo}>
              <DesktopIcon name="redo" size={18} />
            </button>
          </>
        )}
      </div>
      <div className="var-editor__scroll">
        {meta.kind === 'text' ? (
          <pre className="var-editor__text">{meta.text ?? ''}</pre>
        ) : (
          <table className="grid">
            <thead>
              <tr>
                <th className="grid__corner"></th>
                {Array.from({ length: cols }, (_, c) => (
                  <th key={c}>{meta.columns[c] ?? c + 1}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {grid.map((row, r) => (
                <tr key={r}>
                  <th className="grid__rowhead">{r + 1}</th>
                  {row.map((val, c) => {
                    const isSel = sel?.r === r && sel?.c === c;
                    return (
                      <td
                        key={c}
                        className={
                          'grid__cell' +
                          (isSel ? ' grid__cell--sel' : '') +
                          (isText ? ' grid__cell--text' : '')
                        }
                        onClick={() => setSel({ r, c })}
                        onContextMenu={(e) => {
                          e.preventDefault();
                          setSel({ r, c });
                          if (canEdit) setCtxMenu({ x: e.clientX, y: e.clientY, r, c });
                        }}
                        onDoubleClick={() => {
                          if (meta.editable) {
                            setEditing({ r, c });
                            setDraft(val === '<missing>' ? '' : display(val));
                          } else if (nestedKind && isScalarSummary(String(val ?? ''))) {
                            setEditing({ r, c });
                            setDraft(stripDecoration(String(val ?? '')));
                          }
                        }}
                      >
                        {editing && editing.r === r && editing.c === c ? (
                          <input
                            autoFocus
                            className="grid__input"
                            value={draft}
                            onChange={(e) => setDraft(e.target.value)}
                            onBlur={commit}
                            onKeyDown={(e) => {
                              if (e.key === 'Enter') commit();
                              if (e.key === 'Escape') setEditing(null);
                            }}
                          />
                        ) : (
                          renderCell(val)
                        )}
                      </td>
                    );
                  })}
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>

      {ctxMenu && (
        <div
          className="console-menu"
          style={{
            left: Math.min(ctxMenu.x, window.innerWidth - 190),
            top: Math.min(ctxMenu.y, window.innerHeight - 230),
            position: 'fixed',
          }}
        >
          {menuItem('Insert row above', () => insertRow(ctxMenu.r))}
          {menuItem('Insert row below', () => insertRow(ctxMenu.r + 1))}
          {menuItem('Delete row', () => deleteRow(ctxMenu.r))}
          <div className="console-menu__sep" />
          {menuItem('Insert column left', () => insertCol(ctxMenu.c))}
          {menuItem('Insert column right', () => insertCol(ctxMenu.c + 1))}
          {menuItem('Delete column', () => deleteCol(ctxMenu.c))}
        </div>
      )}
    </div>
  );
}
//=============================================================================
