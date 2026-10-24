//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import { rpc } from '../rpc/client';
import { runCommand } from '../desktop/engineSession';
import { DesktopIcon } from './DesktopIcon';
//=============================================================================
// Import Data dialog: pick a data file, preview it,
// choose how to read it (table / matrix / cell, or load a .mat), and a target
// variable name. The import runs through the readtable/readmatrix/readcell/load
// builtins. Opened from the File menu, optionally seeded with a path.
export const OPEN_IMPORT_EVENT = 'nelson-webview.open-import';
type ImportKind = 'table' | 'matrix' | 'cell' | 'load';
const baseName = (p: string) => (p.split(/[\\/]/).pop() ?? p).replace(/\.[^.]*$/, '');
const sanitize = (s: string) =>
  (s.replace(/[^A-Za-z0-9_]/g, '_') || 'data').replace(/^(\d)/, '_$1');
interface FsEntry {
  name: string;
  path: string;
  isDir: boolean;
}
//=============================================================================
export function ImportDataHost(): JSX.Element | null {
  const [open, setOpen] = useState(false);
  const [path, setPath] = useState('');
  const [name, setName] = useState('data');
  const [kind, setKind] = useState<ImportKind>('table');
  const [preview, setPreview] = useState<string>('');
  const [browseDir, setBrowseDir] = useState<string | null>(null);
  const [entries, setEntries] = useState<FsEntry[]>([]);

  const browse = (dir: string) => {
    rpc<{ cwd: string; entries: FsEntry[] }>('fs.list', { path: dir })
      .then((r) => {
        setBrowseDir(r.cwd);
        setEntries(r.entries);
      })
      .catch(() => undefined);
  };
  const pick = (e: FsEntry) => {
    if (e.isDir) {
      browse(e.path);
    } else {
      setPath(e.path);
      setName(sanitize(baseName(e.path)));
      setKind(/\.mat$/i.test(e.path) ? 'load' : 'table');
      setBrowseDir(null);
    }
  };

  useEffect(() => {
    const onOpen = (e: Event) => {
      const seed = (e as CustomEvent<{ path?: string }>).detail?.path ?? '';
      setOpen(true);
      setPath(seed);
      setName(seed ? sanitize(baseName(seed)) : 'data');
      setKind(/\.mat$/i.test(seed) ? 'load' : 'table');
      setPreview('');
    };
    window.addEventListener(OPEN_IMPORT_EVENT, onOpen as EventListener);
    return () => window.removeEventListener(OPEN_IMPORT_EVENT, onOpen as EventListener);
  }, []);

  // Preview the first lines of a text file (skip binary/.mat).
  useEffect(() => {
    if (!open || !path || /\.(mat|xlsx?|bin)$/i.test(path)) {
      setPreview('');
      return;
    }
    let alive = true;
    rpc<{ data?: string }>('fs.download', { path })
      .then((r) => {
        if (!alive || !r.data) return;
        try {
          const text = new TextDecoder().decode(
            Uint8Array.from(atob(r.data), (c) => c.charCodeAt(0))
          );
          setPreview(text.split('\n').slice(0, 15).join('\n'));
        } catch {
          setPreview('(binary file)');
        }
      })
      .catch(() => setPreview(''));
    return () => {
      alive = false;
    };
  }, [open, path]);

  if (!open) return null;

  const isMat = /\.mat$/i.test(path);
  const doImport = () => {
    const q = `'${path.replace(/'/g, "''")}'`;
    const v = sanitize(name);
    let line = '';
    if (kind === 'load') line = `load(${q})`;
    else if (kind === 'table') line = `${v} = readtable(${q})`;
    else if (kind === 'matrix') line = `${v} = readmatrix(${q})`;
    else line = `${v} = readcell(${q})`;
    void runCommand(line);
    setOpen(false);
  };

  return (
    <div className="dialog-overlay" role="presentation" onMouseDown={() => setOpen(false)}>
      <div
        className="dialog-card import-card"
        role="dialog"
        aria-modal="true"
        aria-label="Import Data"
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={(e) => {
          if (e.key === 'Escape') setOpen(false);
        }}
      >
        <div className="about-card__title">Import Data</div>
        <label className="import-row">
          <span>File</span>
          <input
            className="import-input"
            value={path}
            placeholder="path/to/data.csv"
            onChange={(e) => setPath(e.target.value)}
            spellCheck={false}
          />
          <button
            type="button"
            className="dialog-btn"
            onClick={() => browse(path ? path.replace(/[\\/][^\\/]*$/, '') : '')}
          >
            Browse…
          </button>
        </label>
        {browseDir !== null ? (
          <div className="import-browse">
            <div className="import-browse__path">
              <button
                className="files__up"
                title="Parent folder"
                onClick={() => browse(browseDir.replace(/[\\/][^\\/]+$/, '') || '/')}
              >
                <DesktopIcon name="up" />
              </button>
              <span className="browser__mono">{browseDir}</span>
            </div>
            <ul className="import-browse__list">
              {entries.map((e) => (
                <li key={e.path} onClick={() => pick(e)}>
                  <span className="file-list__icon">
                    <DesktopIcon name={e.isDir ? 'folder' : 'file'} size={16} />
                  </span>
                  {e.name}
                </li>
              ))}
            </ul>
          </div>
        ) : null}
        <label className="import-row">
          <span>Import as</span>
          <select value={kind} onChange={(e) => setKind(e.target.value as ImportKind)}>
            {isMat ? (
              <option value="load">Load .mat (all variables)</option>
            ) : (
              <>
                <option value="table">Table (readtable)</option>
                <option value="matrix">Matrix (readmatrix)</option>
                <option value="cell">Cell (readcell)</option>
              </>
            )}
          </select>
        </label>
        {kind !== 'load' ? (
          <label className="import-row">
            <span>Variable</span>
            <input
              className="import-input"
              value={name}
              onChange={(e) => setName(e.target.value)}
              spellCheck={false}
            />
          </label>
        ) : null}
        {preview ? <pre className="import-preview">{preview}</pre> : null}
        <div className="dialog-card__buttons">
          <button className="dialog-btn" onClick={() => setOpen(false)}>
            Cancel
          </button>
          <button className="dialog-btn dialog-btn--default" disabled={!path} onClick={doImport}>
            Import
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
