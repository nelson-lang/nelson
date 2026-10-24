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
import { rpc } from '../rpc/client';
import { openEditor } from '../desktop/desktop';
import type { FsEntry, FsListResult } from '../rpc/types';
import { DesktopIcon } from '../components/DesktopIcon';
//=============================================================================
// Start, stop, clear, and inspect per-function profiling data.
interface ProfRow {
  function: string;
  file: string;
  line: number;
  calls: number;
  total: number;
  perCall: number;
}
//=============================================================================
type SortKey = 'function' | 'calls' | 'total' | 'perCall';
export function ProfilerPanel(): JSX.Element {
  const [rows, setRows] = useState<ProfRow[]>([]);
  const [running, setRunning] = useState(false);
  const [sort, setSort] = useState<SortKey>('total');
  const [busy, setBusy] = useState(false);
  const [exportPath, setExportPath] = useState('');
  const [exportUrl, setExportUrl] = useState('');
  const [exportError, setExportError] = useState('');
  const [exportDialogOpen, setExportDialogOpen] = useState(false);
  const [exportDir, setExportDir] = useState('');
  const [exportEntries, setExportEntries] = useState<FsEntry[]>([]);
  const [exportName, setExportName] = useState('profile_result');
  const [exportDialogError, setExportDialogError] = useState('');
  const refreshGeneration = useRef(0);

  const refresh = useCallback(async () => {
    const generation = ++refreshGeneration.current;
    const r = await rpc<{ rows?: ProfRow[]; on?: boolean }>('profile.info');
    if (generation !== refreshGeneration.current) return;
    setRows(r.rows ?? []);
    // Commands typed in the console can change the profiler state.
    if (typeof r.on === 'boolean') setRunning(r.on);
  }, []);

  useEffect(() => {
    void refresh().catch(() => undefined);
  }, [refresh]);

  const control = async (action: 'on' | 'off' | 'clear') => {
    if (busy) return;
    setBusy(true);
    refreshGeneration.current += 1;
    try {
      const result = await rpc<{ on?: boolean }>('profile.control', { action });
      if (action === 'on' || action === 'clear') setRows([]);
      if (action === 'on' || action === 'clear') {
        setExportPath('');
        setExportUrl('');
        setExportError('');
      }
      if (typeof result.on === 'boolean') setRunning(result.on);
      if (action === 'off') await refresh();
    } catch (e) {
      setExportError(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(false);
    }
  };

  const loadExportDir = useCallback(async (path: string) => {
    const result = await rpc<FsListResult>('fs.list', { path });
    setExportDir(result.cwd);
    setExportEntries(result.entries.filter((entry) => entry.isDir));
  }, []);

  const openExportDialog = async () => {
    if (busy || running) return;
    setBusy(true);
    try {
      setExportError('');
      setExportDialogError('');
      setExportName('profile_result');
      await loadExportDir('');
      setExportDialogOpen(true);
    } catch (e) {
      setExportError(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(false);
    }
  };

  const parentDir = (path: string) =>
    path.replace(/[\\/][^\\/]+[\\/]?$/, '') || (path.startsWith('/') ? '/' : path);
  const joinPath = (dir: string, name: string) =>
    dir.endsWith('/') || dir.endsWith('\\') ? dir + name : `${dir}/${name}`;

  const exportProfile = async (destination: string) => {
    if (busy || running) return;
    setBusy(true);
    try {
      setExportError('');
      setExportDialogError('');
      const result = await rpc<{ path?: string; index?: string; url?: string; on?: boolean }>(
        'profile.export',
        { path: destination }
      );
      if (typeof result.on === 'boolean') setRunning(result.on);
      setExportPath(result.index ?? result.path ?? '');
      setExportUrl(result.url ?? '');
      setExportDialogOpen(false);
      if (result.url) window.open(result.url, '_blank', 'noopener');
    } catch (e) {
      const message = e instanceof Error ? e.message : String(e);
      setExportError(message);
      setExportDialogError(message);
    } finally {
      setBusy(false);
    }
  };

  const openRow = (r: ProfRow) => {
    if (r.file && r.file !== '') openEditor(r.file, r.line || 1);
  };

  const sorted = [...rows].sort((a, b) =>
    sort === 'function' ? a.function.localeCompare(b.function) : b[sort] - a[sort]
  );
  const fmt = (n: number) => (Number.isFinite(n) ? n.toFixed(n < 1 ? 4 : 3) : '0');
  const fileLabel = (file: string) => file.split(/[\\/]/).pop() || file;

  return (
    <div className="browser profiler">
      <div className="profiler__bar">
        <button
          className={'profiler__btn' + (running ? ' profiler__btn--on' : '')}
          disabled={busy}
          onClick={() => void control(running ? 'off' : 'on').catch(() => undefined)}
          title={running ? 'Stop profiling (profile off)' : 'Start profiling (profile on)'}
        >
          <DesktopIcon name={running ? 'stop' : 'execute'} size={16} />
          {running ? 'Stop' : 'Start'}
        </button>
        <button
          className="profiler__btn"
          disabled={busy}
          onClick={() => void control('clear').catch(() => undefined)}
          title="Clear data (profile clear)"
        >
          Clear
        </button>
        <button
          className="profiler__btn"
          disabled={busy || running}
          onClick={() => void openExportDialog()}
          title={running ? 'Stop profiling before exporting' : 'Export profiler report'}
        >
          Export
        </button>
        <button
          className="profiler__btn"
          disabled={busy}
          onClick={() => void refresh().catch(() => undefined)}
          title="Refresh report"
        >
          Refresh
        </button>
        {running ? (
          <span className="profiler__on">
            <span className="profiler__dot" aria-hidden="true" /> profiling…
          </span>
        ) : null}
        <span className="profiler__hint">
          {exportError ? (
            <span className="profiler__error">{exportError}</span>
          ) : exportPath ? (
            <>
              Exported:{' '}
              {exportUrl ? (
                <a href={exportUrl} target="_blank" rel="noreferrer">
                  {exportPath}
                </a>
              ) : (
                exportPath
              )}
            </>
          ) : (
            'Double-click a row to open the file'
          )}
        </span>
      </div>
      <div className="profiler__scroll">
        <table className="grid profiler__table">
          <thead>
            <tr>
              <th className="profiler__sortable" onClick={() => setSort('function')}>
                Function
              </th>
              <th>File</th>
              <th>Line</th>
              <th className="profiler__sortable" onClick={() => setSort('calls')}>
                Calls
              </th>
              <th className="profiler__sortable" onClick={() => setSort('total')}>
                Total (s)
              </th>
              <th className="profiler__sortable" onClick={() => setSort('perCall')}>
                Per call (s)
              </th>
            </tr>
          </thead>
          <tbody>
            {sorted.map((r, i) => (
              <tr
                key={i}
                className={r.file ? 'profiler__row--file' : ''}
                title={r.file ? `${r.file}:${r.line} (double-click to open)` : undefined}
                onDoubleClick={() => openRow(r)}
              >
                <td>{r.function}</td>
                <td title={r.file}>{fileLabel(r.file)}</td>
                <td>{r.line || ''}</td>
                <td>{r.calls}</td>
                <td>{fmt(r.total)}</td>
                <td>{fmt(r.perCall)}</td>
              </tr>
            ))}
            {sorted.length === 0 ? (
              <tr>
                <td colSpan={6} className="profiler__empty">
                  No profile data. Click Start, run some code, then Stop.
                </td>
              </tr>
            ) : null}
          </tbody>
        </table>
      </div>
      {exportDialogOpen ? (
        <div className="dialog-overlay">
          <div className="dialog-card profiler__export-dialog">
            <div className="dialog-card__title">Export Profiler Report</div>
            <div className="dialog-card__body">
              <div className="dialog-file__path" title={exportDir}>
                <button
                  className="dialog-btn"
                  title="Parent folder"
                  onClick={() =>
                    void loadExportDir(parentDir(exportDir)).catch((e) =>
                      setExportDialogError(e instanceof Error ? e.message : String(e))
                    )
                  }
                >
                  <DesktopIcon name="up" />
                </button>
                <span className="dialog-file__cwd">{exportDir}</span>
              </div>
              <div className="dialog-list dialog-file__list" role="listbox">
                {exportEntries.map((entry) => (
                  <div
                    key={entry.path}
                    role="option"
                    aria-selected={false}
                    className="dialog-list__item"
                    onDoubleClick={() =>
                      void loadExportDir(entry.path).catch((e) =>
                        setExportDialogError(e instanceof Error ? e.message : String(e))
                      )
                    }
                  >
                    <DesktopIcon name="folder" size={16} /> {entry.name}
                  </div>
                ))}
                {exportEntries.length === 0 ? (
                  <div className="dialog-list__item dialog-list__item--empty">No folders</div>
                ) : null}
              </div>
              <label className="dialog-field">
                <span className="dialog-field__label">Report folder</span>
                <input
                  type="text"
                  value={exportName}
                  onChange={(e) => setExportName(e.target.value)}
                />
              </label>
              {exportDialogError ? (
                <span className="profiler__error">{exportDialogError}</span>
              ) : null}
            </div>
            <div className="dialog-card__buttons">
              <button className="dialog-btn" onClick={() => setExportDialogOpen(false)}>
                Cancel
              </button>
              <button
                className="dialog-btn dialog-btn--default"
                disabled={busy || exportName.trim().length === 0}
                onClick={() => void exportProfile(joinPath(exportDir, exportName.trim()))}
              >
                Export
              </button>
            </div>
          </div>
        </div>
      ) : null}
    </div>
  );
}
//=============================================================================
