//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Qt-free file picker rendered in HTML. It browses the filesystem through an
// injected `listDir` callback (an RPC in the host). The web_gui desktop and the
// NFlow editor (nflow_gui) share it, so it uses inline styles only: there is no
// CSS to wire across the two Vite builds.
import { useEffect, useRef, useState } from 'react';
//=============================================================================
export type FilePickerMode = 'open' | 'save' | 'dir';
export interface FilePickerFsEntry {
  name: string;
  path: string;
  isDir: boolean;
  size: number;
}
//=============================================================================
export interface FilePickerListResult {
  cwd: string;
  entries: FilePickerFsEntry[];
}
//=============================================================================
export interface FilePickerDialogProps {
  mode: FilePickerMode;
  title?: string;
  /** Directory the browser opens on. Defaults to '.' (host current directory). */
  defaultPath?: string;
  /** Initial file name for `save` mode. */
  suggestedName?: string;
  /** Accepted extensions (e.g. ['.nflow']); empty or ['*'] shows every file. */
  filters?: string[];
  /** Lists a directory. The host wires this to its own filesystem RPC. */
  listDir: (path: string) => Promise<FilePickerListResult>;
  /** Called with the chosen absolute path (never called on cancel). */
  onSubmit: (path: string) => void;
  onCancel: () => void;
}
//=============================================================================
const S = {
  overlay: {
    position: 'fixed',
    inset: 0,
    zIndex: 100000,
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    background: 'rgba(0, 0, 0, 0.45)',
  } as React.CSSProperties,
  card: {
    display: 'flex',
    flexDirection: 'column',
    width: 'min(560px, 92vw)',
    maxHeight: '80vh',
    background: '#ffffff',
    color: '#1a1a1a',
    borderRadius: '8px',
    boxShadow: '0 10px 40px rgba(0, 0, 0, 0.35)',
    overflow: 'hidden',
    fontFamily: 'system-ui, -apple-system, Segoe UI, Roboto, sans-serif',
    fontSize: '13px',
  } as React.CSSProperties,
  title: {
    padding: '12px 16px',
    fontWeight: 600,
    fontSize: '14px',
    borderBottom: '1px solid #e3e3e3',
  } as React.CSSProperties,
  body: {
    display: 'flex',
    flexDirection: 'column',
    gap: '8px',
    padding: '12px 16px',
    minHeight: 0,
    flex: 1,
  } as React.CSSProperties,
  pathRow: {
    display: 'flex',
    alignItems: 'center',
    gap: '8px',
  } as React.CSSProperties,
  upBtn: {
    flex: '0 0 auto',
    padding: '4px 10px',
    border: '1px solid #cfcfcf',
    borderRadius: '4px',
    background: '#f5f5f5',
    cursor: 'pointer',
  } as React.CSSProperties,
  cwd: {
    flex: 1,
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap',
    fontFamily: 'ui-monospace, SFMono-Regular, Consolas, monospace',
    color: '#555',
    direction: 'rtl',
    textAlign: 'left',
  } as React.CSSProperties,
  list: {
    flex: 1,
    minHeight: '160px',
    overflowY: 'auto',
    border: '1px solid #e0e0e0',
    borderRadius: '4px',
    background: '#fafafa',
  } as React.CSSProperties,
  item: {
    padding: '5px 10px',
    cursor: 'pointer',
    userSelect: 'none',
    whiteSpace: 'nowrap',
    overflow: 'hidden',
    textOverflow: 'ellipsis',
  } as React.CSSProperties,
  itemSel: {
    background: '#2563eb',
    color: '#ffffff',
  } as React.CSSProperties,
  nameInput: {
    padding: '6px 10px',
    border: '1px solid #cfcfcf',
    borderRadius: '4px',
    font: 'inherit',
  } as React.CSSProperties,
  buttons: {
    display: 'flex',
    justifyContent: 'flex-end',
    gap: '8px',
    padding: '12px 16px',
    borderTop: '1px solid #e3e3e3',
  } as React.CSSProperties,
  btn: {
    padding: '6px 16px',
    border: '1px solid #cfcfcf',
    borderRadius: '4px',
    background: '#f5f5f5',
    cursor: 'pointer',
    font: 'inherit',
  } as React.CSSProperties,
  btnDefault: {
    borderColor: '#2563eb',
    background: '#2563eb',
    color: '#ffffff',
  } as React.CSSProperties,
  btnDisabled: {
    opacity: 0.5,
    cursor: 'not-allowed',
  } as React.CSSProperties,
};
//=============================================================================
function stripTrailingSep(dir: string): string {
  return dir.replace(/[\\/]+$/, '');
}
//=============================================================================
function joinPath(dir: string, name: string): string {
  return stripTrailingSep(dir) + '/' + name;
}
//=============================================================================
// A file name typed as a full path is used as is, like the system dialogs do.
function isAbsolutePath(name: string): boolean {
  return /^([a-zA-Z]:[\\/]|[\\/])/.test(name);
}
//=============================================================================
function parentPath(dir: string): string {
  const stripped = stripTrailingSep(dir);
  const cut = stripped.replace(/[\\/][^\\/]*$/, '');
  return cut || stripped;
}
//=============================================================================
export function FilePickerDialog(props: FilePickerDialogProps): JSX.Element {
  const { mode, title, defaultPath, suggestedName, filters, listDir, onSubmit, onCancel } = props;
  const [cwd, setCwd] = useState(defaultPath || '.');
  const [entries, setEntries] = useState<FilePickerFsEntry[]>([]);
  const [selected, setSelected] = useState('');
  const [saveName, setSaveName] = useState(suggestedName || '');
  const cardRef = useRef<HTMLDivElement | null>(null);

  const matchesFilter = (name: string): boolean => {
    if (!filters || filters.length === 0 || filters.includes('*') || filters.includes('*.*')) {
      return true;
    }
    const lower = name.toLowerCase();
    return filters.some((f) => lower.endsWith(f.toLowerCase()));
  };

  const browse = (path: string): void => {
    setSelected('');
    void listDir(path)
      .then((r) => {
        setCwd(r.cwd || path);
        setEntries(Array.isArray(r.entries) ? r.entries : []);
      })
      .catch(() => setEntries([]));
  };

  useEffect(() => {
    browse(defaultPath || '.');
    // Browse once on mount; later navigation is driven by user actions.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  useEffect(() => {
    cardRef.current?.focus();
  }, []);

  const canSubmit = mode === 'dir' ? true : mode === 'save' ? !!saveName.trim() : !!selected;

  const submit = (): void => {
    if (mode === 'dir') {
      onSubmit(cwd);
    } else if (mode === 'save') {
      const name = saveName.trim();
      if (!name) return;
      onSubmit(isAbsolutePath(name) ? name : joinPath(cwd, name));
    } else {
      if (!selected) return;
      onSubmit(joinPath(cwd, selected));
    }
  };

  const onKeyDown = (e: React.KeyboardEvent): void => {
    if (e.key === 'Escape') {
      e.preventDefault();
      onCancel();
    } else if (e.key === 'Enter') {
      e.preventDefault();
      if (canSubmit) submit();
    }
  };

  // Files are hidden in `dir` mode.
  const visible = entries.filter((entry) => {
    if (entry.isDir) return true;
    if (mode === 'dir') return false;
    return matchesFilter(entry.name);
  });

  const submitLabel = mode === 'save' ? 'Save' : mode === 'dir' ? 'Select' : 'Open';

  return (
    <div style={S.overlay} role="presentation" onMouseDown={onCancel}>
      <div
        ref={cardRef}
        style={S.card}
        role="dialog"
        aria-modal="true"
        aria-label={title || 'Select a file'}
        tabIndex={-1}
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={onKeyDown}
      >
        <div style={S.title}>{title || 'Select a file'}</div>
        <div style={S.body}>
          <div style={S.pathRow}>
            <button
              type="button"
              style={S.upBtn}
              title="Parent folder"
              onClick={() => browse(parentPath(cwd))}
            >
              ↑
            </button>
            <span style={S.cwd} title={cwd}>
              {cwd}
            </span>
          </div>
          <div style={S.list} role="listbox">
            {visible.map((entry) => {
              const isSel = !entry.isDir && selected === entry.name;
              return (
                <div
                  key={entry.path}
                  role="option"
                  aria-selected={isSel}
                  style={isSel ? { ...S.item, ...S.itemSel } : S.item}
                  onClick={() => (entry.isDir ? undefined : setSelected(entry.name))}
                  onDoubleClick={() => {
                    if (entry.isDir) {
                      browse(entry.path);
                    } else {
                      setSelected(entry.name);
                      onSubmit(joinPath(cwd, entry.name));
                    }
                  }}
                >
                  {entry.isDir ? '\u{1F4C1} ' : '\u{1F4C4} '}
                  {entry.name}
                </div>
              );
            })}
          </div>
          {mode === 'save' ? (
            <input
              style={S.nameInput}
              type="text"
              placeholder="File name"
              value={saveName}
              autoFocus
              onChange={(e) => setSaveName(e.target.value)}
            />
          ) : null}
        </div>
        <div style={S.buttons}>
          <button type="button" style={S.btn} onClick={onCancel}>
            Cancel
          </button>
          <button
            type="button"
            style={canSubmit ? { ...S.btn, ...S.btnDefault } : { ...S.btn, ...S.btnDisabled }}
            disabled={!canSubmit}
            onClick={submit}
          >
            {submitLabel}
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
export default FilePickerDialog;
//=============================================================================
