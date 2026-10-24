//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import { webPrompt, webConfirm } from '../components/PromptHost';
import type { FsEntry, FsListResult } from '../rpc/types';
import { openEditor } from '../desktop/desktop';
import { ContextMenu, useContextMenu, type MenuItem } from '../components/ContextMenu';
import { runCommand as run } from '../desktop/engineSession';
import {
  changeCurrentDirectory,
  refreshCurrentDirectory,
  useCurrentDirectory,
} from '../desktop/currentDirectory';
import { showStatusMessage } from '../desktop/statusMessage';
import { DesktopIcon } from '../components/DesktopIcon';
import { uploadLocalFiles } from '../desktop/localFiles';
//=============================================================================
const SCRIPT_TEMPLATE = '%% Nelson script\n';
const FUNCTION_TEMPLATE = 'function varargout = NAME(varargin)\n  % NAME summary\nend\n';
export function FilesPanel(): JSX.Element {
  const currentDirectory = useCurrentDirectory();
  const [cwd, setCwd] = useState('');
  const [entries, setEntries] = useState<FsEntry[]>([]);
  const [filter, setFilter] = useState('');
  const [showHidden, setShowHidden] = useState(false);
  const { menu, openMenu, closeMenu } = useContextMenu();
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [dragOver, setDragOver] = useState(false);

  const load = useCallback((path: string) => {
    rpc<FsListResult>('fs.list', { path })
      .then((r) => {
        setCwd(r.cwd);
        setEntries(r.entries);
      })
      .catch(() => undefined);
  }, []);

  useEffect(() => {
    void refreshCurrentDirectory().catch(() => undefined);
  }, []);

  useEffect(() => {
    if (currentDirectory.cwd) load(currentDirectory.cwd);
  }, [currentDirectory.cwd, load]);

  useEffect(() => {
    return onPush('fs.changed', () => {
      if (currentDirectory.cwd) load(currentDirectory.cwd);
    });
  }, [currentDirectory.cwd, load]);

  const changeFolder = (path: string) =>
    changeCurrentDirectory(path)
      .then((result) => load(result.cwd))
      .catch(() => showStatusMessage('Directory does not exist'));
  const parent = () => {
    const stripped = cwd.replace(/[\\/]+$/, '');
    const cut = stripped.replace(/[\\/][^\\/]*$/, '');
    void changeFolder(cut || (cwd.startsWith('/') ? '/' : stripped));
  };
  const activate = (entry: FsEntry) =>
    entry.isDir ? void changeFolder(entry.path) : openEditor(entry.path);
  const join = (name: string) =>
    /[\\/]$/.test(cwd) ? cwd + name : cwd + (currentDirectory.separator || '/') + name;

  // Name filter: glob-ish patterns separated by ; , or space (e.g. "*.m;*.nh5").
  const visible = useMemo(() => {
    const list = showHidden ? entries : entries.filter((e) => !e.name.startsWith('.'));
    const patterns = filter
      .split(/[;,\s]+/)
      .map((p) => p.trim())
      .filter(Boolean);
    if (patterns.length === 0) return list;
    const res = patterns.map(
      (p) =>
        new RegExp(
          '^' +
            p
              .replace(/[.+^${}()|[\]\\]/g, '\\$&')
              .replace(/\*/g, '.*')
              .replace(/\?/g, '.') +
            '$',
          'i'
        )
    );
    return list.filter((e) => e.isDir || res.some((r) => r.test(e.name)));
  }, [entries, filter, showHidden]);

  const refresh = () => load(cwd);

  const createEntry = async (kind: 'file' | 'dir', template?: (name: string) => string) => {
    const label = kind === 'dir' ? 'New folder name:' : 'New file name:';
    const name = await webPrompt(label, kind === 'dir' ? 'untitled' : 'untitled.m');
    if (!name) return;
    const path = join(name);
    const content = template ? template(name.replace(/\.[^.]*$/, '')) : '';
    await rpc('fs.create', { path, kind, content }).catch(() => undefined);
    refresh();
    if (kind === 'file') openEditor(path);
  };

  const renameEntry = async (entry: FsEntry) => {
    const name = await webPrompt('Rename to:', entry.name);
    if (!name || name === entry.name) return;
    await rpc('fs.rename', { from: entry.path, to: join(name) }).catch(() => undefined);
    refresh();
  };

  const deleteEntry = async (entry: FsEntry) => {
    if (
      !(await webConfirm(`Delete "${entry.name}"? This cannot be undone.`, {
        okLabel: 'Delete',
        danger: true,
      }))
    )
      return;
    await rpc('fs.delete', { path: entry.path }).catch(() => undefined);
    refresh();
  };

  const uploadFiles = async (files: FileList | File[]) => {
    await uploadLocalFiles(files, cwd).catch((error) =>
      showStatusMessage((error as Error).message || 'Upload failed')
    );
    refresh();
  };

  const downloadEntry = async (entry: FsEntry) => {
    const r = await rpc<{ name?: string; data?: string }>('fs.download', {
      path: entry.path,
    }).catch(() => null);
    if (!r || !r.data) return;
    const bytes = Uint8Array.from(atob(r.data), (c) => c.charCodeAt(0));
    const url = URL.createObjectURL(new Blob([bytes]));
    const a = document.createElement('a');
    a.href = url;
    a.download = r.name ?? entry.name;
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  };

  const backgroundMenu = (e: React.MouseEvent) =>
    openMenu(e, [
      { label: 'New Script', onClick: () => void createEntry('file', () => SCRIPT_TEMPLATE) },
      {
        label: 'New Function',
        onClick: () =>
          void createEntry('file', (n) => FUNCTION_TEMPLATE.replace(/NAME/g, n || 'untitled')),
      },
      { label: 'New Folder', onClick: () => void createEntry('dir') },
      { separator: true },
      {
        label: showHidden ? 'Hide hidden files' : 'Show hidden files',
        onClick: () => setShowHidden((v) => !v),
      },
      { label: 'Refresh', onClick: refresh },
    ]);

  const itemsFor = (entry: FsEntry): MenuItem[] => {
    const isM = /\.m$/i.test(entry.name);
    const items: MenuItem[] = [];
    if (entry.isDir) {
      items.push({ label: 'Open', onClick: () => void changeFolder(entry.path) });
      items.push({ label: 'Set as Current Folder', onClick: () => void changeFolder(entry.path) });
      items.push({ label: 'Add to Path', onClick: () => run(`addpath('${entry.path}')`) });
      items.push({ label: 'Remove from Path', onClick: () => run(`rmpath('${entry.path}')`) });
    } else {
      items.push({ label: 'Edit', onClick: () => openEditor(entry.path) });
      if (isM) items.push({ label: 'Run', onClick: () => run(`run('${entry.path}')`) });
      items.push({ label: 'Download', onClick: () => void downloadEntry(entry) });
      if (/\.(csv|txt|dat|xlsx?|mat)$/i.test(entry.name)) {
        items.push({
          label: 'Import Data…',
          onClick: () =>
            window.dispatchEvent(
              new CustomEvent('nelson-webview.open-import', { detail: { path: entry.path } })
            ),
        });
      }
    }
    items.push({ separator: true });
    items.push({ label: 'Rename…', onClick: () => void renameEntry(entry) });
    items.push({ label: 'Delete', danger: true, onClick: () => void deleteEntry(entry) });
    items.push({ separator: true });
    items.push({
      label: 'Copy full path',
      onClick: () => void navigator.clipboard?.writeText(entry.path).catch(() => undefined),
    });
    items.push({ label: 'Refresh', onClick: refresh });
    return items;
  };

  return (
    <div className="browser">
      <div className="files__path">
        <button className="files__up" onClick={parent} title="Parent folder">
          <DesktopIcon name="up" />
        </button>
        <span className="browser__mono">{cwd}</span>
        <span style={{ flex: 1 }} />
        <button
          className="files__up"
          onClick={() => fileInputRef.current?.click()}
          title="Upload files to this folder"
        >
          <DesktopIcon name="upload" />
        </button>
        <input
          ref={fileInputRef}
          type="file"
          multiple
          style={{ display: 'none' }}
          onChange={(e) => {
            if (e.target.files) void uploadFiles(e.target.files);
            e.target.value = '';
          }}
        />
      </div>
      <input
        className="files__filter"
        placeholder="Filter (*.m;*.nh5)"
        value={filter}
        onChange={(e) => setFilter(e.target.value)}
        spellCheck={false}
      />
      <ul
        className={'file-list' + (dragOver ? ' file-list--drop' : '')}
        onContextMenu={backgroundMenu}
        onDragOver={(e) => {
          e.preventDefault();
          setDragOver(true);
        }}
        onDragLeave={() => setDragOver(false)}
        onDrop={(e) => {
          e.preventDefault();
          e.stopPropagation();
          setDragOver(false);
          if (e.dataTransfer.files.length) void uploadFiles(e.dataTransfer.files);
        }}
      >
        {visible.map((entry) => (
          <li
            key={entry.path}
            onDoubleClick={() => activate(entry)}
            onContextMenu={(e) => openMenu(e, itemsFor(entry))}
          >
            <span className="file-list__icon">
              <DesktopIcon name={entry.isDir ? 'folder' : 'file'} size={16} />
            </span>
            {entry.name}
          </li>
        ))}
      </ul>
      <ContextMenu menu={menu} onClose={closeMenu} />
    </div>
  );
}
//=============================================================================
