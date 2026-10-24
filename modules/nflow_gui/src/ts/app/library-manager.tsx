//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useMemo, useState, type ReactNode } from 'react';
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import { chooseDirectoryViaDialog } from './file-dialogs.js';
import {
  addLibraryLocation,
  getLibraryServiceSnapshot,
  moveLibraryLocation,
  removeLibraryLocation,
  replaceLibraryLocation,
  refreshLibraryServiceSnapshot,
  rescanLibraries,
  selectLibraryBlocks,
  setLibraryEnabled,
  subscribeLibraryService,
  type LibraryServiceSnapshot,
  type ManagedLibrary,
} from './library-service.js';
import { t } from '../utils/i18n.js';
//=============================================================================
let panelEl: HTMLElement | null = null;
let panelRoot: Root | null = null;
let mountedPanelEl: HTMLElement | null = null;
type IconName = 'arrow-left' | 'folder-plus' | 'refresh' | 'up' | 'down' | 'locate' | 'trash';
function Icon({ name }: { name: IconName }): JSX.Element {
  const paths: Record<IconName, ReactNode> = {
    'arrow-left': (
      <>
        <path d="M19 12H5" />
        <path d="m12 19-7-7 7-7" />
      </>
    ),
    'folder-plus': (
      <>
        <path d="M12 10v6" />
        <path d="M9 13h6" />
        <path d="M3 6h5l2 2h11v11H3z" />
      </>
    ),
    refresh: (
      <>
        <path d="M20 11a8 8 0 1 0-2.3 5.7" />
        <path d="M20 4v7h-7" />
      </>
    ),
    up: (
      <>
        <path d="m18 15-6-6-6 6" />
      </>
    ),
    down: (
      <>
        <path d="m6 9 6 6 6-6" />
      </>
    ),
    locate: (
      <>
        <circle cx="12" cy="12" r="3" />
        <path d="M12 2v3M12 19v3M2 12h3M19 12h3" />
      </>
    ),
    trash: (
      <>
        <path d="M3 6h18" />
        <path d="M8 6V4h8v2M19 6l-1 14H6L5 6" />
      </>
    ),
  };
  return (
    <svg
      className="lib-manager-icon"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.8"
      aria-hidden="true"
    >
      {paths[name]}
    </svg>
  );
}
//=============================================================================
function IconButton({
  icon,
  label,
  disabled,
  onClick,
}: {
  icon: IconName;
  label: string;
  disabled?: boolean;
  onClick: () => void;
}): JSX.Element {
  return (
    <button
      type="button"
      className="lib-manager-icon-button"
      title={label}
      aria-label={label}
      disabled={disabled}
      onClick={onClick}
    >
      <Icon name={icon} />
    </button>
  );
}
//=============================================================================
function normalizedSearch(value: string): string {
  return value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLocaleLowerCase();
}
//=============================================================================
function matchesSearch(library: ManagedLibrary, query: string): boolean {
  if (!query) return true;
  return normalizedSearch(`${library.title} ${library.id} ${library.path}`).includes(query);
}
//=============================================================================
function pathContains(root: string, child: string): boolean {
  const base = root.replace(/[\\/]+$/, '').toLowerCase();
  const path = child.toLowerCase();
  return path.startsWith(`${base}\\`) || path.startsWith(`${base}/`);
}
//=============================================================================
function LibraryRow({
  library,
  snapshot,
  setError,
}: {
  library: ManagedLibrary;
  snapshot: LibraryServiceSnapshot;
  setError: (message: string) => void;
}): JSX.Element {
  const locationIndex = snapshot.locations.findIndex((item) =>
    pathContains(item.path, library.path)
  );
  const run = (task: Promise<void>) => {
    setError('');
    void task.catch((error) => setError(t(error instanceof Error ? error.message : String(error))));
  };
  return (
    <li className="lib-manager-library-row">
      <div className="lib-manager-library-main">
        <div className="lib-manager-library-copy">
          <strong>{t(library.title)}</strong>
          <span className="lib-manager-library-id">{library.id}</span>
        </div>
        {library.builtIn ? (
          <span className="lib-manager-badge">{t('Built-in')}</span>
        ) : (
          <label className="lib-manager-switch">
            <input
              type="checkbox"
              role="switch"
              aria-label={t('Enable {name}', { name: library.title })}
              checked={library.enabled}
              disabled={snapshot.busy}
              onChange={(event) => run(setLibraryEnabled(library, event.currentTarget.checked))}
            />
            <span aria-hidden="true" />
          </label>
        )}
      </div>
      <div className="lib-manager-library-meta">
        {library.version ? <span>v{library.version}</span> : null}
        <span>{t('{count} blocks', { count: library.blockCount })}</span>
        <span className={library.enabled ? 'is-enabled' : ''}>
          {t(library.enabled ? 'Enabled' : 'Disabled')}
        </span>
      </div>
      {library.path ? (
        <div className="lib-manager-path" title={library.path}>
          {library.path}
        </div>
      ) : null}
      {library.usedBlockIds.length > 0 ? (
        <button
          type="button"
          className="lib-manager-link"
          onClick={() => selectLibraryBlocks(library.usedBlockIds)}
        >
          {t('Used by {count} blocks', { count: library.usedBlockIds.length })} -{' '}
          {t('Select blocks')}
        </button>
      ) : null}
      {!library.builtIn && locationIndex >= 0 ? (
        <button
          type="button"
          className="lib-manager-text-action"
          disabled={snapshot.busy || library.usedBlockIds.length > 0}
          onClick={() => run(removeLibraryLocation(locationIndex))}
        >
          {t('Remove location')}
        </button>
      ) : null}
    </li>
  );
}
//=============================================================================
type RunTask = (task: Promise<void>) => void;
function LibraryLocationsSection({
  snapshot,
  run,
  chooseFolder,
}: {
  snapshot: LibraryServiceSnapshot;
  run: RunTask;
  chooseFolder: () => Promise<string>;
}): JSX.Element {
  return (
    <details className="lib-manager-section" open>
      <summary>
        {t('Library locations')} <span>{snapshot.locations.length}</span>
      </summary>
      <div className="lib-manager-section-actions">
        <button
          type="button"
          className="lib-manager-command"
          disabled={snapshot.busy}
          onClick={() =>
            run(chooseFolder().then((path) => (path ? addLibraryLocation(path) : undefined)))
          }
        >
          <Icon name="folder-plus" />
          {t('Add folder')}
        </button>
      </div>
      {snapshot.locations.length === 0 ? (
        <p className="lib-manager-empty">{t('No library locations configured.')}</p>
      ) : (
        <ol className="lib-manager-location-list">
          {snapshot.locations.map((location, index) => (
            <li key={location.path} className="lib-manager-location-row">
              <div className="lib-manager-location-copy">
                <span
                  className={
                    location.available
                      ? 'lib-manager-status-dot available'
                      : 'lib-manager-status-dot'
                  }
                  aria-hidden="true"
                />
                <span className="lib-manager-path" title={location.path}>
                  {location.path}
                </span>
              </div>
              <div className="lib-manager-location-meta">
                <span>{t(location.available ? 'Available' : 'Unavailable')}</span>
                <span>{t('{count} libraries', { count: location.libraryCount })}</span>
              </div>
              <div className="lib-manager-row-actions">
                <IconButton
                  icon="locate"
                  label={t('Locate')}
                  disabled={snapshot.busy}
                  onClick={() =>
                    run(
                      chooseFolder().then((path) =>
                        path ? replaceLibraryLocation(index, path) : Promise.resolve()
                      )
                    )
                  }
                />
                <IconButton
                  icon="up"
                  label={t('Move up')}
                  disabled={snapshot.busy || index === 0}
                  onClick={() => run(moveLibraryLocation(index, -1))}
                />
                <IconButton
                  icon="down"
                  label={t('Move down')}
                  disabled={snapshot.busy || index === snapshot.locations.length - 1}
                  onClick={() => run(moveLibraryLocation(index, 1))}
                />
                <IconButton
                  icon="refresh"
                  label={t('Rescan')}
                  disabled={snapshot.busy}
                  onClick={() => run(rescanLibraries())}
                />
                <IconButton
                  icon="trash"
                  label={t('Remove')}
                  disabled={snapshot.busy}
                  onClick={() => run(removeLibraryLocation(index))}
                />
              </div>
            </li>
          ))}
        </ol>
      )}
    </details>
  );
}
//=============================================================================
function LibraryGroup({
  title,
  libraries,
  snapshot,
  setError,
  emptyMessage,
  open,
}: {
  title: string;
  libraries: ManagedLibrary[];
  snapshot: LibraryServiceSnapshot;
  setError: (message: string) => void;
  emptyMessage?: string;
  open?: boolean;
}): JSX.Element {
  return (
    <details className="lib-manager-section" open={open}>
      <summary>
        {t(title)} <span>{libraries.length}</span>
      </summary>
      {libraries.length ? (
        <ul className="lib-manager-library-list">
          {libraries.map((library) => (
            <LibraryRow
              key={library.id}
              library={library}
              snapshot={snapshot}
              setError={setError}
            />
          ))}
        </ul>
      ) : emptyMessage ? (
        <p className="lib-manager-empty">{t(emptyMessage)}</p>
      ) : null}
    </details>
  );
}
//=============================================================================
function LibraryManagerPanel(): JSX.Element {
  const [snapshot, setSnapshot] = useState(getLibraryServiceSnapshot);
  const [query, setQuery] = useState('');
  const [error, setError] = useState('');
  useEffect(() => subscribeLibraryService(setSnapshot), []);
  const normalizedQuery = normalizedSearch(query.trim());
  const builtIns = useMemo(
    () => snapshot.libraries.filter((item) => item.builtIn && matchesSearch(item, normalizedQuery)),
    [snapshot, normalizedQuery]
  );
  const external = useMemo(
    () =>
      snapshot.libraries.filter((item) => !item.builtIn && matchesSearch(item, normalizedQuery)),
    [snapshot, normalizedQuery]
  );
  const run = useCallback((task: Promise<void>) => {
    setError('');
    void task.catch((value) => setError(t(value instanceof Error ? value.message : String(value))));
  }, []);
  const chooseFolder = useCallback(async () => {
    const response = await chooseDirectoryViaDialog();
    return response?.success && response.path ? String(response.path) : '';
  }, []);

  return (
    <section
      className="library-manager-view"
      aria-label={t('Library Manager')}
      aria-busy={snapshot.busy}
    >
      <div className="library-manager-toolbar">
        <button type="button" className="lib-manager-back" onClick={closeLibraryManager}>
          <Icon name="arrow-left" />
          {t('Back to blocks')}
        </button>
        <IconButton
          icon="refresh"
          label={t('Rescan all')}
          disabled={snapshot.busy}
          onClick={() => run(rescanLibraries())}
        />
      </div>
      <label className="lib-manager-search">
        <span className="sr-only">{t('Search libraries')}</span>
        <input
          type="search"
          value={query}
          placeholder={t('Search libraries')}
          disabled={snapshot.busy}
          onChange={(event) => setQuery(event.currentTarget.value)}
        />
      </label>
      {error ? (
        <div className="lib-manager-error" role="alert">
          {error}
        </div>
      ) : null}
      <div className="library-manager-scroll">
        <LibraryLocationsSection snapshot={snapshot} run={run} chooseFolder={chooseFolder} />
        <LibraryGroup
          title="Built-in libraries"
          libraries={builtIns}
          snapshot={snapshot}
          setError={setError}
        />
        <LibraryGroup
          title="External libraries"
          libraries={external}
          snapshot={snapshot}
          setError={setError}
          emptyMessage={
            normalizedQuery
              ? 'No libraries match your search.'
              : 'No external libraries discovered.'
          }
          open
        />
      </div>
    </section>
  );
}
//=============================================================================
export function mountLibraryManager(container: HTMLElement): void {
  panelEl = container;
  renderPanel();
}
//=============================================================================
function renderPanel(): void {
  if (!panelEl) return;
  if (!panelRoot || mountedPanelEl !== panelEl) {
    panelRoot?.unmount();
    panelEl.replaceChildren();
    panelRoot = createRoot(panelEl);
    mountedPanelEl = panelEl;
  }
  flushSync(() => panelRoot?.render(<LibraryManagerPanel />));
}
//=============================================================================
export function openLibraryManager(): void {
  const palette = document.getElementById('blockLibraryView');
  const manager = document.getElementById('libraryManager');
  if (!manager) return;
  palette?.setAttribute('hidden', '');
  manager.removeAttribute('hidden');
  renderPanel();
  void rescanLibraries().catch(() => undefined);
}
//=============================================================================
export function closeLibraryManager(): void {
  document.getElementById('blockLibraryView')?.removeAttribute('hidden');
  document.getElementById('libraryManager')?.setAttribute('hidden', '');
  document.getElementById('blockLibrarySearch')?.focus();
}
//=============================================================================
export function refreshPanel(): void {
  flushSync(() => refreshLibraryServiceSnapshot());
}
//=============================================================================
